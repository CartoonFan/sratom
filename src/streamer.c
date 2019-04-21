/*
  Copyright 2012-2019 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#include "sratom/sratom.h"

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/midi/midi.h"
#include "lv2/urid/urid.h"
#include "serd/serd.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NS_RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
#define NS_XSD "http://www.w3.org/2001/XMLSchema#"

#define STREAM_ERROR(msg) \
	serd_world_logf(streamer->world, SERD_LOG_LEVEL_ERR, NULL, 0, msg);

#define STREAM_WARN(msg) \
	serd_world_logf(     \
	        streamer->world, "sratom", SERD_LOG_LEVEL_WARNING, NULL, 0, msg);

#define STREAM_ERRORF(msg, ...)         \
	serd_world_logf(streamer->world,    \
	                "sratom",           \
	                SERD_LOG_LEVEL_ERR, \
	                NULL,               \
	                0,                  \
	                msg,                \
	                __VA_ARGS__);

struct SratomStreamerImpl
{
	LV2_URID_Map*   map;
	LV2_URID_Unmap* unmap;
	LV2_Atom_Forge  forge;
	SerdWorld*      world;
	LV2_URID        atom_Event;
	LV2_URID        atom_beatTime;
	LV2_URID        midi_MidiEvent;
	struct
	{
		const SerdNode* atom_Path;
		const SerdNode* atom_beatTime;
		const SerdNode* atom_childType;
		const SerdNode* atom_frameTime;
		const SerdNode* midi_MidiEvent;
		const SerdNode* rdf_first;
		const SerdNode* rdf_nil;
		const SerdNode* rdf_rest;
		const SerdNode* rdf_type;
		const SerdNode* rdf_value;
		const SerdNode* xsd_base64Binary;
		const SerdNode* xsd_boolean;
		const SerdNode* xsd_decimal;
		const SerdNode* xsd_double;
		const SerdNode* xsd_float;
		const SerdNode* xsd_int;
		const SerdNode* xsd_integer;
		const SerdNode* xsd_long;
	} nodes;
};

typedef struct
{
	SratomStreamer*    streamer;
	const SerdSink*    sink;
	const SratomFlags  flags;
	SerdStatementFlags sflags;
	LV2_URID           seq_unit;
} StreamContext;

void
sratom_free(void* ptr)
{
	/* The only sratom memory the users frees comes from sratom_from_model(),
	   which is allocated by serd_buffer_sink. */
	serd_free(ptr);
}

SratomStreamer*
sratom_streamer_new(SerdWorld* world, LV2_URID_Map* map, LV2_URID_Unmap* unmap)
{
	SratomStreamer* streamer =
	        (SratomStreamer*)calloc(1, sizeof(SratomStreamer));
	if (!streamer) {
		return NULL;
	}

	streamer->world          = world;
	streamer->map            = map;
	streamer->unmap          = unmap;
	streamer->atom_Event     = map->map(map->handle, LV2_ATOM__Event);
	streamer->atom_beatTime  = map->map(map->handle, LV2_ATOM__beatTime);
	streamer->midi_MidiEvent = map->map(map->handle, LV2_MIDI__MidiEvent);
	lv2_atom_forge_init(&streamer->forge, map);

#define MANAGE_URI(uri)                                                        \
	serd_nodes_manage(serd_world_get_nodes(world), serd_new_uri(uri))

	streamer->nodes.atom_Path        = MANAGE_URI(LV2_ATOM__Path);
	streamer->nodes.atom_beatTime    = MANAGE_URI(LV2_ATOM__beatTime);
	streamer->nodes.atom_childType   = MANAGE_URI(LV2_ATOM__childType);
	streamer->nodes.atom_frameTime   = MANAGE_URI(LV2_ATOM__frameTime);
	streamer->nodes.midi_MidiEvent   = MANAGE_URI(LV2_MIDI__MidiEvent);
	streamer->nodes.rdf_first        = MANAGE_URI(NS_RDF "first");
	streamer->nodes.rdf_nil          = MANAGE_URI(NS_RDF "nil");
	streamer->nodes.rdf_rest         = MANAGE_URI(NS_RDF "rest");
	streamer->nodes.rdf_type         = MANAGE_URI(NS_RDF "type");
	streamer->nodes.rdf_value        = MANAGE_URI(NS_RDF "value");
	streamer->nodes.xsd_base64Binary = MANAGE_URI(NS_XSD "base64Binary");
	streamer->nodes.xsd_boolean      = MANAGE_URI(NS_XSD "boolean");
	streamer->nodes.xsd_decimal      = MANAGE_URI(NS_XSD "decimal");
	streamer->nodes.xsd_double       = MANAGE_URI(NS_XSD "double");
	streamer->nodes.xsd_float        = MANAGE_URI(NS_XSD "float");
	streamer->nodes.xsd_int          = MANAGE_URI(NS_XSD "int");
	streamer->nodes.xsd_integer      = MANAGE_URI(NS_XSD "integer");
	streamer->nodes.xsd_long         = MANAGE_URI(NS_XSD "long");

#undef MANAGE_URI

	return streamer;
}

void
sratom_streamer_free(SratomStreamer* streamer)
{
	free(streamer);
}

static int
write_atom(StreamContext*  ctx,
           const SerdNode* subject,
           const SerdNode* predicate,
           LV2_URID        type,
           uint32_t        size,
           const void*     body);

static void
list_append(StreamContext*   ctx,
            SerdNode**       s,
            const SerdNode** p,
            uint32_t         size,
            uint32_t         type,
            const void*      body)
{
	// Generate a list node
	SerdNode* node = serd_node_copy(serd_world_get_blank(ctx->streamer->world));
	serd_sink_write(ctx->sink, ctx->sflags, *s, *p, node, NULL);

	// _:node rdf:first value
	ctx->sflags = 0;
	*p          = ctx->streamer->nodes.rdf_first;
	write_atom(ctx, node, *p, type, size, body);

	// Set subject to node and predicate to rdf:rest for next time
	serd_node_free(*s);
	*s = node;
	*p = ctx->streamer->nodes.rdf_rest;
}

static void
list_end(StreamContext* ctx, const SerdNode* s, const SerdNode* p)
{
	// _:node rdf:rest rdf:nil
	serd_sink_write(
	        ctx->sink, ctx->sflags, s, p, ctx->streamer->nodes.rdf_nil, NULL);
}

static void
start_object(StreamContext*  ctx,
             const SerdNode* subject,
             const SerdNode* predicate,
             const SerdNode* node,
             const char*     type)
{
	if (subject && predicate) {
		serd_sink_write(ctx->sink,
		                ctx->sflags | SERD_ANON_O,
		                subject,
		                predicate,
		                node,
		                NULL);
	} else {
		ctx->sflags |= SERD_EMPTY_S;
	}

	if (type) {
		SerdNode* o = serd_new_uri(type);
		serd_sink_write(ctx->sink,
		                ctx->sflags,
		                node,
		                ctx->streamer->nodes.rdf_type,
		                o,
		                NULL);
		serd_node_free(o);
	}
}

static void
end_object(StreamContext*  ctx,
             const SerdNode* subject,
             const SerdNode* predicate,
             const SerdNode* node)
{
	if (subject && predicate) {
		serd_sink_write_end(ctx->sink, node);
	}
}

static bool
path_is_absolute(const char* path)
{
	return (path[0] == '/' || (isalpha(path[0]) && path[1] == ':' &&
	                           (path[2] == '/' || path[2] == '\\')));
}

static const SerdNode*
number_type(StreamContext* ctx, const SerdNode* type)
{
	SratomStreamer* const streamer = ctx->streamer;
	const bool            pretty   = (ctx->flags & SRATOM_PRETTY_NUMBERS);
	if (pretty) {
		if (type == streamer->nodes.xsd_int ||
		    type == streamer->nodes.xsd_long) {
			return streamer->nodes.xsd_integer;
		} else if (type == streamer->nodes.xsd_float ||
		           type == streamer->nodes.xsd_double) {
			return streamer->nodes.xsd_decimal;
		}
	}
	return type;
}

static bool
is_primitive_type(StreamContext* ctx, const LV2_URID type)
{
	SratomStreamer* const streamer = ctx->streamer;
	return (!type ||
	        type == streamer->forge.Bool ||
	        type == streamer->forge.Double ||
	        type == streamer->forge.Float ||
	        type == streamer->forge.Int ||
	        type == streamer->forge.Literal ||
	        type == streamer->forge.Long ||
	        type == streamer->forge.Path ||
	        type == streamer->forge.String ||
	        type == streamer->forge.URI ||
	        type == streamer->forge.URID);
}

static int
write_atom(StreamContext*  ctx,
           const SerdNode* subject,
           const SerdNode* predicate,
           LV2_URID        type,
           uint32_t        size,
           const void*     body)
{
	SratomStreamer*   streamer = ctx->streamer;
	LV2_URID_Unmap*   unmap    = streamer->unmap;
	const SerdSink*   sink     = ctx->sink;
	const SerdEnv*    env      = serd_sink_get_env(ctx->sink);
	const char* const type_uri = unmap->unmap(unmap->handle, type);
	SerdNode*         object   = NULL;
	if (type == 0 && size == 0) {
		object = serd_node_copy(streamer->nodes.rdf_nil);
	} else if (type == streamer->forge.String) {
		object = serd_new_string((const char*)body);
	} else if (type == streamer->forge.Chunk) {
		object = serd_new_blob(body, size, true, NULL);
	} else if (type == streamer->forge.Literal) {
		const LV2_Atom_Literal_Body* lit = (const LV2_Atom_Literal_Body*)body;
		const char*                  str = (const char*)(lit + 1);
		if (lit->datatype) {
			SerdNode* datatype =
			        serd_new_uri(unmap->unmap(unmap->handle, lit->datatype));
			object = serd_new_typed_literal(str, datatype);
			serd_node_free(datatype);
		} else if (lit->lang) {
			const char*  lang       = unmap->unmap(unmap->handle, lit->lang);
			const char*  prefix     = "http://lexvo.org/id/iso639-3/";
			const size_t prefix_len = strlen(prefix);
			if (lang && !strncmp(lang, prefix, prefix_len)) {
				object = serd_new_plain_literal(str, lang + prefix_len);
			} else {
				STREAM_ERRORF("Unknown language URID %d\n", lit->lang);
			}
		}
	} else if (type == streamer->forge.URID) {
		const uint32_t urid = *(const uint32_t*)body;
		const char*    str  = unmap->unmap(unmap->handle, urid);
		object              = serd_new_uri(str);
	} else if (type == streamer->forge.Path) {
		const char* str = (const char*)body;
		if (path_is_absolute(str)) {
			object = serd_new_file_uri(str, NULL);
		} else {
			const SerdNode* base_uri = serd_env_get_base_uri(env);
			if (!base_uri ||
			    strncmp(serd_node_get_string(base_uri), "file://", 7)) {
				STREAM_WARN("Relative path but base is not a file URI.\n");
				STREAM_WARN("Writing ambiguous atom:Path literal.\n");
				object = serd_new_typed_literal(str, streamer->nodes.atom_Path);
			} else {
				SerdNode* rel = serd_new_file_uri(str, NULL);
				object        = serd_node_resolve(rel, base_uri);
				serd_node_free(rel);
			}
		}
	} else if (type == streamer->forge.URI) {
		object = serd_new_uri((const char*)body);
	} else if (type == streamer->forge.Int) {
		object = serd_new_integer(*(const int32_t*)body,
		                          number_type(ctx, streamer->nodes.xsd_int));
	} else if (type == streamer->forge.Long) {
		object = serd_new_integer(*(const int64_t*)body,
		                          number_type(ctx, streamer->nodes.xsd_long));
	} else if (type == streamer->forge.Float) {
		object = serd_new_decimal(*(const float*)body,
		                          9,
		                          0,
		                          number_type(ctx, streamer->nodes.xsd_float));
	} else if (type == streamer->forge.Double) {
		object = serd_new_decimal(*(const double*)body,
		                          17,
		                          0,
		                          number_type(ctx, streamer->nodes.xsd_double));
	} else if (type == streamer->forge.Bool) {
		const int32_t val = *(const int32_t*)body;
		object            = serd_new_boolean(val);
	} else if (type == streamer->midi_MidiEvent) {
		char* str = (char*)calloc(size * 2 + 1, 1);
		for (uint32_t i = 0; i < size; ++i) {
			snprintf(str + (2 * i),
			         size * 2 + 1,
			         "%02X",
			         (unsigned)*((const uint8_t*)body + i));
		}
		object = serd_new_typed_literal(str, streamer->nodes.midi_MidiEvent);
		free(str);
	} else if (type == streamer->atom_Event) {
		const LV2_Atom_Event* ev = (const LV2_Atom_Event*)body;
		const SerdNode*       id = serd_world_get_blank(streamer->world);
		start_object(ctx, subject, predicate, id, NULL);
		SerdNode*       time = NULL;
		const SerdNode* p    = NULL;
		if (ctx->seq_unit == streamer->atom_beatTime) {
			p    = streamer->nodes.atom_beatTime;
			time = serd_new_decimal(
			        ev->time.beats,
			        17,
			        0,
			        number_type(ctx, streamer->nodes.xsd_double));
		} else {
			p    = streamer->nodes.atom_frameTime;
			time = serd_new_integer(ev->time.frames,
			                        number_type(ctx, streamer->nodes.xsd_long));
		}
		serd_sink_write(sink, 0, id, p, time, NULL);
		serd_node_free(time);

		p = streamer->nodes.rdf_value;
		write_atom(ctx,
		           id,
		           p,
		           ev->body.type,
		           ev->body.size,
		           LV2_ATOM_BODY_CONST(&ev->body));
		end_object(ctx, subject, predicate, id);
	} else if (type == streamer->forge.Tuple) {
		SerdNode* id = serd_node_copy(serd_world_get_blank(streamer->world));
		start_object(ctx, subject, predicate, id, type_uri);
		const SerdNode* p = streamer->nodes.rdf_value;
		ctx->sflags |= SERD_LIST_O | SERD_TERSE_O;
		LV2_ATOM_TUPLE_BODY_FOREACH(body, size, i) {
			if (!is_primitive_type(ctx, i->type)) {
				ctx->sflags &= ~SERD_TERSE_O;
			}
		}
		LV2_ATOM_TUPLE_BODY_FOREACH(body, size, i)
		{
			list_append(ctx, &id, &p, i->size, i->type, LV2_ATOM_BODY(i));
		}
		list_end(ctx, id, p);
		end_object(ctx, subject, predicate, id);
		serd_node_free(id);
	} else if (type == streamer->forge.Vector) {
		const LV2_Atom_Vector_Body* vec = (const LV2_Atom_Vector_Body*)body;
		SerdNode* id = serd_node_copy(serd_world_get_blank(streamer->world));
		start_object(ctx, subject, predicate, id, type_uri);
		const SerdNode* p = streamer->nodes.atom_childType;
		SerdNode*       child_type =
		        serd_new_uri(unmap->unmap(unmap->handle, vec->child_type));
		serd_sink_write(sink, ctx->sflags, id, p, child_type, NULL);
		p = streamer->nodes.rdf_value;
		serd_node_free(child_type);
		ctx->sflags |= SERD_LIST_O;
		if (is_primitive_type(ctx, vec->child_type)) {
			ctx->sflags |= SERD_TERSE_O;
		}
		for (const char* i = (const char*)(vec + 1);
		     i < (const char*)vec + size;
		     i += vec->child_size) {
			list_append(ctx, &id, &p, vec->child_size, vec->child_type, i);
		}
		list_end(ctx, id, p);
		end_object(ctx, subject, predicate, id);
		serd_node_free(id);
	} else if (lv2_atom_forge_is_object_type(&streamer->forge, type)) {
		const LV2_Atom_Object_Body* obj = (const LV2_Atom_Object_Body*)body;
		const char* otype = unmap->unmap(unmap->handle, obj->otype);

		SerdNode* id = NULL;
		if (lv2_atom_forge_is_blank(&streamer->forge, type, obj)) {
			id = serd_node_copy(serd_world_get_blank(streamer->world));
			start_object(ctx, subject, predicate, id, otype);
		} else {
			id          = serd_new_uri(unmap->unmap(unmap->handle, obj->id));
			ctx->sflags = 0;
			start_object(ctx, NULL, NULL, id, otype);
		}
		LV2_ATOM_OBJECT_BODY_FOREACH(obj, size, prop)
		{
			const char* const key  = unmap->unmap(unmap->handle, prop->key);
			SerdNode*         pred = serd_new_uri(key);
			write_atom(ctx,
			           id,
			           pred,
			           prop->value.type,
			           prop->value.size,
			           LV2_ATOM_BODY(&prop->value));
			serd_node_free(pred);
		}
		end_object(ctx, subject, predicate, id);
		serd_node_free(id);
	} else if (type == streamer->forge.Sequence) {
		const LV2_Atom_Sequence_Body* seq = (const LV2_Atom_Sequence_Body*)body;
		SerdNode* id = serd_node_copy(serd_world_get_blank(streamer->world));
		start_object(ctx, subject, predicate, id, type_uri);
		const SerdNode* p = streamer->nodes.rdf_value;
		ctx->sflags |= SERD_LIST_O;
		LV2_ATOM_SEQUENCE_BODY_FOREACH(seq, size, ev)
		{
			ctx->seq_unit = seq->unit;
			list_append(ctx,
			            &id,
			            &p,
			            sizeof(LV2_Atom_Event) + ev->body.size,
			            streamer->atom_Event,
			            ev);
		}
		list_end(ctx, id, p);
		end_object(ctx, subject, predicate, id);
		serd_node_free(id);
	} else {
		const SerdNode* id = serd_world_get_blank(streamer->world);
		start_object(ctx, subject, predicate, id, type_uri);
		const SerdNode* p = streamer->nodes.rdf_value;
		SerdNode*       o = serd_new_blob(body, size, true, NULL);
		serd_sink_write(sink, ctx->sflags, id, p, o, NULL);
		end_object(ctx, subject, predicate, id);
		serd_node_free(o);
	}

	if (object) {
		if (!subject && !predicate) {
			subject   = serd_world_get_blank(streamer->world);
			predicate = streamer->nodes.rdf_first;
			ctx->sflags |= SERD_EMPTY_S;//SERD_TERSE_S | SERD_LIST_S | SERD_LIST_O;
			serd_sink_write(sink,
			                ctx->sflags | SERD_LIST_S | SERD_TERSE_S,
			                subject,
			                predicate,
			                object,
			                NULL);
			serd_sink_write(sink,
			                SERD_TERSE_S,
			                subject,
			                streamer->nodes.rdf_rest,
			                streamer->nodes.rdf_nil,
			                NULL);
			/* serd_sink_write_end(sink, subject); */
		} else {
			serd_sink_write(
			        sink, ctx->sflags, subject, predicate, object, NULL);
		}
	}

	serd_node_free(object);

	return 0;
}

int
sratom_write(SratomStreamer*   streamer,
             const SerdSink*   sink,
             const SerdNode*   subject,
             const SerdNode*   predicate,
             LV2_URID          type,
             uint32_t          size,
             const void*       body,
             const SratomFlags flags)
{
	StreamContext ctx = { streamer,
		                  sink,
		                  flags,
	                      (flags & SRATOM_NAMED_SUBJECT) ? 0 : SERD_EMPTY_S,
		                  0 };

	return write_atom(&ctx, subject, predicate, type, size, body);
}

SRATOM_API
int
sratom_stream(SratomStreamer* streamer,
              const SerdSink* sink,
              const SerdNode* subject,
              const SerdNode* predicate,
              const LV2_Atom* atom,
              SratomFlags     flags)
{
	return sratom_write(streamer,
	                    sink,
	                    subject,
	                    predicate,
	                    atom->type,
	                    atom->size,
	                    atom + 1,
	                    flags);
}

char*
sratom_to_string(SratomStreamer* const streamer,
                 SerdEnv* const        env,
                 const LV2_Atom* const atom,
                 const SratomFlags     flags)
{
	SerdBuffer  str = { NULL, 0 };
	SerdWriter* writer =
	        serd_writer_new(streamer->world,
	                        SERD_TURTLE,
	                        flags & SRATOM_TERSE ? SERD_WRITE_TERSE : 0,
	                        env,
	                        (SerdWriteFunc)serd_buffer_sink,
	                        &str);

	const SerdSink* sink = serd_writer_get_sink(writer);
	sratom_stream(streamer, sink, NULL, NULL, atom, flags);
	serd_writer_finish(writer);

	serd_writer_free(writer);
	return serd_buffer_sink_finish(&str);
}
