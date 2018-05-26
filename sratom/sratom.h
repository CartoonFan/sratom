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

/**
   @file sratom.h API for Sratom, a library for serialising LV2 atoms.
*/

#ifndef SRATOM_SRATOM_H
#define SRATOM_SRATOM_H

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/urid/urid.h"
#include "serd/serd.h"

#include <stdint.h>

#ifdef SRATOM_SHARED
#    ifdef _WIN32
#        define SRATOM_LIB_IMPORT __declspec(dllimport)
#        define SRATOM_LIB_EXPORT __declspec(dllexport)
#    else
#        define SRATOM_LIB_IMPORT __attribute__((visibility("default")))
#        define SRATOM_LIB_EXPORT __attribute__((visibility("default")))
#    endif
#    ifdef SRATOM_INTERNAL
#        define SRATOM_API SRATOM_LIB_EXPORT
#    else
#        define SRATOM_API SRATOM_LIB_IMPORT
#    endif
#else
#    define SRATOM_API
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
   @defgroup sratom Sratom
   An LV2 Atom RDF serialisation library.
   @{
*/

/// Flags to control how aroms are written and read
typedef enum {
	/**
	   Write the main subject with a label.

	   If set, the main subject will be written using its blank node label,
	   instead of as an anonymous node.
	*/
	SRATOM_NAMED_SUBJECT = 1 << 1,

	/**
	   Write pretty numeric literals.

	   If set, numbers may be written as pretty literals instead of string
	   literals with explicit data types.  Note that enabling this means that
	   types will not survive a round trip.
	*/
	SRATOM_PRETTY_NUMBERS = 1 << 2,

	/**
	   Write terse output without newlines.

	   If set, top level atoms will be written as a single long line.
	*/
	SRATOM_TERSE = 1 << 3,
} SratomFlag;

/// Bitwise OR of SratomFlag values
typedef uint32_t SratomFlags;

/**
   @name Streaming
   Serialising and writing atoms to a stream.
   @{
*/

/// Atom serialiser which streams to a sink
typedef struct SratomStreamerImpl SratomStreamer;

/**
   Create a new streamer for writing atoms to a sink.

   @param world RDF world
   @param map URID mapper
   @param unmap URID unmapper
*/
SRATOM_API
SratomStreamer*
sratom_streamer_new(SerdWorld* world, LV2_URID_Map* map, LV2_URID_Unmap* unmap);

/// Free an atom streamer created with `sratom_streamer_new()`
SRATOM_API
void
sratom_streamer_free(SratomStreamer* streamer);

/**
   Serialise an atom body to a sink.

   This is the fundamental writing function which writes an atom to a sink.  It
   can be used with any sink, such as a Turtle writer, model inserter, or a
   custom sink provided by the application.

   This function takes the `type`, `size`, and `body` separately to allow
   writing atoms from data in memory that does not have an atom header.  For
   true atom pointers, the simpler sratom_stream() can be used.

   Since all statements must be triples, a subject and predicate can be
   provided to serialise literals like `subject predicate "literal"`.  If
   `subject` and `predicate` are null, resources will be written as the
   subject, but literals will bewritten as the only element of an anonymous
   list.

   @param streamer Streamer instance
   @param sink Sink which receives the serialised statements
   @param subject Subject of first statement, or NULL
   @param predicate Predicate of first statement, or NULL
   @param type Type of the atom
   @param size Size of the atom body in bytes
   @param body Atom body
   @param flags Option flags
   @return Zero on success
*/
SRATOM_API
int
sratom_write(SratomStreamer* streamer,
             const SerdSink* sink,
             const SerdNode* subject,
             const SerdNode* predicate,
             LV2_URID        type,
             uint32_t        size,
             const void*     body,
             SratomFlags     flags);

/**
   Serialise an atom to a sink.

   Convenience wrapper that takes a pointer to a complete atom, see the
   sratom_write() documentation for details.

   @param streamer Streamer instance
   @param sink Sink which receives the serialised statements
   @param subject Subject of first statement, or NULL
   @param predicate Predicate of first statement, or NULL
   @param atom Atom to serialise
   @param flags Option flags
   @return Zero on success
*/
SRATOM_API
int
sratom_stream(SratomStreamer* streamer,
              const SerdSink* sink,
              const SerdNode* subject,
              const SerdNode* predicate,
              const LV2_Atom* atom,
              SratomFlags     flags);

/**
   Serialise an atom to a string.

   The returned string can be forged back into an atom using
   sratom_from_string().

   @param streamer Streamer instance
   @param env Environment for namespaces and relative URIs
   @param atom Atom to serialise
   @param flags Option flags
   @return A string that must be freed using sratom_free(), or NULL on error.
*/
SRATOM_API
char*
sratom_to_string(SratomStreamer* streamer,
                 SerdEnv*        env,
                 const LV2_Atom* atom,
                 SratomFlags     flags);

/**
   @}
   @name Forging
   Forging atoms from a serialised document.
   @{
*/

/// Atom forger which reads from a document
typedef struct SratomForgerImpl SratomForger;

/**
   Create a new forger for forging atoms from a document.

   @param world RDF world
   @param map URID mapper
*/
SRATOM_API
SratomForger*
sratom_forger_new(SerdWorld* world, LV2_URID_Map* map);

/// Free an atom forger created with `sratom_forger_new()`
SRATOM_API
void
sratom_forger_free(SratomForger* forger);

/**
   Forge an atom from a model.

   This is the fundamental reading function which builds an atom from a model.
   The atom is constructed by using the provided forge, which can, for example,
   be configured to write to a file descriptor or a buffer in memory.  For
   simpler use cases, see sratom_from_string() and sratom_from_model().

   File URIs will be made relative to the base URI if possible, then forged as
   paths, so it is important to use an appropriate base URI.  For example, to
   read an atom from `/dir/manifest.ttl` which refers to `/dir/file.txt`, and
   produce the path `file.txt`, `base_uri` should be set to `file:///dir/`.

   @param forger Forger instance
   @param base_uri Base URI for creating paths from file URIs.
   @param forge Atom forge where output will be written
   @param model Model which contains a description of `node`
   @param node Node to convert to an atom
   @param flags Option flags
   @return Zero on success
*/
SRATOM_API
int
sratom_forge(SratomForger*    forger,
             const SerdNode*  base_uri,
             LV2_Atom_Forge*  forge,
             const SerdModel* model,
             const SerdNode*  node,
             SratomFlags      flags);

/**
   Forge an atom from a string produced by sratom_to_string().

   @param forger Forger instance
   @param env Environment for namespaces and relative URIs
   @param str Serialised atom from sratom_to_string()
   @param flags Option flags
   @return An atom which must be freed using sratom_free(), or NULL on error.
*/
SRATOM_API
LV2_Atom*
sratom_from_string(SratomForger* forger,
                   SerdEnv*      env,
                   const char*   str,
                   SratomFlags   flags);

/**
   Read an atom from a model.

   @param forger Forger instance
   @param base_uri Base URI for creating paths from file URIs.
   @param model Model which contains `node` and related statements
   @param subject Node to convert to an atom
   @param flags Option flags
   @return An atom which must be freed using sratom_free(), or NULL on error.
*/
SRATOM_API
LV2_Atom*
sratom_from_model(SratomForger*    forger,
                  const SerdNode*  base_uri,
                  const SerdModel* model,
                  const SerdNode*  subject,
                  SratomFlags      flags);

/**
   @}
*/

/**
   Free memory allocated by Sratom.

   This function exists because some systems require memory allocated by a
   library to be freed by code in the same library.  It is otherwise equivalent
   to the standard C free() function.
*/
SRATOM_API
void
sratom_free(void* ptr);

/**
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* SRATOM_SRATOM_H */
