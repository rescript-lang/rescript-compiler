/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  Damien Doligez, Jane Street Group, LLC                */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_STARTUP_AUX_H
#define CAML_STARTUP_AUX_H

#ifdef CAML_INTERNALS

#include "config.h"

extern void caml_init_atom_table (void);

extern uintnat caml_init_percent_free;
extern uintnat caml_init_max_percent_free;
extern uintnat caml_init_minor_heap_wsz;
extern uintnat caml_init_heap_chunk_sz;
extern uintnat caml_init_heap_wsz;
extern uintnat caml_init_max_stack_wsz;
extern uintnat caml_init_major_window;
extern uintnat caml_trace_level;
extern uintnat caml_cleanup_on_exit;

extern void caml_parse_ocamlrunparam (void);

/* Common entry point to caml_startup.
   Returns 0 if the runtime is already initialized.
   If [pooling] is 0, [caml_stat_*] functions will not be backed by a pool. */
extern int caml_startup_aux (int pooling);

#endif /* CAML_INTERNALS */

#endif /* CAML_STARTUP_AUX_H */
