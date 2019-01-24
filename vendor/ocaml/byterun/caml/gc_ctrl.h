/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_GC_CTRL_H
#define CAML_GC_CTRL_H

#ifdef CAML_INTERNALS

#include "misc.h"

extern double
     caml_stat_minor_words,
     caml_stat_promoted_words,
     caml_stat_major_words;

extern intnat
     caml_stat_minor_collections,
     caml_stat_major_collections,
     caml_stat_heap_wsz,
     caml_stat_top_heap_wsz,
     caml_stat_compactions,
     caml_stat_heap_chunks;

uintnat caml_normalize_heap_increment (uintnat);

/*
  minor_size: cf. minor_heap_size in gc.mli
  major_size: Size in words of the initial major heap
  major_incr: cf. major_heap_increment in gc.mli
  percent_fr: cf. space_overhead in gc.mli
  percent_m : cf. max_overhead in gc.mli
  window    : cf. window_size in gc.mli
*/
void caml_init_gc (uintnat minor_size, uintnat major_size, uintnat major_incr,
                   uintnat percent_fr, uintnat percent_m, uintnat window);


CAMLextern value caml_gc_stat(value v);

#ifdef DEBUG
void caml_heap_check (void);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_GC_CTRL_H */
