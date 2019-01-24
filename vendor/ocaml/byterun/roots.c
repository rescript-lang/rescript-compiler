/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* To walk the memory roots for garbage collection */

#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/stacks.h"

CAMLexport struct caml__roots_block *caml_local_roots = NULL;

CAMLexport void (*caml_scan_roots_hook) (scanning_action f) = NULL;

/* FIXME should rename to [caml_oldify_minor_roots] and synchronise with
   asmrun/roots.c */
/* Call [caml_oldify_one] on (at least) all the roots that point to the minor
   heap. */
void caml_oldify_local_roots (void)
{
  register value * sp;
  struct caml__roots_block *lr;
  intnat i, j;

  /* The stack */
  for (sp = caml_extern_sp; sp < caml_stack_high; sp++) {
    caml_oldify_one (*sp, sp);
  }
  /* Local C roots */  /* FIXME do the old-frame trick ? */
  for (lr = caml_local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        caml_oldify_one (*sp, sp);
      }
    }
  }
  /* Global C roots */
  caml_scan_global_young_roots(&caml_oldify_one);
  /* Finalised values */
  caml_final_oldify_young_roots ();
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(&caml_oldify_one);
}

/* Call [caml_darken] on all roots */

void caml_darken_all_roots_start (void)
{
  caml_do_roots (caml_darken, 1);
}

uintnat caml_incremental_roots_count = 1;

intnat caml_darken_all_roots_slice (intnat work)
{
  return work;
}

/* Note, in byte-code there is only one global root, so [do_globals] is
   ignored and [caml_darken_all_roots_slice] does nothing. */
void caml_do_roots (scanning_action f, int do_globals)
{
  CAML_INSTR_SETUP (tmr, "major_roots");
  /* Global variables */
  f(caml_global_data, &caml_global_data);
  CAML_INSTR_TIME (tmr, "major_roots/global");
  /* The stack and the local C roots */
  caml_do_local_roots(f, caml_extern_sp, caml_stack_high, caml_local_roots);
  CAML_INSTR_TIME (tmr, "major_roots/local");
  /* Global C roots */
  caml_scan_global_roots(f);
  CAML_INSTR_TIME (tmr, "major_roots/C");
  /* Finalised values */
  caml_final_do_roots (f);
  CAML_INSTR_TIME (tmr, "major_roots/finalised");
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f);
  CAML_INSTR_TIME (tmr, "major_roots/hook");
}

CAMLexport void caml_do_local_roots (scanning_action f, value *stack_low,
                                     value *stack_high,
                                     struct caml__roots_block *local_roots)
{
  register value * sp;
  struct caml__roots_block *lr;
  int i, j;

  for (sp = stack_low; sp < stack_high; sp++) {
    f (*sp, sp);
  }
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        f (*sp, sp);
      }
    }
  }
}
