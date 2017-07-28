/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_ROOTS_H
#define CAML_ROOTS_H

#include "misc.h"
#include "memory.h"

typedef void (*scanning_action) (value, value *);

void caml_oldify_local_roots (void);
void caml_darken_all_roots (void);
void caml_do_roots (scanning_action);
#ifndef NATIVE_CODE
CAMLextern void caml_do_local_roots (scanning_action, value *, value *,
                                     struct caml__roots_block *);
#else
CAMLextern void caml_do_local_roots(scanning_action f, char * bottom_of_stack,
                                    uintnat last_retaddr, value * gc_regs,
                                    struct caml__roots_block * local_roots);
#endif

CAMLextern void (*caml_scan_roots_hook) (scanning_action);

#endif /* CAML_ROOTS_H */
