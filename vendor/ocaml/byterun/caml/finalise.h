/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_FINALISE_H
#define CAML_FINALISE_H

#ifdef CAML_INTERNALS

#include "roots.h"

void caml_final_update_mark_phase (void);
void caml_final_update_clean_phase (void);
void caml_final_do_calls (void);
void caml_final_do_roots (scanning_action f);
void caml_final_invert_finalisable_values ();
void caml_final_oldify_young_roots ();
void caml_final_empty_young (void);
void caml_final_update_minor_roots(void);
value caml_final_register (value f, value v);
void caml_final_invariant_check(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_FINALISE_H */
