/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Trace the instructions executed */

#ifndef _instrtrace_
#define _instrtrace_

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "misc.h"

extern intnat caml_icount;
void caml_stop_here (void);
void caml_disasm_instr (code_t pc);
void caml_trace_value_file (value v, code_t prog, int proglen, FILE * f);
void caml_trace_accu_sp_file(value accu, value * sp, code_t prog, int proglen,
                             FILE * f);

#endif /* CAML_INTERNALS */

#endif
