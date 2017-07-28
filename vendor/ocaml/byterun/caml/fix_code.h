/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Handling of blocks of bytecode (endianness switch, threading). */

#ifndef CAML_FIX_CODE_H
#define CAML_FIX_CODE_H


#include "config.h"
#include "misc.h"
#include "mlvalues.h"

extern code_t caml_start_code;
extern asize_t caml_code_size;
extern unsigned char * caml_saved_code;

void caml_init_code_fragments();
void caml_load_code (int fd, asize_t len);
void caml_fixup_endianness (code_t code, asize_t len);
void caml_set_instruction (code_t pos, opcode_t instr);
int caml_is_instruction (opcode_t instr1, opcode_t instr2);

#ifdef THREADED_CODE
extern char ** caml_instr_table;
extern char * caml_instr_base;
void caml_thread_code (code_t code, asize_t len);
#endif

#endif /* CAML_FIX_CODE_H */
