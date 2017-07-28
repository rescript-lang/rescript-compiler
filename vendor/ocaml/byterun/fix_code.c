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

#include "caml/config.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/debugger.h"
#include "caml/fix_code.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/reverse.h"

code_t caml_start_code;
asize_t caml_code_size;
unsigned char * caml_saved_code;

/* Read the main bytecode block from a file */

void caml_init_code_fragments() {
  struct code_fragment * cf;
  /* Register the code in the table of code fragments */
  cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = (char *) caml_start_code;
  cf->code_end = (char *) caml_start_code + caml_code_size;
  caml_md5_block(cf->digest, caml_start_code, caml_code_size);
  cf->digest_computed = 1;
  caml_ext_table_init(&caml_code_fragments_table, 8);
  caml_ext_table_add(&caml_code_fragments_table, cf);
}

void caml_load_code(int fd, asize_t len)
{
  int i;

  caml_code_size = len;
  caml_start_code = (code_t) caml_stat_alloc(caml_code_size);
  if (read(fd, (char *) caml_start_code, caml_code_size) != caml_code_size)
    caml_fatal_error("Fatal error: truncated bytecode file.\n");
  caml_init_code_fragments();
  /* Prepare the code for execution */
#ifdef ARCH_BIG_ENDIAN
  caml_fixup_endianness(caml_start_code, caml_code_size);
#endif
  if (caml_debugger_in_use) {
    len /= sizeof(opcode_t);
    caml_saved_code = (unsigned char *) caml_stat_alloc(len);
    for (i = 0; i < len; i++) caml_saved_code[i] = caml_start_code[i];
  }
#ifdef THREADED_CODE
  /* Better to thread now than at the beginning of [caml_interprete],
     since the debugger interface needs to perform SET_EVENT requests
     on the code. */
  caml_thread_code(caml_start_code, caml_code_size);
#endif
}

/* This code is needed only if the processor is big endian */

#ifdef ARCH_BIG_ENDIAN

void caml_fixup_endianness(code_t code, asize_t len)
{
  code_t p;
  len /= sizeof(opcode_t);
  for (p = code; p < code + len; p++) {
    Reverse_32(p, p);
  }
}

#endif

/* This code is needed only if we're using threaded code */

#ifdef THREADED_CODE

char ** caml_instr_table;
char * caml_instr_base;

static int* opcode_nargs = NULL;
int* caml_init_opcode_nargs()
{
  if( opcode_nargs == NULL ){
    int* l = (int*)caml_stat_alloc(sizeof(int) * FIRST_UNIMPLEMENTED_OP);
    int i;

    for (i = 0; i < FIRST_UNIMPLEMENTED_OP; i++) {
      l [i] = 0;
    }
    /* Instructions with one operand */
    l[PUSHACC] = l[ACC] = l[POP] = l[ASSIGN] =
      l[PUSHENVACC] = l[ENVACC] = l[PUSH_RETADDR] = l[APPLY] =
      l[APPTERM1] = l[APPTERM2] = l[APPTERM3] = l[RETURN] =
      l[GRAB] = l[PUSHGETGLOBAL] = l[GETGLOBAL] = l[SETGLOBAL] =
      l[PUSHATOM] = l[ATOM] = l[MAKEBLOCK1] = l[MAKEBLOCK2] =
      l[MAKEBLOCK3] = l[MAKEFLOATBLOCK] = l[GETFIELD] =
      l[GETFLOATFIELD] = l[SETFIELD] = l[SETFLOATFIELD] =
      l[BRANCH] = l[BRANCHIF] = l[BRANCHIFNOT] = l[PUSHTRAP] =
      l[C_CALL1] = l[C_CALL2] = l[C_CALL3] = l[C_CALL4] = l[C_CALL5] =
      l[CONSTINT] = l[PUSHCONSTINT] = l[OFFSETINT] =
      l[OFFSETREF] = l[OFFSETCLOSURE] = l[PUSHOFFSETCLOSURE] = 1;
    
    /* Instructions with two operands */
    l[APPTERM] = l[CLOSURE] = l[PUSHGETGLOBALFIELD] =
      l[GETGLOBALFIELD] = l[MAKEBLOCK] = l[C_CALLN] =
      l[BEQ] = l[BNEQ] = l[BLTINT] = l[BLEINT] = l[BGTINT] = l[BGEINT] =
      l[BULTINT] = l[BUGEINT] = l[GETPUBMET] = 2;

    opcode_nargs = l;
  }
  return opcode_nargs;
}

void caml_thread_code (code_t code, asize_t len)
{
  code_t p;
  int* l = caml_init_opcode_nargs();
  len /= sizeof(opcode_t);
  for (p = code; p < code + len; /*nothing*/) {
    opcode_t instr = *p;
    if (instr < 0 || instr >= FIRST_UNIMPLEMENTED_OP){
      /* FIXME -- should Assert(false) ?
      caml_fatal_error_arg ("Fatal error in fix_code: bad opcode (%lx)\n",
                            (char *)(long)instr);
      */
      instr = STOP;
    }
    *p++ = (opcode_t)(caml_instr_table[instr] - caml_instr_base);
    if (instr == SWITCH) {
      uint32 sizes = *p++;
      uint32 const_size = sizes & 0xFFFF;
      uint32 block_size = sizes >> 16;
      p += const_size + block_size;
    } else if (instr == CLOSUREREC) {
      uint32 nfuncs = *p++;
      p++;                      /* skip nvars */
      p += nfuncs;
    } else {
      p += l[instr];
    }
  }
  Assert(p == code + len);
}

#else

int* caml_init_opcode_nargs()
{
  return NULL;
}

#endif /* THREADED_CODE */

void caml_set_instruction(code_t pos, opcode_t instr)
{
#ifdef THREADED_CODE
  *pos = (opcode_t)(caml_instr_table[instr] - caml_instr_base);
#else
  *pos = instr;
#endif
}

int caml_is_instruction(opcode_t instr1, opcode_t instr2)
{
#ifdef THREADED_CODE
  return instr1 == (opcode_t)(caml_instr_table[instr2] - caml_instr_base);
#else
  return instr1 == instr2;
#endif
}
