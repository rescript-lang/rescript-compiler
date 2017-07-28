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

/* Trace the instructions executed */

#ifdef DEBUG

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "caml/instruct.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/opnames.h"
#include "caml/prims.h"
#include "caml/stacks.h"

extern code_t caml_start_code;

intnat caml_icount = 0;

void caml_stop_here () {}

int caml_trace_flag = 0;

void caml_disasm_instr(pc)
     code_t pc;
{
  int instr = *pc;
  printf("%6ld  %s", (long) (pc - caml_start_code),
         instr < 0 || instr > STOP ? "???" : names_of_instructions[instr]);
  pc++;
  switch(instr) {
    /* Instructions with one integer operand */
  case PUSHACC: case ACC: case POP: case ASSIGN:
  case PUSHENVACC: case ENVACC: case PUSH_RETADDR: case APPLY:
  case APPTERM1: case APPTERM2: case APPTERM3: case RETURN:
  case GRAB: case PUSHGETGLOBAL: case GETGLOBAL: case SETGLOBAL:
  case PUSHATOM: case ATOM: case MAKEBLOCK1: case MAKEBLOCK2:
  case MAKEBLOCK3: case MAKEFLOATBLOCK:
  case GETFIELD: case SETFIELD: case GETFLOATFIELD: case SETFLOATFIELD:
  case BRANCH: case BRANCHIF: case BRANCHIFNOT: case PUSHTRAP:
  case CONSTINT: case PUSHCONSTINT: case OFFSETINT: case OFFSETREF:
  case OFFSETCLOSURE: case PUSHOFFSETCLOSURE:
    printf(" %d\n", pc[0]); break;
    /* Instructions with two operands */
  case APPTERM: case CLOSURE: case CLOSUREREC: case PUSHGETGLOBALFIELD:
  case GETGLOBALFIELD: case MAKEBLOCK:
  case BEQ: case BNEQ: case BLTINT: case BLEINT: case BGTINT: case BGEINT:
  case BULTINT: case BUGEINT:
    printf(" %d, %d\n", pc[0], pc[1]); break;
    /* Instructions with a C primitive as operand */
  case C_CALLN:
    printf(" %d,", pc[0]); pc++;
    /* fallthrough */
  case C_CALL1: case C_CALL2: case C_CALL3: case C_CALL4: case C_CALL5:
    if (pc[0] < 0 || pc[0] >= caml_prim_name_table.size)
      printf(" unknown primitive %d\n", pc[0]);
    else
      printf(" %s\n", (char *) caml_prim_name_table.contents[pc[0]]);
    break;
  default:
    printf("\n");
  }
  fflush (stdout);
}

char * caml_instr_string (code_t pc)
{
  static char buf[256];
  char nambuf[128];
  int instr = *pc;
  char *nam;

  nam = (instr < 0 || instr > STOP)
    ? (snprintf (nambuf, sizeof(nambuf), "???%d", instr), nambuf)
    : names_of_instructions[instr];
  pc++;
  switch (instr) {
    /* Instructions with one integer operand */
  case PUSHACC:
  case ACC:
  case POP:
  case ASSIGN:
  case PUSHENVACC:
  case ENVACC:
  case PUSH_RETADDR:
  case APPLY:
  case APPTERM1:
  case APPTERM2:
  case APPTERM3:
  case RETURN:
  case GRAB:
  case PUSHGETGLOBAL:
  case GETGLOBAL:
  case SETGLOBAL:
  case PUSHATOM:
  case ATOM:
  case MAKEBLOCK1:
  case MAKEBLOCK2:
  case MAKEBLOCK3:
  case MAKEFLOATBLOCK:
  case GETFIELD:
  case SETFIELD:
  case GETFLOATFIELD:
  case SETFLOATFIELD:
  case BRANCH:
  case BRANCHIF:
  case BRANCHIFNOT:
  case PUSHTRAP:
  case CONSTINT:
  case PUSHCONSTINT:
  case OFFSETINT:
  case OFFSETREF:
  case OFFSETCLOSURE:
  case PUSHOFFSETCLOSURE:
    snprintf(buf, sizeof(buf), "%s %d", nam, pc[0]);
    break;
    /* Instructions with two operands */
  case APPTERM:
  case CLOSURE:
  case CLOSUREREC:
  case PUSHGETGLOBALFIELD:
  case GETGLOBALFIELD:
  case MAKEBLOCK:
  case BEQ:
  case BNEQ:
  case BLTINT:
  case BLEINT:
  case BGTINT:
  case BGEINT:
  case BULTINT:
  case BUGEINT:
    snprintf(buf, sizeof(buf), "%s %d, %d", nam, pc[0], pc[1]);
    break;
  case SWITCH:
    snprintf(buf, sizeof(buf), "SWITCH sz%#lx=%ld::ntag%ld nint%ld",
            (long) pc[0], (long) pc[0], (unsigned long) pc[0] >> 16,
            (unsigned long) pc[0] & 0xffff);
    break;
    /* Instructions with a C primitive as operand */
  case C_CALLN:
    snprintf(buf, sizeof(buf), "%s %d,", nam, pc[0]);
    pc++;
    /* fallthrough */
  case C_CALL1:
  case C_CALL2:
  case C_CALL3:
  case C_CALL4:
  case C_CALL5:
    if (pc[0] < 0 || pc[0] >= caml_prim_name_table.size)
      snprintf(buf, sizeof(buf), "%s unknown primitive %d", nam, pc[0]);
    else
      snprintf(buf, sizeof(buf), "%s %s",
               nam, (char *) caml_prim_name_table.contents[pc[0]]);
    break;
  default:
    snprintf(buf, sizeof(buf), "%s", nam);
    break;
  };
  return buf;
}


void
caml_trace_value_file (value v, code_t prog, int proglen, FILE * f)
{
  int i;
  fprintf (f, "%#lx", v);
  if (!v)
    return;
  if (prog && v % sizeof (int) == 0
           && (code_t) v >= prog
           && (code_t) v < (code_t) ((char *) prog + proglen))
    fprintf (f, "=code@%ld", (code_t) v - prog);
  else if (Is_long (v))
    fprintf (f, "=long%" ARCH_INTNAT_PRINTF_FORMAT "d", Long_val (v));
  else if ((void*)v >= (void*)caml_stack_low
           && (void*)v < (void*)caml_stack_high)
    fprintf (f, "=stack_%ld", (intnat*)caml_stack_high - (intnat*)v);
  else if (Is_block (v)) {
    int s = Wosize_val (v);
    int tg = Tag_val (v);
    int l = 0;
    switch (tg) {
    case Closure_tag:
      fprintf (f, "=closure[s%d,cod%ld]", s, (code_t) (Code_val (v)) - prog);
      goto displayfields;
    case String_tag:
      l = caml_string_length (v);
      fprintf (f, "=string[s%dL%d]'", s, l);
      for (i = 0; i < ((l>0x1f)?0x1f:l) ; i++) {
        if (isprint (Byte (v, i)))
          putc (Byte (v, i), f);
        else
          putc ('?', f);
      };
      fprintf (f, "'");
      goto displayfields;
    case Double_tag:
      fprintf (f, "=float[s%d]=%g", s, Double_val (v));
      goto displayfields;
    case Double_array_tag:
      fprintf (f, "=floatarray[s%d]", s);
      for (i = 0; i < ((s>0xf)?0xf:s); i++)
        fprintf (f, " %g", Double_field (v, i));
      goto displayfields;
    case Abstract_tag:
      fprintf (f, "=abstract[s%d]", s);
      goto displayfields;
    case Custom_tag:
      fprintf (f, "=custom[s%d]", s);
      goto displayfields;
    default:
      fprintf (f, "=block<T%d/s%d>", tg, s);
    displayfields:
      if (s > 0)
        fputs ("=(", f);
      for (i = 0; i < s; i++) {
        if (i > 20) {
          fputs ("....", f);
          break;
        };
        if (i > 0)
          putc (' ', f);
        fprintf (f, "%#lx", Field (v, i));
      };
      if (s > 0)
        putc (')', f);
    };
  }
}

void
caml_trace_accu_sp_file (value accu, value * sp, code_t prog, int proglen,
                         FILE * f)
{
  int i;
  value *p;
  fprintf (f, "accu=");
  caml_trace_value_file (accu, prog, proglen, f);
  fprintf (f, "\n sp=%#" ARCH_INTNAT_PRINTF_FORMAT "x @%ld:",
           (intnat) sp, caml_stack_high - sp);
  for (p = sp, i = 0; i < 12 + (1 << caml_trace_flag) && p < caml_stack_high;
       p++, i++) {
    fprintf (f, "\n[%ld] ", caml_stack_high - p);
    caml_trace_value_file (*p, prog, proglen, f);
  };
  putc ('\n', f);
  fflush (f);
}

#endif /* DEBUG */
