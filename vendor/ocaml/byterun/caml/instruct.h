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

/* The instruction set. */

#ifndef CAML_INSTRUCT_H
#define CAML_INSTRUCT_H

enum instructions {
  ACC0, ACC1, ACC2, ACC3, ACC4, ACC5, ACC6, ACC7,
  ACC, PUSH,
  PUSHACC0, PUSHACC1, PUSHACC2, PUSHACC3,
  PUSHACC4, PUSHACC5, PUSHACC6, PUSHACC7,
  PUSHACC, POP, ASSIGN,
  ENVACC1, ENVACC2, ENVACC3, ENVACC4, ENVACC,
  PUSHENVACC1, PUSHENVACC2, PUSHENVACC3, PUSHENVACC4, PUSHENVACC,
  PUSH_RETADDR, APPLY, APPLY1, APPLY2, APPLY3,
  APPTERM, APPTERM1, APPTERM2, APPTERM3,
  RETURN, RESTART, GRAB,
  CLOSURE, CLOSUREREC,
  OFFSETCLOSUREM2, OFFSETCLOSURE0, OFFSETCLOSURE2, OFFSETCLOSURE,
  PUSHOFFSETCLOSUREM2, PUSHOFFSETCLOSURE0,
  PUSHOFFSETCLOSURE2, PUSHOFFSETCLOSURE,
  GETGLOBAL, PUSHGETGLOBAL, GETGLOBALFIELD, PUSHGETGLOBALFIELD, SETGLOBAL,
  ATOM0, ATOM, PUSHATOM0, PUSHATOM,
  MAKEBLOCK, MAKEBLOCK1, MAKEBLOCK2, MAKEBLOCK3, MAKEFLOATBLOCK,
  GETFIELD0, GETFIELD1, GETFIELD2, GETFIELD3, GETFIELD, GETFLOATFIELD,
  SETFIELD0, SETFIELD1, SETFIELD2, SETFIELD3, SETFIELD, SETFLOATFIELD,
  VECTLENGTH, GETVECTITEM, SETVECTITEM,
  GETSTRINGCHAR, SETSTRINGCHAR,
  BRANCH, BRANCHIF, BRANCHIFNOT, SWITCH, BOOLNOT,
  PUSHTRAP, POPTRAP, RAISE,
  CHECK_SIGNALS,
  C_CALL1, C_CALL2, C_CALL3, C_CALL4, C_CALL5, C_CALLN,
  CONST0, CONST1, CONST2, CONST3, CONSTINT,
  PUSHCONST0, PUSHCONST1, PUSHCONST2, PUSHCONST3, PUSHCONSTINT,
  NEGINT, ADDINT, SUBINT, MULINT, DIVINT, MODINT,
  ANDINT, ORINT, XORINT, LSLINT, LSRINT, ASRINT,
  EQ, NEQ, LTINT, LEINT, GTINT, GEINT,
  OFFSETINT, OFFSETREF, ISINT,
  GETMETHOD,
  BEQ, BNEQ,  BLTINT, BLEINT, BGTINT, BGEINT,
  ULTINT, UGEINT,
  BULTINT, BUGEINT,
  GETPUBMET, GETDYNMET,
  STOP,
  EVENT, BREAK,
  RERAISE, RAISE_NOTRACE,
FIRST_UNIMPLEMENTED_OP};


#endif /* CAML_INSTRUCT_H */
