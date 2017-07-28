/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Code specific to the SPARC (V8 and above) architecture. */

#define BngAdd2(res,carryout,arg1,arg2)                                     \
  asm("addcc %2, %3, %0 \n\t"                                               \
      "addx  %%g0, 0, %1"                                                   \
      : "=r" (res), "=r" (carryout)                                         \
      : "r" (arg1), "r" (arg2)                                              \
      : "cc")

#define BngAdd2Carry(res,carryout,arg1,arg2,carryin)                        \
  asm("subcc %%g0, %4, %%g0 \n\t"                                           \
      "addxcc %2, %3, %0 \n\t"                                              \
      "addx  %%g0, 0, %1"                                                   \
      : "=r" (res), "=r" (carryout)                                         \
      : "r" (arg1), "r" (arg2), "r" (carryin)                               \
      : "cc")

#define BngAdd3(res,carryaccu,arg1,arg2,arg3)                               \
  asm("addcc %2, %3, %0 \n\t"                                               \
      "addx %1, 0, %1 \n\t"                                                 \
      "addcc %0, %4, %0 \n\t"                                               \
      "addx %1, 0, %1"                                                      \
      : "=r" (res), "=r" (carryaccu)                                        \
      : "r" (arg1), "r" (arg2), "r" (arg3), "1" (carryaccu)                 \
      : "cc")

#define BngSub2(res,carryout,arg1,arg2)                                     \
  asm("subcc %2, %3, %0 \n\t"                                               \
      "addx  %%g0, 0, %1"                                                   \
      : "=r" (res), "=r" (carryout)                                         \
      : "r" (arg1), "r" (arg2)                                              \
      : "cc")

#define BngSub2Carry(res,carryout,arg1,arg2,carryin)                        \
  asm("subcc %%g0, %4, %%g0 \n\t"                                           \
      "subxcc %2, %3, %0 \n\t"                                              \
      "addx  %%g0, 0, %1"                                                   \
      : "=r" (res), "=r" (carryout)                                         \
      : "r" (arg1), "r" (arg2), "r" (carryin)                               \
      : "cc")

#define BngSub3(res,carryaccu,arg1,arg2,arg3)                               \
  asm("subcc %2, %3, %0 \n\t"                                               \
      "addx %1, 0, %1 \n\t"                                                 \
      "subcc %0, %4, %0 \n\t"                                               \
      "addx %1, 0, %1"                                                      \
      : "=r" (res), "=r" (carryaccu)                                        \
      : "r" (arg1), "r" (arg2), "r" (arg3), "1" (carryaccu)                 \
      : "cc")

#define BngMult(resh,resl,arg1,arg2)                                        \
  asm("umul %2, %3, %0 \n\t"                                                \
      "rd %%y, %1"                                                          \
      : "=r" (resl), "=r" (resh)                                            \
      : "r" (arg1), "r" (arg2))

#define BngDiv(quo,rem,nh,nl,d)                                             \
  asm("wr %1, %%y \n\t"                                                     \
      "udiv %2, %3, %0"                                                     \
      : "=r" (quo)                                                          \
      : "r" (nh), "r" (nl), "r" (d));                                       \
  rem = nl - d * quo
