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

/* Code specific to the PowerPC architecture. */

#define BngAdd2(res,carryout,arg1,arg2)                                     \
  asm("addc %0, %2, %3 \n\t"                                                \
      "li %1, 0 \n\t"                                                       \
      "addze %1, %1"                                                        \
      : "=r" (res), "=r" (carryout)                                         \
      : "r" (arg1), "r" (arg2))

#define BngAdd2Carry(res,carryout,arg1,arg2,carryin)                        \
  asm("addic %1, %4, -1 \n\t"                                               \
      "adde %0, %2, %3 \n\t"                                                \
      "li %1, 0 \n\t"                                                       \
      "addze %1, %1"                                                        \
      : "=r" (res), "=&r" (carryout)                                        \
      : "r" (arg1), "r" (arg2), "1" (carryin))

#define BngAdd3(res,carryaccu,arg1,arg2,arg3)                               \
  asm("addc %0, %2, %3 \n\t"                                                \
      "addze %1, %1 \n\t"                                                   \
      "addc %0, %0, %4 \n\t"                                                \
      "addze %1, %1"                                                        \
      : "=&r" (res), "=&r" (carryaccu)                                      \
      : "r" (arg1), "r" (arg2), "r" (arg3), "1" (carryaccu))

/* The "subtract" instructions interpret carry differently than what we
   need: the processor carry bit CA is 1 if no carry occured,
   0 if a carry occured.  In other terms, CA = !carry.
   Thus, subfe rd,ra,rb computes rd = ra - rb - !CA
         subfe rd,rd,rd sets rd = - !CA
         subfe rd,rd,rd; neg rd, rd sets rd = !CA and recovers "our" carry. */

#define BngSub2(res,carryout,arg1,arg2)                                     \
  asm("subfc %0, %3, %2 \n\t"                                               \
      "subfe %1, %1, %1\n\t"                                                \
      "neg %1, %1"                                                          \
      : "=r" (res), "=r" (carryout)                                         \
      : "r" (arg1), "r" (arg2))

#define BngSub2Carry(res,carryout,arg1,arg2,carryin)                        \
  asm("subfic %1, %4, 0 \n\t"                                               \
      "subfe %0, %3, %2 \n\t"                                               \
      "subfe %1, %1, %1 \n\t"                                               \
      "neg %1, %1"                                                          \
      : "=r" (res), "=&r" (carryout)                                        \
      : "r" (arg1), "r" (arg2), "1" (carryin))

/* Here is what happens with carryaccu:
       neg %1, %1       carryaccu = -carryaccu
       addze %1, %1     carryaccu += !carry1
       addze %1, %1     carryaccu += !carry2
       subifc %1, %1, 2 carryaccu = 2 - carryaccu
   Thus, carryaccu_final = carryaccu_initial + 2 - (1 - carry1) - (1 - carry2)
                         = carryaccu_initial + carry1 + carry2
*/

#define BngSub3(res,carryaccu,arg1,arg2,arg3)                               \
  asm("neg %1, %1 \n\t"                                                     \
      "subfc %0, %3, %2 \n\t"                                               \
      "addze %1, %1 \n\t"                                                   \
      "subfc %0, %4, %0 \n\t"                                               \
      "addze %1, %1 \n\t"                                                   \
      "subfic %1, %1, 2 \n\t"                                               \
      : "=&r" (res), "=&r" (carryaccu)                                      \
      : "r" (arg1), "r" (arg2), "r" (arg3), "1" (carryaccu))

#ifdef __ppc64__
#define BngMult(resh,resl,arg1,arg2)                                        \
  asm("mulld %0, %2, %3 \n\t"                                               \
      "mulhdu %1, %2, %3"                                                   \
      : "=&r" (resl), "=r" (resh)                                           \
      : "r" (arg1), "r" (arg2))
#else
#define BngMult(resh,resl,arg1,arg2)                                        \
  asm("mullw %0, %2, %3 \n\t"                                               \
      "mulhwu %1, %2, %3"                                                   \
      : "=&r" (resl), "=r" (resh)                                           \
      : "r" (arg1), "r" (arg2))
#endif
