/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2013 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* Code specific for the ARM 64 (AArch64) architecture */

#define BngMult(resh,resl,arg1,arg2)                                        \
  asm("mul %0, %2, %3 \n\t"                                                 \
      "umulh %1, %2, %3"                                                    \
      : "=&r" (resl), "=&r" (resh)                                          \
      : "r" (arg1), "r" (arg2))
