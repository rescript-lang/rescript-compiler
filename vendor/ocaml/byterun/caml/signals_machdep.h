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

/* Processor-specific operation: atomic "read and clear" */

#ifndef CAML_SIGNALS_MACHDEP_H
#define CAML_SIGNALS_MACHDEP_H

#if defined(__GNUC__) && defined(__i386__)

#define Read_and_clear(dst,src) \
  asm("xorl %0, %0; xchgl %0, %1" \
      : "=r" (dst), "=m" (src) \
      : "m" (src))

#elif defined(__GNUC__) && defined(__x86_64__)

#define Read_and_clear(dst,src) \
  asm("xorq %0, %0; xchgq %0, %1" \
      : "=r" (dst), "=m" (src) \
      : "m" (src))

#elif defined(__GNUC__) && defined(__ppc__)

#define Read_and_clear(dst,src) \
  asm("0: lwarx %0, 0, %1\n\t" \
      "stwcx. %2, 0, %1\n\t" \
      "bne- 0b" \
      : "=&r" (dst) \
      : "r" (&(src)), "r" (0) \
      : "cr0", "memory")

#elif defined(__GNUC__) && defined(__ppc64__)

#define Read_and_clear(dst,src) \
  asm("0: ldarx %0, 0, %1\n\t" \
      "stdcx. %2, 0, %1\n\t" \
      "bne- 0b" \
      : "=&r" (dst) \
      : "r" (&(src)), "r" (0) \
      : "cr0", "memory")

#else

/* Default, non-atomic implementation */
#define Read_and_clear(dst,src) ((dst) = (src), (src) = 0)

#endif

#endif /* CAML_SIGNALS_MACHDEP_H */
