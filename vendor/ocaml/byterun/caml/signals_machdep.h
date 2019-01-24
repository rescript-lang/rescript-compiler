/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Processor-specific operation: atomic "read and clear" */

#ifndef CAML_SIGNALS_MACHDEP_H
#define CAML_SIGNALS_MACHDEP_H

#ifdef CAML_INTERNALS

#if defined(__GNUC__) && defined(__ATOMIC_SEQ_CST) \
    && defined(__GCC_ATOMIC_LONG_LOCK_FREE)

/* Use the "atomic" builtins of GCC and Clang */
#define Read_and_clear(dst,src) \
  ((dst) = __atomic_exchange_n(&(src), 0, __ATOMIC_SEQ_CST))

#elif defined(__GNUC__) && (defined(__i386__) || (defined(__x86_64__) \
      && defined(__ILP32__)))

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

#endif /* CAML_INTERNALS */

#endif /* CAML_SIGNALS_MACHDEP_H */
