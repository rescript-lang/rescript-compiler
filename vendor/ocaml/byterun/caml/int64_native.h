/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Wrapper macros around native 64-bit integer arithmetic,
   so that it has the same interface as the software emulation
   provided in int64_emul.h */

#ifndef CAML_INT64_NATIVE_H
#define CAML_INT64_NATIVE_H

#define I64_literal(hi,lo) ((int64)(hi) << 32 | (lo))
#define I64_split(x,hi,lo) (hi = (uint32)((x)>>32), lo = (uint32)(x))
#define I64_compare(x,y) (((x) > (y)) - ((x) < (y)))
#define I64_ult(x,y) ((uint64)(x) < (uint64)(y))
#define I64_neg(x) (-(x))
#define I64_add(x,y) ((x) + (y))
#define I64_sub(x,y) ((x) - (y))
#define I64_mul(x,y) ((x) * (y))
#define I64_is_zero(x) ((x) == 0)
#define I64_is_negative(x) ((x) < 0)
#define I64_is_min_int(x) ((x) == ((int64)1 << 63))
#define I64_is_minus_one(x) ((x) == -1)

#define I64_div(x,y) ((x) / (y))
#define I64_mod(x,y) ((x) % (y))
#define I64_udivmod(x,y,quo,rem) \
  (*(rem) = (uint64)(x) % (uint64)(y), \
   *(quo) = (uint64)(x) / (uint64)(y))
#define I64_and(x,y) ((x) & (y))
#define I64_or(x,y) ((x) | (y))
#define I64_xor(x,y) ((x) ^ (y))
#define I64_lsl(x,y) ((x) << (y))
#define I64_asr(x,y) ((x) >> (y))
#define I64_lsr(x,y) ((uint64)(x) >> (y))
#define I64_to_intnat(x) ((intnat) (x))
#define I64_of_intnat(x) ((intnat) (x))
#define I64_to_int32(x) ((int32) (x))
#define I64_of_int32(x) ((int64) (x))
#define I64_to_double(x) ((double)(x))
#define I64_of_double(x) ((int64)(x))

#define I64_bswap(x) ((((x) & 0x00000000000000FFULL) << 56) | \
                      (((x) & 0x000000000000FF00ULL) << 40) | \
                      (((x) & 0x0000000000FF0000ULL) << 24) | \
                      (((x) & 0x00000000FF000000ULL) << 8) |  \
                      (((x) & 0x000000FF00000000ULL) >> 8) |  \
                      (((x) & 0x0000FF0000000000ULL) >> 24) | \
                      (((x) & 0x00FF000000000000ULL) >> 40) | \
                      (((x) & 0xFF00000000000000ULL) >> 56))

#endif /* CAML_INT64_NATIVE_H */
