/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/fail.h"
#include "caml/hash.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#include "bng.h"
#include "nat.h"

/* Stub code for the Nat module. */

static intnat hash_nat(value);
static void serialize_nat(value, uintnat *, uintnat *);
static uintnat deserialize_nat(void * dst);

static struct custom_operations nat_operations = {
  "_nat",
  custom_finalize_default,
  custom_compare_default,
  hash_nat,
  serialize_nat,
  deserialize_nat,
  custom_compare_ext_default
};

CAMLprim value initialize_nat(value unit)
{
  bng_init();
  register_custom_operations(&nat_operations);
  return Val_unit;
}

CAMLprim value create_nat(value size)
{
  mlsize_t sz = Long_val(size);

  return alloc_custom(&nat_operations, sz * sizeof(value), 0, 1);
}

CAMLprim value length_nat(value nat)
{
  return Val_long(Wosize_val(nat) - 1);
}

CAMLprim value set_to_zero_nat(value nat, value ofs, value len)
{
  bng_zero(&Digit_val(nat, Long_val(ofs)), Long_val(len));
  return Val_unit;
}

CAMLprim value blit_nat(value nat1, value ofs1,
                        value nat2, value ofs2,
                        value len)
{
  bng_assign(&Digit_val(nat1, Long_val(ofs1)),
             &Digit_val(nat2, Long_val(ofs2)),
             Long_val(len));
  return Val_unit;
}

CAMLprim value set_digit_nat(value nat, value ofs, value digit)
{
  Digit_val(nat, Long_val(ofs)) = Long_val(digit);
  return Val_unit;
}

CAMLprim value nth_digit_nat(value nat, value ofs)
{
  return Val_long(Digit_val(nat, Long_val(ofs)));
}

CAMLprim value set_digit_nat_native(value nat, value ofs, value digit)
{
  Digit_val(nat, Long_val(ofs)) = Nativeint_val(digit);
  return Val_unit;
}

CAMLprim value nth_digit_nat_native(value nat, value ofs)
{
  return caml_copy_nativeint(Digit_val(nat, Long_val(ofs)));
}

CAMLprim value num_digits_nat(value nat, value ofs, value len)
{
  return Val_long(bng_num_digits(&Digit_val(nat, Long_val(ofs)),
                                 Long_val(len)));
}

CAMLprim value num_leading_zero_bits_in_digit(value nat, value ofs)
{
  return
    Val_long(bng_leading_zero_bits(Digit_val(nat, Long_val(ofs))));
}

CAMLprim value is_digit_int(value nat, value ofs)
{
  return Val_bool(Digit_val(nat, Long_val(ofs)) <= Max_long);
}

CAMLprim value is_digit_zero(value nat, value ofs)
{
  return Val_bool(Digit_val(nat, Long_val(ofs)) == 0);
}

CAMLprim value is_digit_normalized(value nat, value ofs)
{
  return
    Val_bool(Digit_val(nat, Long_val(ofs)) & ((bngdigit)1 << (BNG_BITS_PER_DIGIT-1)));
}

CAMLprim value is_digit_odd(value nat, value ofs)
{
  return Val_bool(Digit_val(nat, Long_val(ofs)) & 1);
}

CAMLprim value incr_nat(value nat, value ofs, value len, value carry_in)
{
  return Val_long(bng_add_carry(&Digit_val(nat, Long_val(ofs)),
                                Long_val(len), Long_val(carry_in)));
}

value add_nat_native(value nat1, value ofs1, value len1,
                     value nat2, value ofs2, value len2, value carry_in)
{
  return Val_long(bng_add(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                          &Digit_val(nat2, Long_val(ofs2)), Long_val(len2),
                          Long_val(carry_in)));
}

CAMLprim value add_nat(value *argv, int argn)
{
  return add_nat_native(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

CAMLprim value complement_nat(value nat, value ofs, value len)
{
  bng_complement(&Digit_val(nat, Long_val(ofs)), Long_val(len));
  return Val_unit;
}

CAMLprim value decr_nat(value nat, value ofs, value len, value carry_in)
{
  return Val_long(1 ^ bng_sub_carry(&Digit_val(nat, Long_val(ofs)),
                                    Long_val(len), 1 ^ Long_val(carry_in)));
}

value sub_nat_native(value nat1, value ofs1, value len1,
                     value nat2, value ofs2, value len2, value carry_in)
{
  return Val_long(1 ^ bng_sub(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                              &Digit_val(nat2, Long_val(ofs2)), Long_val(len2),
                              1 ^ Long_val(carry_in)));
}

CAMLprim value sub_nat(value *argv, int argn)
{
  return sub_nat_native(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

value mult_digit_nat_native(value nat1, value ofs1, value len1,
                            value nat2, value ofs2, value len2,
                            value nat3, value ofs3)
{
  return
    Val_long(bng_mult_add_digit(
                   &Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                   &Digit_val(nat2, Long_val(ofs2)), Long_val(len2),
                   Digit_val(nat3, Long_val(ofs3))));
}

CAMLprim value mult_digit_nat(value *argv, int argn)
{
  return mult_digit_nat_native(argv[0], argv[1], argv[2], argv[3],
                               argv[4], argv[5], argv[6], argv[7]);
}

value mult_nat_native(value nat1, value ofs1, value len1,
                      value nat2, value ofs2, value len2,
                      value nat3, value ofs3, value len3)
{
  return
    Val_long(bng_mult_add(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                          &Digit_val(nat2, Long_val(ofs2)), Long_val(len2),
                          &Digit_val(nat3, Long_val(ofs3)), Long_val(len3)));
}

CAMLprim value mult_nat(value *argv, int argn)
{
  return mult_nat_native(argv[0], argv[1], argv[2], argv[3],
                         argv[4], argv[5], argv[6], argv[7], argv[8]);
}

value square_nat_native(value nat1, value ofs1, value len1,
                        value nat2, value ofs2, value len2)
{
  return
    Val_long(bng_square_add(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                            &Digit_val(nat2, Long_val(ofs2)), Long_val(len2)));
}

CAMLprim value square_nat(value *argv, int argn)
{
  return square_nat_native(argv[0], argv[1], argv[2],
                           argv[3], argv[4], argv[5]);
}

value shift_left_nat_native(value nat1, value ofs1, value len1,
                            value nat2, value ofs2, value nbits)
{
  Digit_val(nat2, Long_val(ofs2)) =
    bng_shift_left(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                   Long_val(nbits));
  return Val_unit;
}

CAMLprim value shift_left_nat(value *argv, int argn)
{
  return shift_left_nat_native(argv[0], argv[1], argv[2],
                               argv[3], argv[4], argv[5]);
}

value div_digit_nat_native(value natq, value ofsq,
                           value natr, value ofsr,
                           value nat1, value ofs1, value len1,
                           value nat2, value ofs2)
{
  Digit_val(natr, Long_val(ofsr)) =
    bng_div_rem_digit(&Digit_val(natq, Long_val(ofsq)),
                      &Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                      Digit_val(nat2, Long_val(ofs2)));
  return Val_unit;
}

CAMLprim value div_digit_nat(value *argv, int argn)
{
  return div_digit_nat_native(argv[0], argv[1], argv[2], argv[3],
                              argv[4], argv[5], argv[6], argv[7], argv[8]);
}

value div_nat_native(value nat1, value ofs1, value len1,
                     value nat2, value ofs2, value len2)
{
  bng_div_rem(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
              &Digit_val(nat2, Long_val(ofs2)), Long_val(len2));
  return Val_unit;
}

CAMLprim value div_nat(value *argv, int argn)
{
  return div_nat_native(argv[0], argv[1], argv[2],
                        argv[3], argv[4], argv[5]);
}

value shift_right_nat_native(value nat1, value ofs1, value len1,
                             value nat2, value ofs2, value nbits)
{
  Digit_val(nat2, Long_val(ofs2)) =
    bng_shift_right(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                    Long_val(nbits));
  return Val_unit;
}

CAMLprim value shift_right_nat(value *argv, int argn)
{
  return shift_right_nat_native(argv[0], argv[1], argv[2],
                                argv[3], argv[4], argv[5]);
}

CAMLprim value compare_digits_nat(value nat1, value ofs1,
                                  value nat2, value ofs2)
{
  bngdigit d1 = Digit_val(nat1, Long_val(ofs1));
  bngdigit d2 = Digit_val(nat2, Long_val(ofs2));
  if (d1 > d2) return Val_int(1);
  if (d1 < d2) return Val_int(-1);
  return Val_int(0);
}

value compare_nat_native(value nat1, value ofs1, value len1,
                         value nat2, value ofs2, value len2)
{
  return
    Val_int(bng_compare(&Digit_val(nat1, Long_val(ofs1)), Long_val(len1),
                        &Digit_val(nat2, Long_val(ofs2)), Long_val(len2)));
}

CAMLprim value compare_nat(value *argv, int argn)
{
  return compare_nat_native(argv[0], argv[1], argv[2],
                            argv[3], argv[4], argv[5]);
}

CAMLprim value land_digit_nat(value nat1, value ofs1, value nat2, value ofs2)
{
  Digit_val(nat1, Long_val(ofs1)) &= Digit_val(nat2, Long_val(ofs2));
  return Val_unit;
}

CAMLprim value lor_digit_nat(value nat1, value ofs1, value nat2, value ofs2)
{
  Digit_val(nat1, Long_val(ofs1)) |= Digit_val(nat2, Long_val(ofs2));
  return Val_unit;
}

CAMLprim value lxor_digit_nat(value nat1, value ofs1, value nat2, value ofs2)
{
  Digit_val(nat1, Long_val(ofs1)) ^= Digit_val(nat2, Long_val(ofs2));
  return Val_unit;
}

/* The wire format for a nat is:
   - 32-bit word: number of 32-bit words in nat
   - N 32-bit words (big-endian format)
   For little-endian platforms, the memory layout between 32-bit and 64-bit
   machines is identical, so we can write the nat using serialize_block_4.
   For big-endian 64-bit platforms, we need to swap the two 32-bit halves
   of 64-bit words to obtain the correct behavior. */

static void serialize_nat(value nat,
                          uintnat * wsize_32,
                          uintnat * wsize_64)
{
  mlsize_t len = Wosize_val(nat) - 1;

#ifdef ARCH_SIXTYFOUR
  len = len * 2; /* two 32-bit words per 64-bit digit  */
  if (len >= ((mlsize_t)1 << 32))
    failwith("output_value: nat too big");
#endif
  serialize_int_4((int32) len);
#if defined(ARCH_SIXTYFOUR) && defined(ARCH_BIG_ENDIAN)
  { int32 * p;
    mlsize_t i;
    for (i = len, p = Data_custom_val(nat); i > 0; i -= 2, p += 2) {
      serialize_int_4(p[1]);    /* low 32 bits of 64-bit digit */
      serialize_int_4(p[0]);    /* high 32 bits of 64-bit digit */
    }
  }
#else
  serialize_block_4(Data_custom_val(nat), len);
#endif
  *wsize_32 = len * 4;
  *wsize_64 = len * 4;
}

static uintnat deserialize_nat(void * dst)
{
  mlsize_t len;

  len = deserialize_uint_4();
#if defined(ARCH_SIXTYFOUR) && defined(ARCH_BIG_ENDIAN)
  { uint32 * p;
    mlsize_t i;
    for (i = len, p = dst; i > 1; i -= 2, p += 2) {
      p[1] = deserialize_uint_4();   /* low 32 bits of 64-bit digit */
      p[0] = deserialize_uint_4();   /* high 32 bits of 64-bit digit */
    }
    if (i > 0){
      p[1] = deserialize_uint_4();   /* low 32 bits of 64-bit digit */
      p[0] = 0;                      /* high 32 bits of 64-bit digit */
      ++ len;
    }
  }
#else
  deserialize_block_4(dst, len);
#if defined(ARCH_SIXTYFOUR)
  if (len & 1){
    ((uint32 *) dst)[len] = 0;
    ++ len;
  }
#endif
#endif
  return len * 4;
}

static intnat hash_nat(value v)
{
  bngsize len, i;
  uint32 h;

  len = bng_num_digits(&Digit_val(v,0), Wosize_val(v) - 1);
  h = 0;
  for (i = 0; i < len; i++) {
    bngdigit d = Digit_val(v, i);
#ifdef ARCH_SIXTYFOUR
    /* Mix the two 32-bit halves as if we were on a 32-bit platform,
       namely low 32 bits first, then high 32 bits.
       Also, ignore final 32 bits if they are zero. */
    h = caml_hash_mix_uint32(h, (uint32) d);
    d = d >> 32;
    if (d == 0 && i + 1 == len) break;
    h = caml_hash_mix_uint32(h, (uint32) d);
#else
    h = caml_hash_mix_uint32(h, d);
#endif
  }
  return h;
}
