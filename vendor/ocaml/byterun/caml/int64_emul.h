/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Software emulation of 64-bit integer arithmetic, for C compilers
   that do not support it.  */

#ifndef CAML_INT64_EMUL_H
#define CAML_INT64_EMUL_H

#ifdef CAML_INTERNALS

#include <math.h>

#ifdef ARCH_BIG_ENDIAN
#define I64_literal(hi,lo) { hi, lo }
#else
#define I64_literal(hi,lo) { lo, hi }
#endif

#define I64_split(x,hi,lo) (hi = (x).h, lo = (x).l)

/* Unsigned comparison */
static int I64_ucompare(uint64_t x, uint64_t y)
{
  if (x.h > y.h) return 1;
  if (x.h < y.h) return -1;
  if (x.l > y.l) return 1;
  if (x.l < y.l) return -1;
  return 0;
}

#define I64_ult(x, y) (I64_ucompare(x, y) < 0)

/* Signed comparison */
static int I64_compare(int64_t x, int64_t y)
{
  if ((int32_t)x.h > (int32_t)y.h) return 1;
  if ((int32_t)x.h < (int32_t)y.h) return -1;
  if (x.l > y.l) return 1;
  if (x.l < y.l) return -1;
  return 0;
}

/* Negation */
static int64_t I64_neg(int64_t x)
{
  int64_t res;
  res.l = -x.l;
  res.h = ~x.h;
  if (res.l == 0) res.h++;
  return res;
}

/* Addition */
static int64_t I64_add(int64_t x, int64_t y)
{
  int64_t res;
  res.l = x.l + y.l;
  res.h = x.h + y.h;
  if (res.l < x.l) res.h++;
  return res;
}

/* Subtraction */
static int64_t I64_sub(int64_t x, int64_t y)
{
  int64_t res;
  res.l = x.l - y.l;
  res.h = x.h - y.h;
  if (x.l < y.l) res.h--;
  return res;
}

/* Multiplication */
static int64_t I64_mul(int64_t x, int64_t y)
{
  int64_t res;
  uint32_t prod00 = (x.l & 0xFFFF) * (y.l & 0xFFFF);
  uint32_t prod10 = (x.l >> 16) * (y.l & 0xFFFF);
  uint32_t prod01 = (x.l & 0xFFFF) * (y.l >> 16);
  uint32_t prod11 = (x.l >> 16) * (y.l >> 16);
  res.l = prod00;
  res.h = prod11 + (prod01 >> 16) + (prod10 >> 16);
  prod01 = prod01 << 16; res.l += prod01; if (res.l < prod01) res.h++;
  prod10 = prod10 << 16; res.l += prod10; if (res.l < prod10) res.h++;
  res.h += x.l * y.h + x.h * y.l;
  return res;
}

#define I64_is_zero(x) (((x).l | (x).h) == 0)
#define I64_is_negative(x) ((int32_t) (x).h < 0)
#define I64_is_min_int(x) ((x).l == 0 && (x).h == 0x80000000U)
#define I64_is_minus_one(x) (((x).l & (x).h) == 0xFFFFFFFFU)

/* Bitwise operations */
static int64_t I64_and(int64_t x, int64_t y)
{
  int64_t res;
  res.l = x.l & y.l;
  res.h = x.h & y.h;
  return res;
}

static int64_t I64_or(int64_t x, int64_t y)
{
  int64_t res;
  res.l = x.l | y.l;
  res.h = x.h | y.h;
  return res;
}

static int64_t I64_xor(int64_t x, int64_t y)
{
  int64_t res;
  res.l = x.l ^ y.l;
  res.h = x.h ^ y.h;
  return res;
}

/* Shifts */
static int64_t I64_lsl(int64_t x, int s)
{
  int64_t res;
  s = s & 63;
  if (s == 0) return x;
  if (s < 32) {
    res.l = x.l << s;
    res.h = (x.h << s) | (x.l >> (32 - s));
  } else {
    res.l = 0;
    res.h = x.l << (s - 32);
  }
  return res;
}

static int64_t I64_lsr(int64_t x, int s)
{
  int64_t res;
  s = s & 63;
  if (s == 0) return x;
  if (s < 32) {
    res.l = (x.l >> s) | (x.h << (32 - s));
    res.h = x.h >> s;
  } else {
    res.l = x.h >> (s - 32);
    res.h = 0;
  }
  return res;
}

static int64_t I64_asr(int64_t x, int s)
{
  int64_t res;
  s = s & 63;
  if (s == 0) return x;
  if (s < 32) {
    res.l = (x.l >> s) | (x.h << (32 - s));
    res.h = (int32_t) x.h >> s;
  } else {
    res.l = (int32_t) x.h >> (s - 32);
    res.h = (int32_t) x.h >> 31;
  }
  return res;
}

/* Division and modulus */

#define I64_SHL1(x) x.h = (x.h << 1) | (x.l >> 31); x.l <<= 1
#define I64_SHR1(x) x.l = (x.l >> 1) | (x.h << 31); x.h >>= 1

static void I64_udivmod(uint64_t modulus, uint64_t divisor,
                        uint64_t * quo, uint64_t * mod)
{
  int64_t quotient, mask;
  int cmp;

  quotient.h = 0; quotient.l = 0;
  mask.h = 0; mask.l = 1;
  while ((int32_t) divisor.h >= 0) {
    cmp = I64_ucompare(divisor, modulus);
    I64_SHL1(divisor);
    I64_SHL1(mask);
    if (cmp >= 0) break;
  }
  while (mask.l | mask.h) {
    if (I64_ucompare(modulus, divisor) >= 0) {
      quotient.h |= mask.h; quotient.l |= mask.l;
      modulus = I64_sub(modulus, divisor);
    }
    I64_SHR1(mask);
    I64_SHR1(divisor);
  }
  *quo = quotient;
  *mod = modulus;
}

static int64_t I64_div(int64_t x, int64_t y)
{
  int64_t q, r;
  int32_t sign;

  sign = x.h ^ y.h;
  if ((int32_t) x.h < 0) x = I64_neg(x);
  if ((int32_t) y.h < 0) y = I64_neg(y);
  I64_udivmod(x, y, &q, &r);
  if (sign < 0) q = I64_neg(q);
  return q;
}

static int64_t I64_mod(int64_t x, int64_t y)
{
  int64_t q, r;
  int32_t sign;

  sign = x.h;
  if ((int32_t) x.h < 0) x = I64_neg(x);
  if ((int32_t) y.h < 0) y = I64_neg(y);
  I64_udivmod(x, y, &q, &r);
  if (sign < 0) r = I64_neg(r);
  return r;
}

/* Coercions */

static int64_t I64_of_int32(int32_t x)
{
  int64_t res;
  res.l = x;
  res.h = x >> 31;
  return res;
}

#define I64_to_int32(x) ((int32_t) (x).l)

/* Note: we assume sizeof(intnat) = 4 here, which is true otherwise
   autoconfiguration would have selected native 64-bit integers */
#define I64_of_intnat I64_of_int32
#define I64_to_intnat I64_to_int32

static double I64_to_double(int64_t x)
{
  double res;
  int32_t sign = x.h;
  if (sign < 0) x = I64_neg(x);
  res = ldexp((double) x.h, 32) + x.l;
  if (sign < 0) res = -res;
  return res;
}

static int64_t I64_of_double(double f)
{
  int64_t res;
  double frac, integ;
  int neg;

  neg = (f < 0);
  f = fabs(f);
  frac = modf(ldexp(f, -32), &integ);
  res.h = (uint32_t) integ;
  res.l = (uint32_t) ldexp(frac, 32);
  if (neg) res = I64_neg(res);
  return res;
}

static int64_t I64_bswap(int64_t x)
{
  int64_t res;
  res.h = (((x.l & 0x000000FF) << 24) |
           ((x.l & 0x0000FF00) << 8) |
           ((x.l & 0x00FF0000) >> 8) |
           ((x.l & 0xFF000000) >> 24));
  res.l = (((x.h & 0x000000FF) << 24) |
           ((x.h & 0x0000FF00) << 8) |
           ((x.h & 0x00FF0000) >> 8) |
           ((x.h & 0xFF000000) >> 24));
  return res;
}

#endif /* CAML_INTERNALS */

#endif /* CAML_INT64_EMUL_H */
