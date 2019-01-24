/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* The interface of this file is in "caml/mlvalues.h" and "caml/alloc.h" */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <limits.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/reverse.h"
#include "caml/stacks.h"

#ifdef _MSC_VER
#include <float.h>
#ifndef isnan
#define isnan _isnan
#endif
#ifndef isfinite
#define isfinite _finite
#endif
#endif

#ifdef ARCH_ALIGN_DOUBLE

CAMLexport double caml_Double_val(value val)
{
  union { value v[2]; double d; } buffer;

  CAMLassert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

CAMLexport void caml_Store_double_val(value val, double dbl)
{
  union { value v[2]; double d; } buffer;

  CAMLassert(sizeof(double) == 2 * sizeof(value));
  buffer.d = dbl;
  Field(val, 0) = buffer.v[0];
  Field(val, 1) = buffer.v[1];
}

#endif

CAMLexport value caml_copy_double(double d)
{
  value res;

#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

#ifndef FLAT_FLOAT_ARRAY
CAMLexport void caml_Store_double_array_field(value val, mlsize_t i, double dbl)
{
  CAMLparam1 (val);
  value d = caml_copy_double (dbl);

  CAMLassert (Tag_val (val) != Double_array_tag);
  caml_modify (&Field(val, i), d);
  CAMLreturn0;
}
#endif /* ! FLAT_FLOAT_ARRAY */

CAMLprim value caml_format_float(value fmt, value arg)
{
  value res;
  double d = Double_val(arg);

#ifdef HAS_BROKEN_PRINTF
  if (isfinite(d)) {
#endif
    res = caml_alloc_sprintf(String_val(fmt), d);
#ifdef HAS_BROKEN_PRINTF
  } else {
    if (isnan(d)) {
      res = caml_copy_string("nan");
    } else {
      if (d > 0)
        res = caml_copy_string("inf");
      else
        res = caml_copy_string("-inf");
    }
  }
#endif
  return res;
}

CAMLprim value caml_hexstring_of_float(value arg, value vprec, value vstyle)
{
  union { uint64_t i; double d; } u;
  int sign, exp;
  uint64_t m;
  char buffer[64];
  char * buf, * p;
  intnat prec;
  int d;
  value res;

  /* Allocate output buffer */
  prec = Long_val(vprec);
                  /* 12 chars for sign, 0x, decimal point, exponent */
  buf = (prec + 12 <= 64 ? buffer : caml_stat_alloc(prec + 12));
  /* Extract sign, mantissa, and exponent */
  u.d = Double_val(arg);
  sign = u.i >> 63;
  exp = (u.i >> 52) & 0x7FF;
  m = u.i & (((uint64_t) 1 << 52) - 1);
  /* Put sign */
  p = buf;
  if (sign) {
    *p++ = '-';
  } else {
    switch (Int_val(vstyle)) {
    case '+': *p++ = '+'; break;
    case ' ': *p++ = ' '; break;
    }
  }
  /* Treat special cases */
  if (exp == 0x7FF) {
    char * txt;
    if (m == 0) txt = "infinity"; else txt = "nan";
    memcpy(p, txt, strlen(txt));
    p[strlen(txt)] = 0;
    res = caml_copy_string(buf);
  } else {
    /* Output "0x" prefix */
    *p++ = '0'; *p++ = 'x';
    /* Normalize exponent and mantissa */
    if (exp == 0) {
      if (m != 0) exp = -1022;    /* denormal */
    } else {
      exp = exp - 1023;
      m = m | ((uint64_t) 1 << 52);
    }
    /* If a precision is given, and is small, round mantissa accordingly */
    prec = Long_val(vprec);
    if (prec >= 0 && prec < 13) {
      int i = 52 - prec * 4;
      uint64_t unit = (uint64_t) 1 << i;
      uint64_t half = unit >> 1;
      uint64_t mask = unit - 1;
      uint64_t frac = m & mask;
      m = m & ~mask;
      /* Round to nearest, ties to even */
      if (frac > half || (frac == half && (m & unit) != 0)) {
        m += unit;
      }
    }
    /* Leading digit */
    d = m >> 52;
    *p++ = (d < 10 ? d + '0' : d - 10 + 'a');
    m = (m << 4) & (((uint64_t) 1 << 56) - 1);
    /* Fractional digits.  If a precision is given, print that number of
       digits.  Otherwise, print as many digits as needed to represent
       the mantissa exactly. */
    if (prec >= 0 ? prec > 0 : m != 0) {
      *p++ = '.';
      while (prec >= 0 ? prec > 0 : m != 0) {
        d = m >> 52;
        *p++ = (d < 10 ? d + '0' : d - 10 + 'a');
        m = (m << 4) & (((uint64_t) 1 << 56) - 1);
        prec--;
      }
    }
    *p = 0;
    /* Add exponent */
    res = caml_alloc_sprintf("%sp%+d", buf, exp);
  }
  if (buf != buffer) caml_stat_free(buf);
  return res;
}

static int caml_float_of_hex(const char * s, double * res)
{
  int64_t m = 0;                /* the mantissa - top 60 bits at most */
  int n_bits = 0;               /* total number of bits read */
  int m_bits = 0;               /* number of bits in mantissa */
  int x_bits = 0;               /* number of bits after mantissa */
  int dec_point = -1;           /* bit count corresponding to decimal point */
                                /* -1 if no decimal point seen */
  int exp = 0;                  /* exponent */
  char * p;                     /* for converting the exponent */
  double f;

  while (*s != 0) {
    char c = *s++;
    switch (c) {
    case '_':
      break;
    case '.':
      if (dec_point >= 0) return -1; /* multiple decimal points */
      dec_point = n_bits;
      break;
    case 'p': case 'P': {
      long e;
      if (*s == 0) return -1;   /* nothing after exponent mark */
      e = strtol(s, &p, 10);
      if (*p != 0) return -1;   /* ill-formed exponent */
      if (e < INT_MIN || e > INT_MAX) return -1; /* unreasonable exponent */
      exp = e;
      s = p;                    /* stop at next loop iteration */
      break;
    }
    default: {                  /* Nonzero digit */
      int d;
      if (c >= '0' && c <= '9') d = c - '0';
      else if (c >= 'A' && c <= 'F') d = c - 'A' + 10;
      else if (c >= 'a' && c <= 'f') d = c - 'a' + 10;
      else return -1;           /* bad digit */
      n_bits += 4;
      if (d == 0 && m == 0) break; /* leading zeros are skipped */
      if (m_bits < 60) {
        /* There is still room in m.  Add this digit to the mantissa. */
        m = (m << 4) + d;
        m_bits += 4;
      } else {
        /* We've already collected 60 significant bits in m.
           Now all we care about is whether there is a nonzero bit
           after. In this case, round m to odd so that the later
           rounding of m to FP produces the correct result. */
        if (d != 0) m |= 1;        /* round to odd */
        x_bits += 4;
      }
      break;
    }
    }
  }
  if (n_bits == 0) return -1;
  /* Convert mantissa to FP.  We use a signed conversion because we can
     (m has 60 bits at most) and because it is faster
     on several architectures. */
  f = (double) (int64_t) m;
  /* Adjust exponent to take decimal point and extra digits into account */
  if (dec_point >= 0) exp = exp + (dec_point - n_bits);
  exp = exp + x_bits;
  /* Apply exponent if needed */
  if (exp != 0) f = ldexp(f, exp);
  /* Done! */
  *res = f;
  return 0;
}

CAMLprim value caml_float_of_string(value vs)
{
  char parse_buffer[64];
  char * buf, * dst, * end;
  const char *src;
  mlsize_t len;
  int sign;
  double d;

  /* Check for hexadecimal FP constant */
  src = String_val(vs);
  sign = 1;
  if (*src == '-') { sign = -1; src++; }
  else if (*src == '+') { src++; };
  if (src[0] == '0' && (src[1] == 'x' || src[1] == 'X')) {
    if (caml_float_of_hex(src + 2, &d) == -1)
      caml_failwith("float_of_string");
    return caml_copy_double(sign < 0 ? -d : d);
  }
  /* Remove '_' characters before calling strtod () */
  len = caml_string_length(vs);
  buf = len < sizeof(parse_buffer) ? parse_buffer : caml_stat_alloc(len + 1);
  src = String_val(vs);
  dst = buf;
  while (len--) {
    char c = *src++;
    if (c != '_') *dst++ = c;
  }
  *dst = 0;
  if (dst == buf) goto error;
  /* Convert using strtod */
  d = strtod((const char *) buf, &end);
  if (end != dst) goto error;
  if (buf != parse_buffer) caml_stat_free(buf);
  return caml_copy_double(d);
 error:
  if (buf != parse_buffer) caml_stat_free(buf);
  caml_failwith("float_of_string");
  return Val_unit; /* not reached */
}

CAMLprim value caml_int_of_float(value f)
{
  return Val_long((intnat) Double_val(f));
}

CAMLprim value caml_float_of_int(value n)
{
  return caml_copy_double((double) Long_val(n));
}

CAMLprim value caml_neg_float(value f)
{
  return caml_copy_double(- Double_val(f));
}

CAMLprim value caml_abs_float(value f)
{
  return caml_copy_double(fabs(Double_val(f)));
}

CAMLprim value caml_add_float(value f, value g)
{
  return caml_copy_double(Double_val(f) + Double_val(g));
}

CAMLprim value caml_sub_float(value f, value g)
{
  return caml_copy_double(Double_val(f) - Double_val(g));
}

CAMLprim value caml_mul_float(value f, value g)
{
  return caml_copy_double(Double_val(f) * Double_val(g));
}

CAMLprim value caml_div_float(value f, value g)
{
  return caml_copy_double(Double_val(f) / Double_val(g));
}

CAMLprim value caml_exp_float(value f)
{
  return caml_copy_double(exp(Double_val(f)));
}

CAMLprim value caml_floor_float(value f)
{
  return caml_copy_double(floor(Double_val(f)));
}

CAMLprim value caml_fmod_float(value f1, value f2)
{
  return caml_copy_double(fmod(Double_val(f1), Double_val(f2)));
}

CAMLprim value caml_frexp_float(value f)
{
  CAMLparam1 (f);
  CAMLlocal2 (res, mantissa);
  int exponent;

  mantissa = caml_copy_double(frexp (Double_val(f), &exponent));
  res = caml_alloc_tuple(2);
  Field(res, 0) = mantissa;
  Field(res, 1) = Val_int(exponent);
  CAMLreturn (res);
}

// Seems dumb but intnat could not correspond to int type.
double caml_ldexp_float_unboxed(double f, intnat i)
{
  return ldexp(f, i);
}


CAMLprim value caml_ldexp_float(value f, value i)
{
  return caml_copy_double(ldexp(Double_val(f), Int_val(i)));
}

CAMLprim value caml_log_float(value f)
{
  return caml_copy_double(log(Double_val(f)));
}

CAMLprim value caml_log10_float(value f)
{
  return caml_copy_double(log10(Double_val(f)));
}

CAMLprim value caml_modf_float(value f)
{
  double frem;

  CAMLparam1 (f);
  CAMLlocal3 (res, quo, rem);

  quo = caml_copy_double(modf (Double_val(f), &frem));
  rem = caml_copy_double(frem);
  res = caml_alloc_tuple(2);
  Field(res, 0) = quo;
  Field(res, 1) = rem;
  CAMLreturn (res);
}

CAMLprim value caml_sqrt_float(value f)
{
  return caml_copy_double(sqrt(Double_val(f)));
}

CAMLprim value caml_power_float(value f, value g)
{
  return caml_copy_double(pow(Double_val(f), Double_val(g)));
}

CAMLprim value caml_sin_float(value f)
{
  return caml_copy_double(sin(Double_val(f)));
}

CAMLprim value caml_sinh_float(value f)
{
  return caml_copy_double(sinh(Double_val(f)));
}

CAMLprim value caml_cos_float(value f)
{
  return caml_copy_double(cos(Double_val(f)));
}

CAMLprim value caml_cosh_float(value f)
{
  return caml_copy_double(cosh(Double_val(f)));
}

CAMLprim value caml_tan_float(value f)
{
  return caml_copy_double(tan(Double_val(f)));
}

CAMLprim value caml_tanh_float(value f)
{
  return caml_copy_double(tanh(Double_val(f)));
}

CAMLprim value caml_asin_float(value f)
{
  return caml_copy_double(asin(Double_val(f)));
}

CAMLprim value caml_acos_float(value f)
{
  return caml_copy_double(acos(Double_val(f)));
}

CAMLprim value caml_atan_float(value f)
{
  return caml_copy_double(atan(Double_val(f)));
}

CAMLprim value caml_atan2_float(value f, value g)
{
  return caml_copy_double(atan2(Double_val(f), Double_val(g)));
}

CAMLprim value caml_ceil_float(value f)
{
  return caml_copy_double(ceil(Double_val(f)));
}

CAMLexport double caml_hypot(double x, double y)
{
#ifdef HAS_C99_FLOAT_OPS
  return hypot(x, y);
#else
  double tmp, ratio;
  x = fabs(x); y = fabs(y);
  if (x != x) /* x is NaN */
    return y > DBL_MAX ? y : x;  /* PR#6321 */
  if (y != y) /* y is NaN */
    return x > DBL_MAX ? x : y;  /* PR#6321 */
  if (x < y) { tmp = x; x = y; y = tmp; }
  if (x == 0.0) return 0.0;
  ratio = y / x;
  return x * sqrt(1.0 + ratio * ratio);
#endif
}

CAMLprim value caml_hypot_float(value f, value g)
{
  return caml_copy_double(caml_hypot(Double_val(f), Double_val(g)));
}

/* These emulations of expm1() and log1p() are due to William Kahan.
   See http://www.plunk.org/~hatch/rightway.php */
CAMLexport double caml_expm1(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return expm1(x);
#else
  double u = exp(x);
  if (u == 1.)
    return x;
  if (u - 1. == -1.)
    return -1.;
  return (u - 1.) * x / log(u);
#endif
}

CAMLexport double caml_log1p(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return log1p(x);
#else
  double u = 1. + x;
  if (u == 1.)
    return x;
  else
    return log(u) * x / (u - 1.);
#endif
}

CAMLprim value caml_expm1_float(value f)
{
  return caml_copy_double(caml_expm1(Double_val(f)));
}

CAMLprim value caml_log1p_float(value f)
{
  return caml_copy_double(caml_log1p(Double_val(f)));
}

union double_as_two_int32 {
    double d;
#if defined(ARCH_BIG_ENDIAN) || (defined(__arm__) && !defined(__ARM_EABI__))
    struct { uint32_t h; uint32_t l; } i;
#else
    struct { uint32_t l; uint32_t h; } i;
#endif
};

CAMLexport double caml_copysign(double x, double y)
{
#ifdef HAS_C99_FLOAT_OPS
  return copysign(x, y);
#else
  union double_as_two_int32 ux, uy;
  ux.d = x;
  uy.d = y;
  ux.i.h &= 0x7FFFFFFFU;
  ux.i.h |= (uy.i.h & 0x80000000U);
  return ux.d;
#endif
}

CAMLprim value caml_copysign_float(value f, value g)
{
  return caml_copy_double(caml_copysign(Double_val(f), Double_val(g)));
}

#ifdef LACKS_SANE_NAN

CAMLprim value caml_neq_float(value vf, value vg)
{
  double f = Double_val(vf);
  double g = Double_val(vg);
  return Val_bool(isnan(f) || isnan(g) || f != g);
}

#define DEFINE_NAN_CMP(op) (value vf, value vg) \
{ \
  double f = Double_val(vf); \
  double g = Double_val(vg); \
  return Val_bool(!isnan(f) && !isnan(g) && f op g); \
}

intnat caml_float_compare_unboxed(double f, double g)
{
  /* Insane => nan == everything && nan < everything && nan > everything */
  if (isnan(f) && isnan(g)) return 0;
  if (!isnan(g) && f < g) return -1;
  if (f != g) return 1;
  return 0;
}

#else

CAMLprim value caml_neq_float(value f, value g)
{
  return Val_bool(Double_val(f) != Double_val(g));
}

#define DEFINE_NAN_CMP(op) (value f, value g) \
{ \
  return Val_bool(Double_val(f) op Double_val(g)); \
}

intnat caml_float_compare_unboxed(double f, double g)
{
  /* If one or both of f and g is NaN, order according to the convention
     NaN = NaN and NaN < x for all other floats x. */
  /* This branchless implementation is from GPR#164.
     Note that [f == f] if and only if f is not NaN. */
  return (f > g) - (f < g) + (f == f) - (g == g);
}

#endif

CAMLprim value caml_eq_float DEFINE_NAN_CMP(==)
CAMLprim value caml_le_float DEFINE_NAN_CMP(<=)
CAMLprim value caml_lt_float DEFINE_NAN_CMP(<)
CAMLprim value caml_ge_float DEFINE_NAN_CMP(>=)
CAMLprim value caml_gt_float DEFINE_NAN_CMP(>)

CAMLprim value caml_float_compare(value vf, value vg)
{
  return Val_int(caml_float_compare_unboxed(Double_val(vf),Double_val(vg)));
}

enum { FP_normal, FP_subnormal, FP_zero, FP_infinite, FP_nan };

value caml_classify_float_unboxed(double vd)
{
#ifdef ARCH_SIXTYFOUR
  union { double d; uint64_t i; } u;
  uint64_t n;
  uint32_t e;

  u.d = vd;
  n = u.i << 1;                 /* shift sign bit off */
  if (n == 0) return Val_int(FP_zero);
  e = n >> 53;                  /* extract exponent */
  if (e == 0) return Val_int(FP_subnormal);
  if (e == 0x7FF) {
    if (n << 11 == 0)           /* shift exponent off */
      return Val_int(FP_infinite);
    else
      return Val_int(FP_nan);
  }
  return Val_int(FP_normal);
#else
  union double_as_two_int32 u;
  uint32_t h, l;

  u.d = vd;
  h = u.i.h;  l = u.i.l;
  l = l | (h & 0xFFFFF);
  h = h & 0x7FF00000;
  if ((h | l) == 0)
    return Val_int(FP_zero);
  if (h == 0)
    return Val_int(FP_subnormal);
  if (h == 0x7FF00000) {
    if (l == 0)
      return Val_int(FP_infinite);
    else
      return Val_int(FP_nan);
  }
  return Val_int(FP_normal);
#endif
}

CAMLprim value caml_classify_float(value vd)
{
  return caml_classify_float_unboxed(Double_val(vd));
}

/* The [caml_init_ieee_float] function should initialize floating-point hardware
   so that it behaves as much as possible like the IEEE standard.
   In particular, return special numbers like Infinity and NaN instead
   of signalling exceptions.  Currently, everyone is in IEEE mode
   at program startup, except FreeBSD prior to 4.0R. */

#ifdef __FreeBSD__
#include <osreldate.h>
#if (__FreeBSD_version < 400017)
#include <floatingpoint.h>
#endif
#endif

void caml_init_ieee_floats(void)
{
#if defined(__FreeBSD__) && (__FreeBSD_version < 400017)
  fpsetmask(0);
#endif
}
