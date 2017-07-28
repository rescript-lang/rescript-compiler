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

/* The interface of this file is in "caml/mlvalues.h" and "caml/alloc.h" */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/reverse.h"
#include "caml/stacks.h"

#ifdef _MSC_VER
#include <float.h>
#define isnan _isnan
#define isfinite _finite
#endif

#ifdef ARCH_ALIGN_DOUBLE

CAMLexport double caml_Double_val(value val)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

CAMLexport void caml_Store_double_val(value val, double dbl)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
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

#if 0
/*CAMLprim*/ value caml_float_of_substring(value vs, value idx, value l)
{
  char parse_buffer[64];
  char * buf, * src, * dst, * end;
  mlsize_t len, lenvs;
  double d;
  intnat flen = Long_val(l);
  intnat fidx = Long_val(idx);

  lenvs = caml_string_length(vs);
  len =
    fidx >= 0 && fidx < lenvs && flen > 0 && flen <= lenvs - fidx
    ? flen : 0;
  buf = len < sizeof(parse_buffer) ? parse_buffer : caml_stat_alloc(len + 1);
  src = String_val(vs) + fidx;
  dst = buf;
  while (len--) {
    char c = *src++;
    if (c != '_') *dst++ = c;
  }
  *dst = 0;
  if (dst == buf) goto error;
  d = strtod((const char *) buf, &end);
  if (end != dst) goto error;
  if (buf != parse_buffer) caml_stat_free(buf);
  return caml_copy_double(d);
 error:
  if (buf != parse_buffer) caml_stat_free(buf);
  caml_failwith("float_of_string");
}
#endif

CAMLprim value caml_float_of_string(value vs)
{
  char parse_buffer[64];
  char * buf, * src, * dst, * end;
  mlsize_t len;
  double d;

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
  if (x != x) return x;  /* NaN */
  if (y != y) return y;  /* NaN */
  x = fabs(x); y = fabs(y);
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
    struct { uint32 h; uint32 l; } i;
#else
    struct { uint32 l; uint32 h; } i;
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

CAMLprim value caml_eq_float(value f, value g)
{
  return Val_bool(Double_val(f) == Double_val(g));
}

CAMLprim value caml_neq_float(value f, value g)
{
  return Val_bool(Double_val(f) != Double_val(g));
}

CAMLprim value caml_le_float(value f, value g)
{
  return Val_bool(Double_val(f) <= Double_val(g));
}

CAMLprim value caml_lt_float(value f, value g)
{
  return Val_bool(Double_val(f) < Double_val(g));
}

CAMLprim value caml_ge_float(value f, value g)
{
  return Val_bool(Double_val(f) >= Double_val(g));
}

CAMLprim value caml_gt_float(value f, value g)
{
  return Val_bool(Double_val(f) > Double_val(g));
}

CAMLprim value caml_float_compare(value vf, value vg)
{
  double f = Double_val(vf);
  double g = Double_val(vg);
  if (f == g) return Val_int(0);
  if (f < g) return Val_int(-1);
  if (f > g) return Val_int(1);
  /* One or both of f and g is NaN.  Order according to the
     convention NaN = NaN and NaN < x for all other floats x. */
  if (f == f) return Val_int(1);  /* f is not NaN, g is NaN */
  if (g == g) return Val_int(-1); /* g is not NaN, f is NaN */
  return Val_int(0);              /* both f and g are NaN */
}

enum { FP_normal, FP_subnormal, FP_zero, FP_infinite, FP_nan };

CAMLprim value caml_classify_float(value vd)
{
  /* Cygwin 1.3 has problems with fpclassify (PR#1293), so don't use it */
  /* FIXME Cygwin 1.3 is ancient! Revisit this decision. */
#if defined(fpclassify) && !defined(__CYGWIN__) && !defined(__MINGW32__)
  switch (fpclassify(Double_val(vd))) {
  case FP_NAN:
    return Val_int(FP_nan);
  case FP_INFINITE:
    return Val_int(FP_infinite);
  case FP_ZERO:
    return Val_int(FP_zero);
  case FP_SUBNORMAL:
    return Val_int(FP_subnormal);
  default: /* case FP_NORMAL */
    return Val_int(FP_normal);
  }
#else
  union double_as_two_int32 u;
  uint32 h, l;

  u.d = Double_val(vd);
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
