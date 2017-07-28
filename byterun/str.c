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

/* Operations on strings */

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#ifdef HAS_LOCALE
#include <locale.h>
#endif

CAMLexport mlsize_t caml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

CAMLprim value caml_ml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}

CAMLprim value caml_create_string(value len)
{
  mlsize_t size = Long_val(len);
  if (size > Bsize_wsize (Max_wosize) - 1){
    caml_invalid_argument("String.create");
  }
  return caml_alloc_string(size);
}

CAMLprim value caml_string_get(value str, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str)) caml_array_bound_error();
  return Val_int(Byte_u(str, idx));
}

CAMLprim value caml_string_set(value str, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str)) caml_array_bound_error();
  Byte_u(str, idx) = Int_val(newval);
  return Val_unit;
}

CAMLprim value caml_string_get16(value str, value index)
{
  intnat res;
  unsigned char b1, b2;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 1 >= caml_string_length(str)) caml_array_bound_error();
  b1 = Byte_u(str, idx);
  b2 = Byte_u(str, idx + 1);
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 8 | b2;
#else
  res = b2 << 8 | b1;
#endif
  return Val_int(res);
}

CAMLprim value caml_string_get32(value str, value index)
{
  intnat res;
  unsigned char b1, b2, b3, b4;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 3 >= caml_string_length(str)) caml_array_bound_error();
  b1 = Byte_u(str, idx);
  b2 = Byte_u(str, idx + 1);
  b3 = Byte_u(str, idx + 2);
  b4 = Byte_u(str, idx + 3);
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 24 | b2 << 16 | b3 << 8 | b4;
#else
  res = b4 << 24 | b3 << 16 | b2 << 8 | b1;
#endif
  return caml_copy_int32(res);
}

CAMLprim value caml_string_get64(value str, value index)
{
  uint64 res;
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 7 >= caml_string_length(str)) caml_array_bound_error();
  b1 = Byte_u(str, idx);
  b2 = Byte_u(str, idx + 1);
  b3 = Byte_u(str, idx + 2);
  b4 = Byte_u(str, idx + 3);
  b5 = Byte_u(str, idx + 4);
  b6 = Byte_u(str, idx + 5);
  b7 = Byte_u(str, idx + 6);
  b8 = Byte_u(str, idx + 7);
#ifdef ARCH_BIG_ENDIAN
  res = (uint64) b1 << 56 | (uint64) b2 << 48
        | (uint64) b3 << 40 | (uint64) b4 << 32
        | (uint64) b5 << 24 | (uint64) b6 << 16
        | (uint64) b7 << 8 | (uint64) b8;
#else
  res = (uint64) b8 << 56 | (uint64) b7 << 48
        | (uint64) b6 << 40 | (uint64) b5 << 32
        | (uint64) b4 << 24 | (uint64) b3 << 16
        | (uint64) b2 << 8 | (uint64) b1;
#endif
  return caml_copy_int64(res);
}

CAMLprim value caml_string_set16(value str, value index, value newval)
{
  unsigned char b1, b2;
  intnat val;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 1 >= caml_string_length(str)) caml_array_bound_error();
  val = Long_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 8;
  b2 = 0xFF & val;
#else
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  return Val_unit;
}

CAMLprim value caml_string_set32(value str, value index, value newval)
{
  unsigned char b1, b2, b3, b4;
  intnat val;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 3 >= caml_string_length(str)) caml_array_bound_error();
  val = Int32_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 24;
  b2 = 0xFF & val >> 16;
  b3 = 0xFF & val >> 8;
  b4 = 0xFF & val;
#else
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  Byte_u(str, idx + 2) = b3;
  Byte_u(str, idx + 3) = b4;
  return Val_unit;
}

CAMLprim value caml_string_set64(value str, value index, value newval)
{
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  int64 val;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 7 >= caml_string_length(str)) caml_array_bound_error();
  val = Int64_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 56;
  b2 = 0xFF & val >> 48;
  b3 = 0xFF & val >> 40;
  b4 = 0xFF & val >> 32;
  b5 = 0xFF & val >> 24;
  b6 = 0xFF & val >> 16;
  b7 = 0xFF & val >> 8;
  b8 = 0xFF & val;
#else
  b8 = 0xFF & val >> 56;
  b7 = 0xFF & val >> 48;
  b6 = 0xFF & val >> 40;
  b5 = 0xFF & val >> 32;
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  Byte_u(str, idx + 2) = b3;
  Byte_u(str, idx + 3) = b4;
  Byte_u(str, idx + 4) = b5;
  Byte_u(str, idx + 5) = b6;
  Byte_u(str, idx + 6) = b7;
  Byte_u(str, idx + 7) = b8;
  return Val_unit;
}

CAMLprim value caml_string_equal(value s1, value s2)
{
  mlsize_t sz1, sz2;
  value * p1, * p2;

  if (s1 == s2) return Val_true;
  sz1 = Wosize_val(s1);
  sz2 = Wosize_val(s2);
  if (sz1 != sz2) return Val_false;
  for(p1 = Op_val(s1), p2 = Op_val(s2); sz1 > 0; sz1--, p1++, p2++)
    if (*p1 != *p2) return Val_false;
  return Val_true;
}

CAMLprim value caml_string_notequal(value s1, value s2)
{
  return Val_not(caml_string_equal(s1, s2));
}

CAMLprim value caml_string_compare(value s1, value s2)
{
  mlsize_t len1, len2;
  int res;

  if (s1 == s2) return Val_int(0);
  len1 = caml_string_length(s1);
  len2 = caml_string_length(s2);
  res = memcmp(String_val(s1), String_val(s2), len1 <= len2 ? len1 : len2);
  if (res < 0) return Val_int(-1);
  if (res > 0) return Val_int(1);
  if (len1 < len2) return Val_int(-1);
  if (len1 > len2) return Val_int(1);
  return Val_int(0);
}

CAMLprim value caml_string_lessthan(value s1, value s2)
{
  return caml_string_compare(s1, s2) < Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_string_lessequal(value s1, value s2)
{
  return caml_string_compare(s1, s2) <= Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_string_greaterthan(value s1, value s2)
{
  return caml_string_compare(s1, s2) > Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_string_greaterequal(value s1, value s2)
{
  return caml_string_compare(s1, s2) >= Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_blit_string(value s1, value ofs1, value s2, value ofs2,
                                value n)
{
  memmove(&Byte(s2, Long_val(ofs2)), &Byte(s1, Long_val(ofs1)), Int_val(n));
  return Val_unit;
}

CAMLprim value caml_blit_bytes(value s1, value ofs1, value s2, value ofs2,
                                value n)
{
  memmove(&Byte(s2, Long_val(ofs2)), &Byte(s1, Long_val(ofs1)), Int_val(n));
  return Val_unit;
}


CAMLprim value caml_fill_string(value s, value offset, value len, value init)
{
  memset(&Byte(s, Long_val(offset)), Int_val(init), Long_val(len));
  return Val_unit;
}

CAMLprim value caml_is_printable(value chr)
{
  int c;

#ifdef HAS_LOCALE
  static int locale_is_set = 0;
  if (! locale_is_set) {
    setlocale(LC_CTYPE, "");
    locale_is_set = 1;
  }
#endif
  c = Int_val(chr);
  return Val_bool(isprint(c));
}

CAMLprim value caml_bitvect_test(value bv, value n)
{
  int pos = Int_val(n);
  return Val_int(Byte_u(bv, pos >> 3) & (1 << (pos & 7)));
}

CAMLexport value caml_alloc_sprintf(const char * format, ...)
{
  va_list args;
  char buf[64];
  int n;
  value res;

#ifndef _WIN32
  /* C99-compliant implementation */
  va_start(args, format);
  /* "vsnprintf(dest, sz, format, args)" writes at most "sz" characters
     into "dest", including the terminating '\0'.
     It returns the number of characters of the formatted string,
     excluding the terminating '\0'. */
  n = vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);
  /* Allocate a Caml string with length "n" as computed by vsnprintf. */
  res = caml_alloc_string(n);
  if (n < sizeof(buf)) {
    /* All output characters were written to buf, including the
       terminating '\0'.  Just copy them to the result. */
    memcpy(String_val(res), buf, n);
  } else {
    /* Re-do the formatting, outputting directly in the Caml string.
       Note that caml_alloc_string left room for a '\0' at position n,
       so the size passed to vsnprintf is n+1. */
    va_start(args, format);
    vsnprintf(String_val(res), n + 1, format, args);
    va_end(args);
  }
  return res;
#else
  /* Implementation specific to the Microsoft CRT library */
  va_start(args, format);
  /* "_vsnprintf(dest, sz, format, args)" writes at most "sz" characters
     into "dest".  Let "len" be the number of characters of the formatted
     string.
     If "len" < "sz", a null terminator was appended, and "len" is returned.
     If "len" == "sz", no null termination, and "len" is returned.
     If "len" > "sz", a negative value is returned. */
  n = _vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);
  if (n >= 0 && n <= sizeof(buf)) {
    /* All output characters were written to buf.
       "n" is the actual length of the output.
       Copy the characters to a Caml string of length n. */
    res = caml_alloc_string(n);
    memcpy(String_val(res), buf, n);
  } else {
    /* Determine actual length of output, excluding final '\0' */
    va_start(args, format);
    n = _vscprintf(format, args);
    va_end(args);
    res = caml_alloc_string(n);
    /* Re-do the formatting, outputting directly in the Caml string.
       Note that caml_alloc_string left room for a '\0' at position n,
       so the size passed to _vsnprintf is n+1. */
    va_start(args, format);
    _vsnprintf(String_val(res), n + 1, format, args);
    va_end(args);
  }
  return res;
#endif
}
