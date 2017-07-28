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

/* printf-like formatting of 64-bit integers, in case the C library
   printf() function does not support them. */

#ifndef CAML_INT64_FORMAT_H
#define CAML_INT64_FORMAT_H

static void I64_format(char * buffer, char * fmt, int64 x)
{
  static char conv_lower[] = "0123456789abcdef";
  static char conv_upper[] = "0123456789ABCDEF";
  char rawbuffer[24];
  char justify, signstyle, filler, alternate, signedconv;
  int base, width, sign, i, rawlen;
  char * cvtbl;
  char * p, * r;
  int64 wbase, digit;

  /* Parsing of format */
  justify = '+';
  signstyle = '-';
  filler = ' ';
  alternate = 0;
  base = 0;
  signedconv = 0;
  width = 0;
  cvtbl = conv_lower;
  for (p = fmt; *p != 0; p++) {
    switch (*p) {
    case '-':
      justify = '-'; break;
    case '+': case ' ':
      signstyle = *p; break;
    case '0':
      filler = '0'; break;
    case '#':
      alternate = 1; break;
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      width = atoi(p);
      while (p[1] >= '0' && p[1] <= '9') p++;
      break;
    case 'd': case 'i':
      signedconv = 1; /* fallthrough */
    case 'u':
      base = 10; break;
    case 'x':
      base = 16; break;
    case 'X':
      base = 16; cvtbl = conv_upper; break;
    case 'o':
      base = 8; break;
    }
  }
  if (base == 0) { buffer[0] = 0; return; }
  /* Do the conversion */
  sign = 1;
  if (signedconv && I64_is_negative(x)) { sign = -1; x = I64_neg(x); }
  r = rawbuffer + sizeof(rawbuffer);
  wbase = I64_of_int32(base);
  do {
    I64_udivmod(x, wbase, &x, &digit);
    *--r = cvtbl[I64_to_int32(digit)];
  } while (! I64_is_zero(x));
  rawlen = rawbuffer + sizeof(rawbuffer) - r;
  /* Adjust rawlen to reflect additional chars (sign, etc) */
  if (signedconv && (sign < 0 || signstyle != '-')) rawlen++;
  if (alternate) {
    if (base == 8) rawlen += 1;
    if (base == 16) rawlen += 2;
  }
  /* Do the formatting */
  p = buffer;
  if (justify == '+' && filler == ' ') {
    for (i = rawlen; i < width; i++) *p++ = ' ';
  }
  if (signedconv) {
    if (sign < 0) *p++ = '-';
    else if (signstyle != '-') *p++ = signstyle;
  }
  if (alternate && base == 8) *p++ = '0';
  if (alternate && base == 16) { *p++ = '0'; *p++ = 'x'; }
  if (justify == '+' && filler == '0') {
    for (i = rawlen; i < width; i++) *p++ = '0';
  }
  while (r < rawbuffer + sizeof(rawbuffer)) *p++ = *r++;
  if (justify == '-') {
    for (i = rawlen; i < width; i++) *p++ = ' ';
  }
  *p = 0;
}

#endif /* CAML_INT64_FORMAT_H */
