/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "bng.h"
#include "caml/config.h"

#if defined(__GNUC__) && BNG_ASM_LEVEL > 0
#if defined(BNG_ARCH_ia32)
#include "bng_ia32.c"
#elif defined(BNG_ARCH_amd64)
#include "bng_amd64.c"
#elif defined(BNG_ARCH_ppc)
#include "bng_ppc.c"
#elif defined (BNG_ARCH_sparc)
#include "bng_sparc.c"
#elif defined (BNG_ARCH_arm64)
#include "bng_arm64.c"
#endif
#endif

#include "bng_digit.c"

/**** Operations that cannot be overridden ****/

/* Return number of leading zero bits in d */
int bng_leading_zero_bits(bngdigit d)
{
  int n = BNG_BITS_PER_DIGIT;
#ifdef ARCH_SIXTYFOUR
  if ((d & 0xFFFFFFFF00000000L) != 0) { n -= 32; d = d >> 32; }
#endif
  if ((d & 0xFFFF0000) != 0) { n -= 16; d = d >> 16; }
  if ((d & 0xFF00) != 0) { n -= 8; d = d >> 8; }
  if ((d & 0xF0) != 0) { n -= 4; d = d >> 4; }
  if ((d & 0xC) != 0) { n -= 2; d = d >> 2; }
  if ((d & 2) != 0) { n -= 1; d = d >> 1; }
  return n - d;
}

/* Complement the digits of {a,len} */
void bng_complement(bng a/*[alen]*/, bngsize alen)
{
  for (/**/; alen > 0; alen--, a++) *a = ~*a;
}

/* Return number of significant digits in {a,alen}. */
bngsize bng_num_digits(bng a/*[alen]*/, bngsize alen)
{
  while (1) {
    if (alen == 0) return 1;
    if (a[alen - 1] != 0) return alen;
    alen--;
  }
}

/* Return 0 if {a,alen} = {b,blen}
         -1 if {a,alen} < {b,blen}
          1 if {a,alen} > {b,blen}. */
int bng_compare(bng a/*[alen]*/, bngsize alen,
                bng b/*[blen]*/, bngsize blen)
{
  bngdigit da, db;

  while (alen > 0 && a[alen-1] == 0) alen--;
  while (blen > 0 && b[blen-1] == 0) blen--;
  if (alen > blen) return 1;
  if (alen < blen) return -1;
  while (alen > 0) {
    alen--;
    da = a[alen];
    db = b[alen];
    if (da > db) return 1;
    if (da < db) return -1;
  }
  return 0;
}

/**** Generic definitions of the overridable operations ****/

/* {a,alen} := {a, alen} + carry.  Return carry out. */
static bngcarry bng_generic_add_carry
       (bng a/*[alen]*/,  bngsize alen, bngcarry carry)
{
  if (carry == 0 || alen == 0) return carry;
  do {
    if (++(*a) != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

/* {a,alen} := {a,alen} + {b,blen} + carry.  Return carry out.
   Require alen >= blen. */
static bngcarry bng_generic_add
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  alen -= blen;
  for (/**/; blen > 0; blen--, a++, b++) {
    BngAdd2Carry(*a, carry, *a, *b, carry);
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if (++(*a) != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

/* {a,alen} := {a, alen} - carry.  Return carry out. */
static bngcarry bng_generic_sub_carry
       (bng a/*[alen]*/,  bngsize alen, bngcarry carry)
{
  if (carry == 0 || alen == 0) return carry;
  do {
    if ((*a)-- != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

/* {a,alen} := {a,alen} - {b,blen} - carry.  Return carry out.
   Require alen >= blen. */
static bngcarry bng_generic_sub
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  alen -= blen;
  for (/**/; blen > 0; blen--, a++, b++) {
    BngSub2Carry(*a, carry, *a, *b, carry);
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if ((*a)-- != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

/* {a,alen} := {a,alen} << shift.
   Return the bits shifted out of the most significant digit of a.
   Require 0 <= shift < BITS_PER_BNGDIGIT. */
static bngdigit bng_generic_shift_left
     (bng a/*[alen]*/, bngsize alen,
      int shift)
{
  int shift2 = BNG_BITS_PER_DIGIT - shift;
  bngdigit carry = 0;
  if (shift > 0) {
    for (/**/; alen > 0; alen--, a++) {
      bngdigit d = *a;
      *a = (d << shift) | carry;
      carry = d >> shift2;
    }
  }
  return carry;
}

/* {a,alen} := {a,alen} >> shift.
   Return the bits shifted out of the least significant digit of a.
   Require 0 <= shift < BITS_PER_BNGDIGIT. */
static bngdigit bng_generic_shift_right
     (bng a/*[alen]*/, bngsize alen,
      int shift)
{
  int shift2 = BNG_BITS_PER_DIGIT - shift;
  bngdigit carry = 0;
  if (shift > 0) {
    for (a = a + alen - 1; alen > 0; alen--, a--) {
      bngdigit d = *a;
      *a = (d >> shift) | carry;
      carry = d << shift2;
    }
  }
  return carry;
}

/* {a,alen} := {a,alen} + d * {b,blen}.  Return carry out.
   Require alen >= blen. */
static bngdigit bng_generic_mult_add_digit
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen,
      bngdigit d)
{
  bngdigit out, ph, pl;
  bngcarry carry;

  alen -= blen;
  for (out = 0; blen > 0; blen--, a++, b++) {
    bngdigit bd = *b;
    /* ph:pl = double-digit product of b's current digit and d */
    BngMult(ph, pl, bd, d);
    /* current digit of a += pl + out.  Accumulate carries in ph. */
    BngAdd3(*a, ph, *a, pl, out);
    /* prepare out for next iteration */
    out = ph;
  }
  if (alen == 0) return out;
  /* current digit of a += out */
  BngAdd2(*a, carry, *a, out);
  a++;
  alen--;
  /* Propagate carry */
  if (carry == 0 || alen == 0) return carry;
  do {
    if (++(*a) != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

/* {a,alen} := {a,alen} - d * {b,blen}.  Return carry out.
   Require alen >= blen. */
static bngdigit bng_generic_mult_sub_digit
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen,
      bngdigit d)
{
  bngdigit out, ph, pl;
  bngcarry carry;

  alen -= blen;
  for (out = 0; blen > 0; blen--, a++, b++) {
    bngdigit bd = *b;
    /* ph:pl = double-digit product of b's current digit and d */
    BngMult(ph, pl, bd, d);
    /* current digit of a -= pl + out.  Accumulate carrys in ph. */
    BngSub3(*a, ph, *a, pl, out);
    /* prepare out for next iteration */
    out = ph;
  }
  if (alen == 0) return out;
  /* current digit of a -= out */
  BngSub2(*a, carry, *a, out);
  a++;
  alen--;
  /* Propagate carry */
  if (carry == 0 || alen == 0) return carry;
  do {
    if ((*a)-- != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

/* {a,alen} := {a,alen} + {b,blen} * {c,clen}.  Return carry out.
   Require alen >= blen + clen. */
static bngcarry bng_generic_mult_add
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen,
      bng c/*[clen]*/, bngsize clen)
{
  bngcarry carry;
  for (carry = 0; clen > 0; clen--, c++, alen--, a++)
    carry += bng_mult_add_digit(a, alen, b, blen, *c);
  return carry;
}

/* {a,alen} := 2 * {a,alen} + {b,blen}^2.  Return carry out.
   Require alen >= 2 * blen. */
static bngcarry bng_generic_square_add
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen)
{
  bngcarry carry1, carry2;
  bngsize i, aofs;
  bngdigit ph, pl, d;

  /* Double products */
  for (carry1 = 0, i = 1; i < blen; i++) {
    aofs = 2 * i - 1;
    carry1 += bng_mult_add_digit(a + aofs, alen - aofs,
                                 b + i, blen - i, b[i - 1]);
  }
  /* Multiply by two */
  carry1 = (carry1 << 1) | bng_shift_left(a, alen, 1);
  /* Add square of digits */
  carry2 = 0;
  for (i = 0; i < blen; i++) {
    d = b[i];
    BngMult(ph, pl, d, d);
    BngAdd2Carry(*a, carry2, *a, pl, carry2);
    a++;
    BngAdd2Carry(*a, carry2, *a, ph, carry2);
    a++;
  }
  alen -= 2 * blen;
  if (alen > 0 && carry2 != 0) {
    do {
      if (++(*a) != 0) { carry2 = 0; break; }
      a++;
    } while (--alen);
  }
  return carry1 + carry2;
}

/* {a,len-1} := {b,len} / d.  Return {b,len} modulo d.
   Require MSD of b < d.
   If BngDivNeedsNormalization is defined, require d normalized. */
static bngdigit bng_generic_div_rem_norm_digit
     (bng a/*[len-1]*/, bng b/*[len]*/, bngsize len, bngdigit d)
{
  bngdigit topdigit, quo, rem;
  intnat i;

  topdigit = b[len - 1];
  for (i = len - 2; i >= 0; i--) {
    /* Divide topdigit:current digit of numerator by d */
    BngDiv(quo, rem, topdigit, b[i], d);
    /* Quotient is current digit of result */
    a[i] = quo;
    /* Iterate with topdigit = remainder */
    topdigit = rem;
  }
  return topdigit;
}

#ifdef BngDivNeedsNormalization
/* {a,len-1} := {b,len} / d.  Return {b,len} modulo d.
   Require MSD of b < d. */
static bngdigit bng_generic_div_rem_digit
     (bng a/*[len-1]*/, bng b/*[len]*/, bngsize len, bngdigit d)
{
  bngdigit rem;
  int shift;

  /* Normalize d and b */
  shift = bng_leading_zero_bits(d);
  d <<= shift;
  bng_shift_left(b, len, shift);
  /* Do the division */
  rem = bng_div_rem_norm_digit(a, b, len, d);
  /* Undo normalization on b and remainder */
  bng_shift_right(b, len, shift);
  return rem >> shift;
}
#endif

/* {n+dlen, nlen-dlen} := {n,nlen} / {d, dlen}.
   {n, dlen} := {n,nlen} modulo {d, dlen}.
   Require nlen > dlen and MSD of n < MSD of d.
   (This implies MSD of d > 0). */
static void bng_generic_div_rem
       (bng n/*[nlen]*/, bngsize nlen,
        bng d/*[dlen]*/, bngsize dlen)
{
  bngdigit topden, quo, rem;
  int shift;
  bngsize i, j;

  /* Normalize d */
  shift = bng_leading_zero_bits(d[dlen - 1]);
  /* Note that no bits of n are lost by the following shift,
     since n[nlen-1] < d[dlen-1] */
  bng_shift_left(n, nlen, shift);
  bng_shift_left(d, dlen, shift);
  /* Special case if d is just one digit */
  if (dlen == 1) {
    *n = bng_div_rem_norm_digit(n + 1, n, nlen, *d);
  } else {
    topden = d[dlen - 1];
    /* Long division */
    for (j = nlen - 1; j >= dlen; j--) {
      i = j - dlen;
      /* At this point:
         - the current numerator is      n[j] : ...................... : n[0]
         - to be subtracted quo times:   d[dlen-1] : ... : d[0] : 0... : 0
         (there are i zeroes at the end) */
      /* Under-estimate the next digit of the quotient (quo) */
      if (topden + 1 == 0)
        quo = n[j];
      else
        BngDiv(quo, rem, n[j], n[j - 1], topden + 1);
      /* Subtract d * quo (shifted i places) from numerator */
      n[j] -= bng_mult_sub_digit(n + i, dlen, d, dlen, quo);
      /* Adjust if necessary */
      while (n[j] != 0 || bng_compare(n + i, dlen, d, dlen) >= 0) {
        /* Numerator is still bigger than shifted divisor.
           Increment quotient and subtract shifted divisor. */
        quo++;
        n[j] -= bng_sub(n + i, dlen, d, dlen, 0);
      }
      /* Store quotient digit */
      n[j] = quo;
    }
  }
  /* Undo normalization on remainder and divisor */
  bng_shift_right(n, dlen, shift);
  bng_shift_right(d, dlen, shift);
}

/**** Construction of the table of operations ****/

struct bng_operations bng_ops = {
  bng_generic_add_carry,
  bng_generic_add,
  bng_generic_sub_carry,
  bng_generic_sub,
  bng_generic_shift_left,
  bng_generic_shift_right,
  bng_generic_mult_add_digit,
  bng_generic_mult_sub_digit,
  bng_generic_mult_add,
  bng_generic_square_add,
  bng_generic_div_rem_norm_digit,
#ifdef BngDivNeedsNormalization
  bng_generic_div_rem_digit,
#else
  bng_generic_div_rem_norm_digit,
#endif
  bng_generic_div_rem
};

void bng_init(void)
{
#ifdef BNG_SETUP_OPS
  BNG_SETUP_OPS;
#endif
}
