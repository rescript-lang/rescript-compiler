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

#include <string.h>
#include "caml/config.h"

typedef uintnat bngdigit;
typedef bngdigit * bng;
typedef unsigned int bngcarry;
typedef uintnat bngsize;

#define BNG_BITS_PER_DIGIT (sizeof(bngdigit) * 8)
#define BNG_BITS_PER_HALF_DIGIT (sizeof(bngdigit) * 4)

struct bng_operations {

  /* {a,alen} := {a, alen} + carry.  Return carry out. */
  bngcarry (*add_carry)
       (bng a/*[alen]*/,  bngsize alen, bngcarry carry);
#define bng_add_carry bng_ops.add_carry

  /* {a,alen} := {a,alen} + {b,blen} + carry.  Return carry out.
     Require alen >= blen. */
  bngcarry (*add)
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry);
#define bng_add bng_ops.add

  /* {a,alen} := {a, alen} - carry.  Return carry out. */
  bngcarry (*sub_carry)
       (bng a/*[alen]*/,  bngsize alen, bngcarry carry);
#define bng_sub_carry bng_ops.sub_carry

  /* {a,alen} := {a,alen} - {b,blen} - carry.  Return carry out.
     Require alen >= blen. */
  bngcarry (*sub)
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry);
#define bng_sub bng_ops.sub

  /* {a,alen} := {a,alen} << shift.
     Return the bits shifted out of the most significant digit of a.
     Require 0 <= shift < BITS_PER_BNGDIGIT. */
  bngdigit (*shift_left)
       (bng a/*[alen]*/, bngsize alen,
        int shift);
#define bng_shift_left bng_ops.shift_left

  /* {a,alen} := {a,alen} >> shift.
     Return the bits shifted out of the least significant digit of a.
     Require 0 <= shift < BITS_PER_BNGDIGIT. */
  bngdigit (*shift_right)
       (bng a/*[alen]*/, bngsize alen,
        int shift);
#define bng_shift_right bng_ops.shift_right

  /* {a,alen} := {a,alen} + d * {b,blen}.  Return carry out.
     Require alen >= blen.
     If alen > blen, the carry out returned is 0 or 1.
     If alen == blen, the carry out returned is a full digit. */
  bngdigit (*mult_add_digit)
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngdigit d);
#define bng_mult_add_digit bng_ops.mult_add_digit

  /* {a,alen} := {a,alen} - d * {b,blen}.  Return carry out.
     Require alen >= blen.
     If alen > blen, the carry out returned is 0 or 1.
     If alen == blen, the carry out returned is a full digit. */
  bngdigit (*mult_sub_digit)
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngdigit d);
#define bng_mult_sub_digit bng_ops.mult_sub_digit

  /* {a,alen} := {a,alen} + {b,blen} * {c,clen}.  Return carry out.
     Require alen >= blen + clen. */
  bngcarry (*mult_add)
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bng c/*[clen]*/, bngsize clen);
#define bng_mult_add bng_ops.mult_add

  /* {a,alen} := 2 * {a,alen} + {b,blen}^2.  Return carry out.
     Require alen >= 2 * blen. */
  bngcarry (*square_add)
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen);
#define bng_square_add bng_ops.square_add

  /* {a,len-1} := {b,len} / d.  Return {b,len} modulo d.
     Require d is normalized and MSD of b < d.
     See div_rem_digit for a function that does not require d
     to be normalized */
  bngdigit (*div_rem_norm_digit)
       (bng a/*[len-1]*/, bng b/*[len]*/, bngsize len, bngdigit d);
#define bng_div_rem_norm_digit bng_ops.div_rem_norm_digit

  /* {a,len-1} := {b,len} / d.  Return {b,len} modulo d.
     Require MSD of b < d. */
     bngdigit (*div_rem_digit)
       (bng a/*[len-1]*/, bng b/*[len]*/, bngsize len, bngdigit d);
#define bng_div_rem_digit bng_ops.div_rem_digit

  /* {n+dlen, nlen-dlen} := {n,nlen} / {d, dlen}.
     {n, dlen} := {n,nlen} modulo {d, dlen}.
     Require nlen > dlen and MSD of n < MSD of d (which implies d != 0). */
  void (*div_rem)
       (bng n/*[nlen]*/, bngsize nlen,
        bng d/*[nlen]*/, bngsize dlen);
#define bng_div_rem bng_ops.div_rem
};

extern struct bng_operations bng_ops;

/* Initialize the BNG library */
extern void bng_init(void);

/* {a,alen} := 0 */
#define bng_zero(a,alen) memset((a), 0, (alen) * sizeof(bngdigit))

/* {a,len} := {b,len} */
#define bng_assign(a,b,len) memmove((a), (b), (len) * sizeof(bngdigit))

/* Complement the digits of {a,len} */
extern void bng_complement(bng a/*[alen]*/, bngsize alen);

/* Return number of significant digits in {a,alen}. */
extern bngsize bng_num_digits(bng a/*[alen]*/, bngsize alen);

/* Return 1 if {a,alen} is 0, 0 otherwise. */
#define bng_is_zero(a,alen) (bng_num_digits(a,alen) == 0)

/* Return 0 if {a,alen} = {b,blen}
         <0 if {a,alen} < {b,blen}
         >0 if {a,alen} > {b,blen}. */
extern int bng_compare(bng a/*[alen]*/, bngsize alen,
                       bng b/*[blen]*/, bngsize blen);

/* Return the number of leading zero bits in digit d. */
extern int bng_leading_zero_bits(bngdigit d);
