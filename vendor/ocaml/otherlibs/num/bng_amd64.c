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

/* Code specific to the AMD x86_64 architecture. */

#define BngAdd2(res,carryout,arg1,arg2)                                     \
  asm("xorl %1, %1 \n\t"                                                    \
      "addq %3, %0 \n\t"                                                    \
      "setc %b1"                                                            \
      : "=r" (res), "=&q" (carryout)                                        \
      : "0" (arg1), "rm" (arg2))

#define BngSub2(res,carryout,arg1,arg2)                                     \
  asm("xorl %1, %1 \n\t"                                                    \
      "subq %3, %0 \n\t"                                                    \
      "setc %b1"                                                            \
      : "=r" (res), "=&q" (carryout)                                        \
      : "0" (arg1), "rm" (arg2))

#define BngMult(resh,resl,arg1,arg2)                                        \
  asm("mulq %3"                                                             \
      : "=a" (resl), "=d" (resh)                                            \
      : "a" (arg1), "r" (arg2))

#define BngDiv(quo,rem,nh,nl,d)                                             \
  asm("divq %4"                                                             \
      : "=a" (quo), "=d" (rem)                                              \
      : "a" (nl), "d" (nh), "r" (d))

/* Reimplementation in asm of some of the bng operations. */

static bngcarry bng_amd64_add
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  bngdigit tmp;
  alen -= blen;
  if (blen > 0) {
    asm("negb %b3 \n\t"
        "1: \n\t"
        "movq (%0), %4 \n\t"
        "adcq (%1), %4 \n\t"
        "movq %4, (%0) \n\t"
        "leaq 8(%0), %0 \n\t"
        "leaq 8(%1), %1 \n\t"
        "decq %2 \n\t"
        "jnz 1b \n\t"
        "setc %b3"
        : "=r" (a), "=r" (b), "=r" (blen), "=q" (carry), "=r" (tmp)
        : "0" (a), "1" (b), "2" (blen), "3" (carry));
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if (++(*a) != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

static bngcarry bng_amd64_sub
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  bngdigit tmp;
  alen -= blen;
  if (blen > 0) {
    asm("negb %b3 \n\t"
        "1: \n\t"
        "movq (%0), %4 \n\t"
        "sbbq (%1), %4 \n\t"
        "movq %4, (%0) \n\t"
        "leaq 8(%0), %0 \n\t"
        "leaq 8(%1), %1 \n\t"
        "decq %2 \n\t"
        "jnz 1b \n\t"
        "setc %b3"
        : "=r" (a), "=r" (b), "=r" (blen), "=q" (carry), "=r" (tmp)
        : "0" (a), "1" (b), "2" (blen), "3" (carry));
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if ((*a)-- != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

static bngdigit bng_amd64_mult_add_digit
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen,
      bngdigit d)
{
  bngdigit out;
  bngcarry carry;

  alen -= blen;
  out = 0;
  if (blen > 0) {
    asm("1: \n\t"
        "movq (%1), %%rax \n\t"
        "mulq %7\n\t"           /* rdx:rax = d * next digit of b */
        "addq (%0), %%rax \n\t" /* add next digit of a to rax */
        "adcq $0, %%rdx \n\t"   /* accumulate carry in rdx */
        "addq %3, %%rax \n\t"   /* add out to rax */
        "adcq $0, %%rdx \n\t"   /* accumulate carry in rdx */
        "movq %%rax, (%0) \n\t" /* rax is next digit of result */
        "movq %%rdx, %3 \n\t"   /* rdx is next out */
        "leaq 8(%0), %0 \n\t"
        "leaq 8(%1), %1 \n\t"
        "decq %2 \n\t"
        "jnz 1b"
        : "=&r" (a), "=&r" (b), "=&r" (blen), "=&r" (out)
        : "0" (a), "1" (b), "2" (blen), "rm" (d), "3" (out)
        : "rax", "rdx");
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

static bngdigit bng_amd64_mult_sub_digit
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen,
      bngdigit d)
{
  bngdigit out, tmp;
  bngcarry carry;

  alen -= blen;
  out = 0;
  if (blen > 0) {
    asm("1: \n\t"
        "movq (%1), %%rax \n\t"
        "movq (%0), %4 \n\t"
        "mulq %8\n\t"           /* rdx:rax = d * next digit of b */
        "subq %%rax, %4 \n\t"   /* subtract rax from next digit of a */
        "adcq $0, %%rdx \n\t"   /* accumulate carry in rdx */
        "subq %3, %4 \n\t"      /* subtract out */
        "adcq $0, %%rdx \n\t"   /* accumulate carry in rdx */
        "movq %4, (%0) \n\t"    /* store next digit of result */
        "movq %%rdx, %3 \n\t"   /* rdx is next out */
        "leaq 8(%0), %0 \n\t"
        "leaq 8(%1), %1 \n\t"
        "decq %2 \n\t"
        "jnz 1b"
        : "=&r" (a), "=&r" (b), "=&rm" (blen), "=&r" (out), "=&r" (tmp)
        : "0" (a), "1" (b), "2" (blen), "rm" (d), "3" (out)
        : "rax", "rdx");
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

static void bng_amd64_setup_ops(void)
{
  bng_ops.add = bng_amd64_add;
  bng_ops.sub = bng_amd64_sub;
  bng_ops.mult_add_digit = bng_amd64_mult_add_digit;
  bng_ops.mult_sub_digit = bng_amd64_mult_sub_digit;
}

#define BNG_SETUP_OPS bng_amd64_setup_ops()
