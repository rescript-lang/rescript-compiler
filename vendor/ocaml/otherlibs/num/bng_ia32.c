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

/* Code specific to the Intel IA32 (x86) architecture. */

#define BngAdd2(res,carryout,arg1,arg2)                                     \
  asm("xorl %1, %1 \n\t"                                                    \
      "addl %3, %0 \n\t"                                                    \
      "setc %b1"                                                            \
      : "=r" (res), "=&q" (carryout)                                        \
      : "0" (arg1), "rm" (arg2))

#define BngSub2(res,carryout,arg1,arg2)                                     \
  asm("xorl %1, %1 \n\t"                                                    \
      "subl %3, %0 \n\t"                                                    \
      "setc %b1"                                                            \
      : "=r" (res), "=&q" (carryout)                                        \
      : "0" (arg1), "rm" (arg2))

#define BngMult(resh,resl,arg1,arg2)                                        \
  asm("mull %3"                                                             \
      : "=a" (resl), "=d" (resh)                                            \
      : "a" (arg1), "r" (arg2))

#define BngDiv(quo,rem,nh,nl,d)                                             \
  asm("divl %4"                                                             \
      : "=a" (quo), "=d" (rem)                                              \
      : "a" (nl), "d" (nh), "r" (d))

/* Reimplementation in asm of some of the bng operations. */

static bngcarry bng_ia32_add
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  bngdigit tmp;
  alen -= blen;
  if (blen > 0) {
    asm("negb %b3 \n\t"
        "1: \n\t"
        "movl (%0), %4 \n\t"
        "adcl (%1), %4 \n\t"
        "movl %4, (%0) \n\t"
        "leal 4(%0), %0 \n\t"
        "leal 4(%1), %1 \n\t"
        "decl %2 \n\t"
        "jnz 1b \n\t"
        "setc %b3"
        : "+&r" (a), "+&r" (b), "+&r" (blen), "+&q" (carry), "=&r" (tmp));
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if (++(*a) != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

static bngcarry bng_ia32_sub
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  bngdigit tmp;
  alen -= blen;
  if (blen > 0) {
    asm("negb %b3 \n\t"
        "1: \n\t"
        "movl (%0), %4 \n\t"
        "sbbl (%1), %4 \n\t"
        "movl %4, (%0) \n\t"
        "leal 4(%0), %0 \n\t"
        "leal 4(%1), %1 \n\t"
        "decl %2 \n\t"
        "jnz 1b \n\t"
        "setc %b3"
        : "+&r" (a), "+&r" (b), "+&r" (blen), "+&q" (carry), "=&r" (tmp));
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if ((*a)-- != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

static bngdigit bng_ia32_mult_add_digit
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
        "movl (%1), %%eax \n\t"
        "mull %4\n\t"           /* edx:eax = d * next digit of b */
        "addl (%0), %%eax \n\t" /* add next digit of a to eax */
        "adcl $0, %%edx \n\t"   /* accumulate carry in edx */
        "addl %3, %%eax \n\t"   /* add out to eax */
        "adcl $0, %%edx \n\t"   /* accumulate carry in edx */
        "movl %%eax, (%0) \n\t" /* eax is next digit of result */
        "movl %%edx, %3 \n\t"   /* edx is next out */
        "leal 4(%0), %0 \n\t"
        "leal 4(%1), %1 \n\t"
        "decl %2 \n\t"
        "jnz 1b"
        : "+&r" (a), "+&r" (b), "+&r" (blen), "=m" (out)
        : "m" (d)
        : "eax", "edx");
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

static bngdigit bng_ia32_mult_sub_digit
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
        "movl (%1), %%eax \n\t"
        "movl (%0), %4 \n\t"
        "mull %5\n\t"           /* edx:eax = d * next digit of b */
        "subl %%eax, %4 \n\t"   /* subtract eax from next digit of a */
        "adcl $0, %%edx \n\t"   /* accumulate carry in edx */
        "subl %3, %4 \n\t"      /* subtract out */
        "adcl $0, %%edx \n\t"   /* accumulate carry in edx */
        "movl %4, (%0) \n\t"    /* store next digit of result */
        "movl %%edx, %3 \n\t"   /* edx is next out */
        "leal 4(%0), %0 \n\t"
        "leal 4(%1), %1 \n\t"
        "decl %2 \n\t"
        "jnz 1b"
        : "+&r" (a), "+&r" (b), "=m" (blen), "=m" (out), "=&r" (tmp)
        : "m" (d)
        : "eax", "edx");
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

/* This is another asm implementation of some of the bng operations,
   using SSE2 operations to provide 64-bit arithmetic.
   This is faster than the plain IA32 code above on the Pentium 4.
   (Arithmetic operations with carry are slow on the Pentium 4). */

#if BNG_ASM_LEVEL >= 2

static bngcarry bng_ia32sse2_add
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  alen -= blen;
  if (blen > 0) {
    asm("movd %3, %%mm0 \n\t"       /* MM0 is carry */
        "1: \n\t"
        "movd (%0), %%mm1 \n\t"     /* MM1 is next digit of a */
        "movd (%1), %%mm2 \n\t"     /* MM2 is next digit of b */
        "paddq %%mm1, %%mm0 \n\t"   /* Add carry (64 bits) */
        "paddq %%mm2, %%mm0 \n\t"   /* Add digits (64 bits) */
        "movd %%mm0, (%0) \n\t"     /* Store low 32 bits of result */
        "psrlq $32, %%mm0 \n\t"     /* Next carry is top 32 bits of results */
        "addl $4, %0\n\t"
        "addl $4, %1\n\t"
        "subl $1, %2\n\t"
        "jne 1b \n\t"
        "movd %%mm0, %3 \n\t"
        "emms"
        : "+&r" (a), "+&r" (b), "+&r" (blen), "+&rm" (carry));
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if (++(*a) != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

static bngcarry bng_ia32sse2_sub
       (bng a/*[alen]*/, bngsize alen,
        bng b/*[blen]*/, bngsize blen,
        bngcarry carry)
{
  alen -= blen;
  if (blen > 0) {
    asm("movd %3, %%mm0 \n\t"       /* MM0 is carry */
        "1: \n\t"
        "movd (%0), %%mm1 \n\t"     /* MM1 is next digit of a */
        "movd (%1), %%mm2 \n\t"     /* MM2 is next digit of b */
        "psubq %%mm0, %%mm1 \n\t"   /* Subtract carry (64 bits) */
        "psubq %%mm2, %%mm1 \n\t"   /* Subtract digits (64 bits) */
        "movd %%mm1, (%0) \n\t"     /* Store low 32 bits of result */
        "psrlq $63, %%mm1 \n\t"     /* Next carry is sign bit of result */
        "movq %%mm1, %%mm0 \n\t"
        "addl $4, %0\n\t"
        "addl $4, %1\n\t"
        "subl $1, %2\n\t"
        "jne 1b \n\t"
        "movd %%mm0, %3 \n\t"
        "emms"
        : "+&r" (a), "+&r" (b), "+&r" (blen), "+&rm" (carry));
  }
  if (carry == 0 || alen == 0) return carry;
  do {
    if ((*a)-- != 0) return 0;
    a++;
  } while (--alen);
  return 1;
}

static bngdigit bng_ia32sse2_mult_add_digit
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen,
      bngdigit d)
{
  bngdigit out;
  bngcarry carry;

  alen -= blen;
  out = 0;
  if (blen > 0) {
    asm("pxor %%mm0, %%mm0 \n\t"      /* MM0 is carry */
        "movd %4, %%mm7 \n\t"         /* MM7 is digit d */
        "1: \n\t"
        "movd (%0), %%mm1 \n\t"       /* MM1 is next digit of a */
        "movd (%1), %%mm2 \n\t"       /* MM2 is next digit of b */
        "pmuludq %%mm7, %%mm2 \n\t"   /* MM2 = d * digit of b */
        "paddq %%mm1, %%mm0 \n\t"     /* Add product and carry ... */
        "paddq %%mm2, %%mm0 \n\t"     /* ... and digit of a */
        "movd %%mm0, (%0) \n\t"       /* Store low 32 bits of result */
        "psrlq $32, %%mm0 \n\t"       /* Next carry is high 32 bits result */
        "addl $4, %0\n\t"
        "addl $4, %1\n\t"
        "subl $1, %2\n\t"
        "jne 1b \n\t"
        "movd %%mm0, %3 \n\t"
        "emms"
        : "+&r" (a), "+&r" (b), "+&r" (blen), "=&rm" (out)
        : "m" (d));
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

static bngdigit bng_ia32sse2_mult_sub_digit
     (bng a/*[alen]*/, bngsize alen,
      bng b/*[blen]*/, bngsize blen,
      bngdigit d)
{
  static unsigned long long bias1 = 0xFFFFFFFF00000000ULL - 0xFFFFFFFFULL;
  static unsigned long bias2 = 0xFFFFFFFFUL;
  bngdigit out;
  bngcarry carry;

  alen -= blen;
  out = 0;
  if (blen > 0) {
    /* Carry C is represented by ENC(C) = 0xFFFFFFFF - C (one's complement) */
    asm("movd %6, %%mm0 \n\t"         /* MM0 is carry (initially 0xFFFFFFFF) */
        "movq %5, %%mm6 \n\t"         /* MM6 is magic constant bias1 */
        "movd %4, %%mm7 \n\t"         /* MM7 is digit d */
        "1: \n\t"
        "movd (%0), %%mm1 \n\t"       /* MM1 is next digit of a */
        "movd (%1), %%mm2 \n\t"       /* MM2 is next digit of b */
        "paddq %%mm6, %%mm1 \n\t"     /* bias digit of a */
        "pmuludq %%mm7, %%mm2 \n\t"   /* MM2 = d * digit of b */
        /* Compute
           digit of a + ENC(carry) + 0xFFFFFFFF00000000 - 0xFFFFFFFF - product
           = digit of a - carry + 0xFFFFFFFF00000000 - product
           = digit of a - carry - productlow + (ENC(nextcarry) << 32) */
        "psubq %%mm2, %%mm1 \n\t"
        "paddq %%mm1, %%mm0 \n\t"
        "movd %%mm0, (%0) \n\t"       /* Store low 32 bits of result */
        "psrlq $32, %%mm0 \n\t"       /* Next carry is 32 high bits of result */
        "addl $4, %0\n\t"
        "addl $4, %1\n\t"
        "subl $1, %2\n\t"
        "jne 1b \n\t"
        "movd %%mm0, %3 \n\t"
        "emms"
        : "+&r" (a), "+&r" (b), "+&r" (blen), "=&rm" (out)
        : "m" (d), "m" (bias1), "m" (bias2));
    out = ~out; /* Undo encoding on out digit */
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

/* Detect whether SSE2 instructions are supported */

static int bng_ia32_sse2_supported(void)
{
  unsigned int flags, newflags, max_id, capabilities;

#define EFLAG_CPUID 0x00200000
#define CPUID_IDENTIFY 0
#define CPUID_CAPABILITIES 1
#define SSE2_CAPABILITY 26

  /* Check if processor has CPUID instruction */
  asm("pushfl \n\t"
      "popl %0"
      : "=r" (flags) : );
  newflags = flags ^ EFLAG_CPUID;   /* CPUID detection flag */
  asm("pushfl \n\t"
      "pushl %1 \n\t"
      "popfl \n\t"
      "pushfl \n\t"
      "popl %0 \n\t"
      "popfl"
      : "=r" (flags) : "r" (newflags));
  /* If CPUID detection flag cannot be changed, CPUID instruction is not
     available */
  if ((flags & EFLAG_CPUID) != (newflags & EFLAG_CPUID)) return 0;
  /* See if SSE2 extensions are supported */
  asm("pushl %%ebx \n\t"        /* need to preserve %ebx for PIC */
      "cpuid \n\t"
      "popl %%ebx"
      : "=a" (max_id) : "a" (CPUID_IDENTIFY): "ecx", "edx");
  if (max_id < 1) return 0;
  asm("pushl %%ebx \n\t"
      "cpuid \n\t"
      "popl %%ebx"
      : "=d" (capabilities) : "a" (CPUID_CAPABILITIES) : "ecx");
  return capabilities & (1 << SSE2_CAPABILITY);
}

#endif

static void bng_ia32_setup_ops(void)
{
#if BNG_ASM_LEVEL >= 2
  if (bng_ia32_sse2_supported()) {
    bng_ops.add = bng_ia32sse2_add;
    bng_ops.sub = bng_ia32sse2_sub;
    bng_ops.mult_add_digit = bng_ia32sse2_mult_add_digit;
    bng_ops.mult_sub_digit = bng_ia32sse2_mult_sub_digit;
    return;
  }
#endif
  bng_ops.add = bng_ia32_add;
  bng_ops.sub = bng_ia32_sub;
  bng_ops.mult_add_digit = bng_ia32_mult_add_digit;
  bng_ops.mult_sub_digit = bng_ia32_mult_sub_digit;
}

#define BNG_SETUP_OPS bng_ia32_setup_ops()
