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

/**** Generic operations on digits ****/

/* These macros can be defined in the machine-specific include file.
   Below are the default definitions (in plain C).
   Except for BngMult, all macros are guaranteed to evaluate their
   arguments exactly once. */

#ifndef BngAdd2
/* res = arg1 + arg2.  carryout = carry out. */
#define BngAdd2(res,carryout,arg1,arg2) {                                   \
  bngdigit tmp1, tmp2;                                                      \
  tmp1 = arg1;                                                              \
  tmp2 = tmp1 + (arg2);                                                     \
  carryout = (tmp2 < tmp1);                                                 \
  res = tmp2;                                                               \
}
#endif

#ifndef BngAdd2Carry
/* res = arg1 + arg2 + carryin.  carryout = carry out. */
#define BngAdd2Carry(res,carryout,arg1,arg2,carryin) {                      \
  bngdigit tmp1, tmp2, tmp3;                                                \
  tmp1 = arg1;                                                              \
  tmp2 = tmp1 + (arg2);                                                     \
  tmp3 = tmp2 + (carryin);                                                  \
  carryout = (tmp2 < tmp1) + (tmp3 < tmp2);                                 \
  res = tmp3;                                                               \
}
#endif

#ifndef BngAdd3
/* res = arg1 + arg2 + arg3.  Each carry increments carryaccu. */
#define BngAdd3(res,carryaccu,arg1,arg2,arg3) {                             \
  bngdigit tmp1, tmp2, tmp3;                                                \
  tmp1 = arg1;                                                              \
  tmp2 = tmp1 + (arg2);                                                     \
  carryaccu += (tmp2 < tmp1);                                               \
  tmp3 = tmp2 + (arg3);                                                     \
  carryaccu += (tmp3 < tmp2);                                               \
  res = tmp3;                                                               \
}
#endif

#ifndef BngSub2
/* res = arg1 - arg2.  carryout = carry out. */
#define BngSub2(res,carryout,arg1,arg2) {                                   \
  bngdigit tmp1, tmp2;                                                      \
  tmp1 = arg1;                                                              \
  tmp2 = arg2;                                                              \
  res = tmp1 - tmp2;                                                        \
  carryout = (tmp1 < tmp2);                                                 \
}
#endif

#ifndef BngSub2Carry
/* res = arg1 - arg2 - carryin.  carryout = carry out. */
#define BngSub2Carry(res,carryout,arg1,arg2,carryin) {                      \
  bngdigit tmp1, tmp2, tmp3;                                                \
  tmp1 = arg1;                                                              \
  tmp2 = arg2;                                                              \
  tmp3 = tmp1 - tmp2;                                                       \
  res = tmp3 - (carryin);                                                   \
  carryout = (tmp1 < tmp2) + (tmp3 < carryin);                              \
}
#endif

#ifndef BngSub3
/* res = arg1 - arg2 - arg3.  Each carry increments carryaccu. */
#define BngSub3(res,carryaccu,arg1,arg2,arg3) {                             \
  bngdigit tmp1, tmp2, tmp3, tmp4;                                          \
  tmp1 = arg1;                                                              \
  tmp2 = arg2;                                                              \
  tmp3 = arg3;                                                              \
  tmp4 = tmp1 - tmp2;                                                       \
  res = tmp4 - tmp3;                                                        \
  carryaccu += (tmp1 < tmp2) + (tmp4 < tmp3);                               \
}
#endif

#define BngLowHalf(d) ((d) & (((bngdigit)1 << BNG_BITS_PER_HALF_DIGIT) - 1))
#define BngHighHalf(d) ((d) >> BNG_BITS_PER_HALF_DIGIT)

#ifndef BngMult
/* resl = low  digit of product arg1 * arg2
   resh = high digit of product arg1 * arg2. */
#if SIZEOF_PTR == 4 && defined(ARCH_UINT64_TYPE)
#define BngMult(resh,resl,arg1,arg2) {                                      \
  ARCH_UINT64_TYPE p = (ARCH_UINT64_TYPE)(arg1) * (ARCH_UINT64_TYPE)(arg2); \
  resh = p >> 32;                                                           \
  resl = p;                                                                 \
}
#else
#define BngMult(resh,resl,arg1,arg2) {                                      \
  bngdigit p11 = BngLowHalf(arg1) * BngLowHalf(arg2);                       \
  bngdigit p12 = BngLowHalf(arg1) * BngHighHalf(arg2);                      \
  bngdigit p21 = BngHighHalf(arg1) * BngLowHalf(arg2);                      \
  bngdigit p22 = BngHighHalf(arg1) * BngHighHalf(arg2);                     \
  resh = p22 + (p12 >> BNG_BITS_PER_HALF_DIGIT)                             \
             + (p21 >> BNG_BITS_PER_HALF_DIGIT);                            \
  BngAdd3(resl, resh,                                                       \
     p11, p12 << BNG_BITS_PER_HALF_DIGIT, p21 << BNG_BITS_PER_HALF_DIGIT);  \
}
#endif
#endif

#ifndef BngDiv
/* Divide the double-width number nh:nl by d.
   Require d != 0 and nh < d.
   Store quotient in quo, remainder in rem.
   Can be slow if d is not normalized. */
#define BngDiv(quo,rem,nh,nl,d) bng_div_aux(&(quo),&(rem),nh,nl,d)
#define BngDivNeedsNormalization

static void bng_div_aux(bngdigit * quo, bngdigit * rem,
                        bngdigit nh, bngdigit nl, bngdigit d)
{
  bngdigit dl, dh, ql, qh, pl, ph, nsaved;

  dl = BngLowHalf(d);
  dh = BngHighHalf(d);
  /* Under-estimate the top half of the quotient (qh) */
  qh = nh / (dh + 1);
  /* Shift nh:nl right by BNG_BITS_PER_HALF_DIGIT bits,
     so that we focus on the top 1.5 digits of the numerator.
     Then, subtract (qh * d) from nh:nl. */
  nsaved = BngLowHalf(nl);
  ph = qh * dh;
  pl = qh * dl;
  nh -= ph; /* Subtract before shifting so that carry propagates for free */
  nl = (nl >> BNG_BITS_PER_HALF_DIGIT) | (nh << BNG_BITS_PER_HALF_DIGIT);
  nh = (nh >> BNG_BITS_PER_HALF_DIGIT);
  nh -= (nl < pl);  /* Borrow */
  nl -= pl;
  /* Adjust estimate qh until nh:nl < 0:d */
  while (nh != 0 || nl >= d) {
    nh -= (nl < d); /* Borrow */
    nl -= d;
    qh++;
  }
  /* Under-estimate the bottom half of the quotient (ql) */
  ql = nl / (dh + 1);
  /* Shift nh:nl left by BNG_BITS_PER_HALF_DIGIT bits, restoring the
     low bits we saved earlier, so that we focus on the bottom 1.5 digit
     of the numerator.  Then, subtract (ql * d) from nh:nl. */
  ph = ql * dh;
  pl = ql * dl;
  nl -= ph; /* Subtract before shifting so that carry propagates for free */
  nh = (nl >> BNG_BITS_PER_HALF_DIGIT);
  nl = (nl << BNG_BITS_PER_HALF_DIGIT) | nsaved;
  nh -= (nl < pl);  /* Borrow */
  nl -= pl;
  /* Adjust estimate ql until nh:nl < 0:d */
  while (nh != 0 || nl >= d) {
    nh -= (nl < d); /* Borrow */
    nl -= d;
    ql++;
  }
  /* We're done */
  *quo = (qh << BNG_BITS_PER_HALF_DIGIT) | ql;
  *rem = nl;
}

#endif
