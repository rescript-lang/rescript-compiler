/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

#include "../../../byterun/caml/config.h"
#define FMT ARCH_INTNAT_PRINTF_FORMAT

void caml_ml_array_bound_error(void)
{
  fprintf(stderr, "Fatal error: out-of-bound access in array or string\n");
  exit(2);
}

intnat R[200];
double D[40];
intnat X, Y;
double F, G;

#define INTTEST(arg,res) \
  { intnat result = (res); \
    if (arg != result) \
      printf("Failed test \"%s == %s\" for X=%"FMT"d and Y=%"FMT"d: " \
             "result %"FMT"d, expected %"FMT"d\n",                    \
             #arg, #res, X, Y, arg, result); \
  }
#define INTFLOATTEST(arg,res) \
  { intnat result = (res); \
    if (arg != result) \
      printf("Failed test \"%s == %s\" for F=%.15g and G=%.15g: "\
             "result %"FMT"d, expected %"FMT"d\n",               \
             #arg, #res, F, G, arg, result); \
  }
#define FLOATTEST(arg,res) \
  { double result = (res); \
    if (arg < result || arg > result) \
      printf("Failed test \"%s == %s\" for F=%.15g and G=%.15g: "\
             "result %.15g, expected %.15g\n",                   \
             #arg, #res, F, G, arg, result); \
  }
#define FLOATINTTEST(arg,res) \
  { double result = (res); \
    if (arg < result || arg > result) \
      printf("Failed test \"%s == %s\" for X=%"FMT"d and Y=%"FMT"d: "\
             "result %.15g, expected %.15g\n",                       \
             #arg, #res, X, Y, arg, result); \
  }

extern void call_gen_code();
extern void testarith();

void do_test(void)
{
      call_gen_code(testarith);

      INTTEST(R[0], 0);
      INTTEST(R[1], 1);
      INTTEST(R[2], -1);
      INTTEST(R[3], 256);
      INTTEST(R[4], 65536);
      INTTEST(R[5], 16777216);
      INTTEST(R[6], -256);
      INTTEST(R[7], -65536);
      INTTEST(R[8], -16777216);

      INTTEST(R[9], (X + Y));
      INTTEST(R[10], (X + 1));
      INTTEST(R[11], (X + -1));

      INTTEST(R[12], ((intnat) ((char *)R + 8)));
      INTTEST(R[13], ((intnat) ((char *)R + Y)));

      INTTEST(R[14], (X - Y));
      INTTEST(R[15], (X - 1));
      INTTEST(R[16], (X - -1));

      INTTEST(R[17], ((intnat) ((char *)R - 8)));
      INTTEST(R[18], ((intnat) ((char *)R - Y)));

      INTTEST(R[19], (X * 2));
      INTTEST(R[20], (2 * X));
      INTTEST(R[21], (X * 16));
      INTTEST(R[22], (16 * X));
      INTTEST(R[23], (X * 12345));
      INTTEST(R[24], (12345 * X));
      INTTEST(R[25], (X * Y));

      INTTEST(R[26], (X / 2));
      INTTEST(R[27], (X / 16));
      INTTEST(R[28], (X / 7));
      INTTEST(R[29], (Y != 0 ? X / Y : 0));

      INTTEST(R[30], (X % 2));
      INTTEST(R[31], (X % 16));
      INTTEST(R[32], (Y != 0 ? X % Y : 0));

      INTTEST(R[33], (X & Y));
      INTTEST(R[34], (X & 3));
      INTTEST(R[35], (3 & X));

      INTTEST(R[36], (X | Y));
      INTTEST(R[37], (X | 3));
      INTTEST(R[38], (3 | X));

      INTTEST(R[39], (X ^ Y));
      INTTEST(R[40], (X ^ 3));
      INTTEST(R[41], (3 ^ X));

      INTTEST(R[42], (X << Y));
      INTTEST(R[43], (X << 1));
      INTTEST(R[44], (X << 8));

      INTTEST(R[45], ((uintnat) X >> Y));
      INTTEST(R[46], ((uintnat) X >> 1));
      INTTEST(R[47], ((uintnat) X >> 8));

      INTTEST(R[48], (X >> Y));
      INTTEST(R[49], (X >> 1));
      INTTEST(R[50], (X >> 8));

      INTTEST(R[51], (X == Y));
      INTTEST(R[52], (X != Y));
      INTTEST(R[53], (X < Y));
      INTTEST(R[54], (X > Y));
      INTTEST(R[55], (X <= Y));
      INTTEST(R[56], (X >= Y));
      INTTEST(R[57], (X == 1));
      INTTEST(R[58], (X != 1));
      INTTEST(R[59], (X < 1));
      INTTEST(R[60], (X > 1));
      INTTEST(R[61], (X <= 1));
      INTTEST(R[62], (X >= 1));

      INTTEST(R[63], ((char *)X == (char *)Y));
      INTTEST(R[64], ((char *)X != (char *)Y));
      INTTEST(R[65], ((char *)X < (char *)Y));
      INTTEST(R[66], ((char *)X > (char *)Y));
      INTTEST(R[67], ((char *)X <= (char *)Y));
      INTTEST(R[68], ((char *)X >= (char *)Y));
      INTTEST(R[69], ((char *)X == (char *)1));
      INTTEST(R[70], ((char *)X != (char *)1));
      INTTEST(R[71], ((char *)X < (char *)1));
      INTTEST(R[72], ((char *)X > (char *)1));
      INTTEST(R[73], ((char *)X <= (char *)1));
      INTTEST(R[74], ((char *)X >= (char *)1));

      INTTEST(R[75], (X + (Y << 1)));
      INTTEST(R[76], (X + (Y << 2)));
      INTTEST(R[77], (X + (Y << 3)));
      INTTEST(R[78], (X - (Y << 1)));
      INTTEST(R[79], (X - (Y << 2)));
      INTTEST(R[80], (X - (Y << 3)));

      FLOATTEST(D[0], 0.0);
      FLOATTEST(D[1], 1.0);
      FLOATTEST(D[2], -1.0);
      FLOATTEST(D[3], (F + G));
      FLOATTEST(D[4], (F - G));
      FLOATTEST(D[5], (F * G));
      FLOATTEST(D[6], F / G);

      FLOATTEST(D[7], (F + (G + 1.0)));
      FLOATTEST(D[8], (F - (G + 1.0)));
      FLOATTEST(D[9], (F * (G + 1.0)));
      FLOATTEST(D[10], F / (G + 1.0));

      FLOATTEST(D[11], ((F + 1.0) + G));
      FLOATTEST(D[12], ((F + 1.0) - G));
      FLOATTEST(D[13], ((F + 1.0) * G));
      FLOATTEST(D[14], (F + 1.0) / G);

      FLOATTEST(D[15], ((F + 1.0) + (G + 1.0)));
      FLOATTEST(D[16], ((F + 1.0) - (G + 1.0)));
      FLOATTEST(D[17], ((F + 1.0) * (G + 1.0)));
      FLOATTEST(D[18], (F + 1.0) / (G + 1.0));

      INTFLOATTEST(R[81], (F == G));
      INTFLOATTEST(R[82], (F != G));
      INTFLOATTEST(R[83], (F < G));
      INTFLOATTEST(R[84], (F > G));
      INTFLOATTEST(R[85], (F <= G));
      INTFLOATTEST(R[86], (F >= G));

      FLOATINTTEST(D[19], (double) X);
      INTFLOATTEST(R[87], (intnat) F);

      INTTEST(R[88], (X >= 0) && (X < Y));
      INTTEST(R[89], (0 < Y));
      INTTEST(R[90], (5 < Y));

      INTFLOATTEST(R[91], (F == G));
      INTFLOATTEST(R[92], (F != G));
      INTFLOATTEST(R[93], (F < G));
      INTFLOATTEST(R[94], (F > G));
      INTFLOATTEST(R[95], (F <= G));
      INTFLOATTEST(R[96], (F >= G));

      INTFLOATTEST(R[97], (F + 1.0 == G + 1.0));
      INTFLOATTEST(R[98], (F + 1.0 != G + 1.0));
      INTFLOATTEST(R[99], (F + 1.0 < G + 1.0));
      INTFLOATTEST(R[100], (F + 1.0 > G + 1.0));
      INTFLOATTEST(R[101], (F + 1.0 <= G + 1.0));
      INTFLOATTEST(R[102], (F + 1.0 >= G + 1.0));

      INTFLOATTEST(R[103], (F == G + 1.0));
      INTFLOATTEST(R[104], (F != G + 1.0));
      INTFLOATTEST(R[105], (F < G + 1.0));
      INTFLOATTEST(R[106], (F > G + 1.0));
      INTFLOATTEST(R[107], (F <= G + 1.0));
      INTFLOATTEST(R[108], (F >= G + 1.0));

      INTFLOATTEST(R[109], (F + 1.0 == G));
      INTFLOATTEST(R[110], (F + 1.0 != G));
      INTFLOATTEST(R[111], (F + 1.0 < G));
      INTFLOATTEST(R[112], (F + 1.0 > G));
      INTFLOATTEST(R[113], (F + 1.0 <= G));
      INTFLOATTEST(R[114], (F + 1.0 >= G));

      FLOATINTTEST(D[20], ((double) X) + 1.0);
      INTFLOATTEST(R[115], (intnat)(F + 1.0));

      FLOATTEST(D[21], F + G);
      FLOATTEST(D[22], G + F);
      FLOATTEST(D[23], F - G);
      FLOATTEST(D[24], G - F);
      FLOATTEST(D[25], F * G);
      FLOATTEST(D[26], G * F);
      FLOATTEST(D[27], F / G);
      FLOATTEST(D[28], G / F);

      FLOATTEST(D[29], (F * 2.0) + G);
      FLOATTEST(D[30], G + (F * 2.0));
      FLOATTEST(D[31], (F * 2.0) - G);
      FLOATTEST(D[32], G - (F * 2.0));
      FLOATTEST(D[33], (F + 2.0) * G);
      FLOATTEST(D[34], G * (F + 2.0));
      FLOATTEST(D[35], (F * 2.0) / G);
      FLOATTEST(D[36], G / (F * 2.0));

      FLOATTEST(D[37], - F);
      FLOATTEST(D[38], fabs(F));
}

#ifdef __i386__
#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif
#endif

void init_ieee_floats(void)
{
#ifdef __i386__
#ifdef __FreeBSD__
  fpsetmask(0);
#endif
#endif
}

int main(int argc, char **argv)
{
  double weird[4];

  init_ieee_floats();

  if (argc >= 5) {
    X = atoi(argv[1]);
    Y = atoi(argv[2]);
    sscanf(argv[3], "%lf", &F);
    sscanf(argv[4], "%lf", &G);
    do_test();
    return 0;
  }
  for(Y = -2; Y <= 2; Y++) {
    for (X = -2; X <= 2; X++) {
      F = X; G = Y; do_test();
    }
  }
  if (!(argc >= 2 && strcmp(argv[1], "noinf"))) {
    weird[0] = 0.0;
    weird[1] = 1.0 / weird[0];         /* +infty */
    weird[2] = -1.0 / weird[0];        /* -infty */
    weird[3] = 0.0 / weird[0];         /* NaN */
    for (X = 0; X < 4; X++) {
      for (Y = 0; Y < 4; Y++) {
        F = weird[X]; G = weird[Y]; do_test();
      }
    }
  }
  while(1) {
    X = (rand() & 0x1FFFFFFF) - 0x10000000;
    Y = (rand() & 0x1FFFFFFF) - 0x10000000;
    F = X / 1e3;
    G = Y / 1e3;
    do_test();
    printf("."); fflush(stdout);
  }
  return 0;
}
