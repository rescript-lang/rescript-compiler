/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <caml/mlvalues.h>
#include <bigarray.h>

extern void filltab_(void);
extern void printtab_(float * data, int * dimx, int * dimy);
extern float ftab_[];

#define DIMX 6
#define DIMY 8

double ctab[DIMX][DIMY];

void filltab(void)
{
  int x, y;
  for (x = 0; x < DIMX; x++)
    for (y = 0; y < DIMY; y++)
      ctab[x][y] = x * 100 + y;
}

void printtab(double tab[DIMX][DIMY])
{
  int x, y;
  for (x = 0; x < DIMX; x++) {
    printf("%3d", x);
    for (y = 0; y < DIMY; y++)
      printf("  %6.1f", tab[x][y]);
    printf("\n");
  }
}

value c_filltab(value unit)
{
  filltab();
  return alloc_bigarray_dims(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT,
                             2, ctab, DIMX, DIMY);
}

value c_printtab(value ba)
{
  printtab(Data_bigarray_val(ba));
  return Val_unit;
}

value fortran_filltab(value unit)
{
  filltab_();
  return alloc_bigarray_dims(BIGARRAY_FLOAT32 | BIGARRAY_FORTRAN_LAYOUT,
                             2, ftab_, 8, 6);
}

value fortran_printtab(value ba)
{
  int dimx = Bigarray_val(ba)->dim[0];
  int dimy = Bigarray_val(ba)->dim[1];
  printtab_(Data_bigarray_val(ba), &dimx, &dimy);
  return Val_unit;
}
