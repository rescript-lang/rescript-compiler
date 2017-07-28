/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#include "caml/mlvalues.h"
#include "stdio.h"

value manyargs(value a, value b, value c, value d, value e, value f,
               value g, value h, value i, value j, value k)
{
  printf("a = %d\n", Int_val(a));
  printf("b = %d\n", Int_val(b));
  printf("c = %d\n", Int_val(c));
  printf("d = %d\n", Int_val(d));
  printf("e = %d\n", Int_val(e));
  printf("f = %d\n", Int_val(f));
  printf("g = %d\n", Int_val(g));
  printf("h = %d\n", Int_val(h));
  printf("i = %d\n", Int_val(i));
  printf("j = %d\n", Int_val(j));
  printf("k = %d\n", Int_val(k));
  return Val_unit;
}

value manyargs_argv(value *argv, int argc)
{
  return manyargs(argv[0], argv[1], argv[2], argv[3], argv[4],
                  argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}
