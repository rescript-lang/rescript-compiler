/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                        Alain Frisch, LexiFi                         */
/*                                                                     */
/*  Copyright 2007 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include <stdio.h>

value factorial(value n){
  CAMLparam1(n);
  CAMLlocal1(s);

  static char buf[256];
  int x = 1;
  int i;
  int m = Int_val(n);
  for (i = 1; i <= m; i++) x *= i;
  sprintf(buf,"%i",x);
  s = copy_string(buf);
  CAMLreturn (s);
}
