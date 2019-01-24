/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*                  Jeremie Dimino, Jane Street Europe                    */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>

int64_t test_int64_add(int64_t x, int64_t y) { return (x + y); }
int64_t test_int64_sub(int64_t x, int64_t y) { return (x - y); }
int64_t test_int64_mul(int64_t x, int64_t y) { return (x * y); }

value test_ignore_int64(int64_t x)
{
  return Val_unit;
}
