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
#include <caml/bigarray.h>

char *ocaml_buffer;
char *c_buffer;

value test_set_buffers(value v_ocaml_buffer, value v_c_buffer)
{
  ocaml_buffer = Caml_ba_data_val(v_ocaml_buffer);
  c_buffer = Caml_ba_data_val(v_c_buffer);
  return Val_unit;
}

value test_cleanup_normal(void)
{
  return Val_int(0);
}

double test_cleanup_float(void)
{
  return 0.;
}
