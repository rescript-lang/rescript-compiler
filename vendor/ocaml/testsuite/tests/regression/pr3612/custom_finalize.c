/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*                        Pierre Chambart, OCamlPro                       */
/*                                                                        */
/*   Copyright 2014 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/custom.h>

static int free_counter = 0;
static int alloc_counter = 0;

static void caml_test_finalize(value v)
{
  free_counter++;
}

static void caml_test_serialize(value v,
                                uintnat * wsize_32,
                                uintnat * wsize_64)
{
  *wsize_32 = 0;
  *wsize_64 = 0;
}

uintnat caml_test_deserialize(void * dst)
{
  alloc_counter++;
  return 0;
}

static struct custom_operations caml_test_ops = {
  "_test",
  caml_test_finalize,
  custom_compare_default,
  custom_hash_default,
  caml_test_serialize,
  caml_test_deserialize,
  custom_compare_ext_default
};

value caml_test_pr3612_alloc(value unit)
{
  return caml_alloc_custom(&caml_test_ops, 0, 0, 1);
}

value caml_test_pr3612_counter(value unit)
{
  return Val_int(alloc_counter-free_counter);
}

CAMLprim value caml_test_pr3612_init(value unit)
{
  caml_register_custom_operations(&caml_test_ops);
  return Val_unit;
}
