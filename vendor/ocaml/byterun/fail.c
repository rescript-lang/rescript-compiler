/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Raising exceptions from C. */

#include <stdio.h>
#include <stdlib.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/gc.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/stacks.h"

CAMLexport struct longjmp_buffer * caml_external_raise = NULL;
value caml_exn_bucket;

CAMLexport void caml_raise(value v)
{
  Unlock_exn();
  caml_exn_bucket = v;
  if (caml_external_raise == NULL) caml_fatal_uncaught_exception(v);
  siglongjmp(caml_external_raise->buf, 1);
}

CAMLexport void caml_raise_constant(value tag)
{
  caml_raise(tag);
}

CAMLexport void caml_raise_with_arg(value tag, value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  caml_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void caml_raise_with_args(value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  value bucket;
  int i;

  Assert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  caml_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void caml_raise_with_string(value tag, char const *msg)
{
  CAMLparam1(tag);
  value v_msg = caml_copy_string(msg);
  caml_raise_with_arg(tag, v_msg);
  CAMLnoreturn;
}

/* PR#5115: Failure and Invalid_argument can be triggered by
   input_value while reading the initial value of [caml_global_data]. */

CAMLexport void caml_failwith (char const *msg)
{
  if (caml_global_data == 0) {
    fprintf(stderr, "Fatal error: exception Failure(\"%s\")\n", msg);
    exit(2);
  }
  caml_raise_with_string(Field(caml_global_data, FAILURE_EXN), msg);
}

CAMLexport void caml_invalid_argument (char const *msg)
{
  if (caml_global_data == 0) {
    fprintf(stderr, "Fatal error: exception Invalid_argument(\"%s\")\n", msg);
    exit(2);
  }
  caml_raise_with_string(Field(caml_global_data, INVALID_EXN), msg);
}

CAMLexport void caml_array_bound_error(void)
{
  caml_invalid_argument("index out of bounds");
}

CAMLexport void caml_raise_out_of_memory(void)
{
  caml_raise_constant(Field(caml_global_data, OUT_OF_MEMORY_EXN));
}

CAMLexport void caml_raise_stack_overflow(void)
{
  caml_raise_constant(Field(caml_global_data, STACK_OVERFLOW_EXN));
}

CAMLexport void caml_raise_sys_error(value msg)
{
  caml_raise_with_arg(Field(caml_global_data, SYS_ERROR_EXN), msg);
}

CAMLexport void caml_raise_end_of_file(void)
{
  caml_raise_constant(Field(caml_global_data, END_OF_FILE_EXN));
}

CAMLexport void caml_raise_zero_divide(void)
{
  caml_raise_constant(Field(caml_global_data, ZERO_DIVIDE_EXN));
}

CAMLexport void caml_raise_not_found(void)
{
  caml_raise_constant(Field(caml_global_data, NOT_FOUND_EXN));
}

CAMLexport void caml_raise_sys_blocked_io(void)
{
  caml_raise_constant(Field(caml_global_data, SYS_BLOCKED_IO));
}

int caml_is_special_exception(value exn) {
  return exn == Field(caml_global_data, MATCH_FAILURE_EXN)
    || exn == Field(caml_global_data, ASSERT_FAILURE_EXN)
    || exn == Field(caml_global_data, UNDEFINED_RECURSIVE_MODULE_EXN);
}
