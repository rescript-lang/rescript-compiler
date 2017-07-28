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

#ifndef CAML_FAIL_H
#define CAML_FAIL_H

/* <private> */
#include <setjmp.h>
/* </private> */

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

/* <private> */
#define OUT_OF_MEMORY_EXN 0     /* "Out_of_memory" */
#define SYS_ERROR_EXN 1         /* "Sys_error" */
#define FAILURE_EXN 2           /* "Failure" */
#define INVALID_EXN 3           /* "Invalid_argument" */
#define END_OF_FILE_EXN 4       /* "End_of_file" */
#define ZERO_DIVIDE_EXN 5       /* "Division_by_zero" */
#define NOT_FOUND_EXN 6         /* "Not_found" */
#define MATCH_FAILURE_EXN 7     /* "Match_failure" */
#define STACK_OVERFLOW_EXN 8    /* "Stack_overflow" */
#define SYS_BLOCKED_IO 9        /* "Sys_blocked_io" */
#define ASSERT_FAILURE_EXN 10   /* "Assert_failure" */
#define UNDEFINED_RECURSIVE_MODULE_EXN 11 /* "Undefined_recursive_module" */

#ifdef POSIX_SIGNALS
struct longjmp_buffer {
  sigjmp_buf buf;
};
#else
struct longjmp_buffer {
  jmp_buf buf;
};
#define sigsetjmp(buf,save) setjmp(buf)
#define siglongjmp(buf,val) longjmp(buf,val)
#endif

CAMLextern struct longjmp_buffer * caml_external_raise;
extern value caml_exn_bucket;
int caml_is_special_exception(value exn);

/* </private> */

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern void caml_raise (value bucket) Noreturn;
CAMLextern void caml_raise_constant (value tag) Noreturn;
CAMLextern void caml_raise_with_arg (value tag, value arg) Noreturn;
CAMLextern void caml_raise_with_args (value tag, int nargs, value arg[])
                Noreturn;
CAMLextern void caml_raise_with_string (value tag, char const * msg) Noreturn;
CAMLextern void caml_failwith (char const *) Noreturn;
CAMLextern void caml_invalid_argument (char const *) Noreturn;
CAMLextern void caml_raise_out_of_memory (void) Noreturn;
CAMLextern void caml_raise_stack_overflow (void) Noreturn;
CAMLextern void caml_raise_sys_error (value) Noreturn;
CAMLextern void caml_raise_end_of_file (void) Noreturn;
CAMLextern void caml_raise_zero_divide (void) Noreturn;
CAMLextern void caml_raise_not_found (void) Noreturn;
CAMLextern void caml_array_bound_error (void) Noreturn;
CAMLextern void caml_raise_sys_blocked_io (void) Noreturn;

#ifdef __cplusplus
}
#endif

#endif /* CAML_FAIL_H */
