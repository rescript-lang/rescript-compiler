/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_SYS_H
#define CAML_SYS_H

#ifdef CAML_INTERNALS

#include "misc.h"

#ifdef __cplusplus
extern "C" {
#endif

#define NO_ARG Val_int(0)

CAMLextern void caml_sys_error (value);
CAMLextern void caml_sys_io_error (value);
CAMLextern double caml_sys_time_unboxed(value);
CAMLextern void caml_sys_init (char_os * exe_name, char_os ** argv);
CAMLextern value caml_sys_exit (value);
extern double caml_sys_time_unboxed(value);
CAMLextern value caml_sys_get_argv(value unit);

extern char_os * caml_exe_name;

#ifdef __cplusplus
}
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_SYS_H */
