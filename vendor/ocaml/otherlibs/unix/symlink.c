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

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_SYMLINK

CAMLprim value unix_symlink(value to_dir, value path1, value path2)
{
  CAMLparam3(to_dir, path1, path2);
  char * p1;
  char * p2;
  int ret;
  caml_unix_check_path(path1, "symlink");
  caml_unix_check_path(path2, "symlink");
  p1 = caml_stat_strdup(String_val(path1));
  p2 = caml_stat_strdup(String_val(path2));
  caml_enter_blocking_section();
  ret = symlink(p1, p2);
  caml_leave_blocking_section();
  caml_stat_free(p1);
  caml_stat_free(p2);
  if (ret == -1)
    uerror("symlink", path2);
  CAMLreturn(Val_unit);
}

CAMLprim value unix_has_symlink(value unit)
{
  CAMLparam0();
  CAMLreturn(Val_true);
}

#else

CAMLprim value unix_symlink(value to_dir, value path1, value path2)
{ caml_invalid_argument("symlink not implemented"); }

CAMLprim value unix_has_symlink(value unit)
{
  CAMLparam0();
  CAMLreturn(Val_false);
}

#endif
