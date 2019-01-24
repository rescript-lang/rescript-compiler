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

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

CAMLprim value unix_chdir(value path)
{
  CAMLparam1(path);
  char_os * p;
  int ret;
  caml_unix_check_path(path, "chdir");
  p = caml_stat_strdup_to_os(String_val(path));
  caml_enter_blocking_section();
  ret = chdir_os(p);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) uerror("chdir", path);
  CAMLreturn(Val_unit);
}
