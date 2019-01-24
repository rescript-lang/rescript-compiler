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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#define CAML_INTERNALS
#include <caml/osdeps.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
# include <unistd.h>
#else
# ifndef _WIN32
#  include <sys/file.h>
# endif
# ifndef R_OK
#   define R_OK    4/* test for read permission */
#   define W_OK    2/* test for write permission */
#   define X_OK    1/* test for execute (search) permission */
#   define F_OK    0/* test for presence of file */
# endif
#endif

static int access_permission_table[] = {
  R_OK,
  W_OK, 
#ifdef _WIN32
  /* Since there is no concept of execute permission on Windows,
     we fall b+ack to the read permission */
  R_OK,
#else
  X_OK,
#endif
  F_OK
};

CAMLprim value unix_access(value path, value perms)
{
  CAMLparam2(path, perms);
  char_os * p;
  int ret, cv_flags;

  caml_unix_check_path(path, "access");
  cv_flags = caml_convert_flag_list(perms, access_permission_table);
  p = caml_stat_strdup_to_os(String_val(path));
  caml_enter_blocking_section();
  ret = access_os(p, cv_flags);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("access", path);
  CAMLreturn(Val_unit);
}
