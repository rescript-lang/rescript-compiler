/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
# include <unistd.h>
#else
# ifndef _WIN32
#  include <sys/file.h>
#  ifndef R_OK
#   define R_OK    4/* test for read permission */
#   define W_OK    2/* test for write permission */
#   define X_OK    1/* test for execute (search) permission */
#   define F_OK    0/* test for presence of file */
#  endif
# else
#  define R_OK    4/* test for read permission */
#  define W_OK    2/* test for write permission */
#  define X_OK    4/* test for execute permission - not implemented in Win32 */
#  define F_OK    0/* test for presence of file */
# endif
#endif

static int access_permission_table[] = {
  R_OK, W_OK, X_OK, F_OK
};

CAMLprim value unix_access(value path, value perms)
{
  CAMLparam2(path, perms);
  char * p;
  int ret, cv_flags;

  cv_flags = convert_flag_list(perms, access_permission_table);
  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = access(p, cv_flags);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("access", path);
  CAMLreturn(Val_unit);
}
