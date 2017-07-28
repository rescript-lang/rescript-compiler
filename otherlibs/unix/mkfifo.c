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

#include <sys/types.h>
#include <sys/stat.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_MKFIFO

CAMLprim value unix_mkfifo(value path, value mode)
{
  CAMLparam2(path, mode);
  char * p;
  int ret;
  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = mkfifo(p, Int_val(mode));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("mkfifo", path);
  CAMLreturn(Val_unit);
}

#else

#include <sys/types.h>
#include <sys/stat.h>

#ifdef S_IFIFO

CAMLprim value unix_mkfifo(value path, value mode)
{
  CAMLparam2(path, mode);
  char * p;
  int ret;
  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = mknod(p, (Int_val(mode) & 07777) | S_IFIFO, 0);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("mkfifo", path);
  CAMLreturn(Val_unit);
}

#else

CAMLprim value unix_mkfifo(value path, value mode)
{
  invalid_argument("mkfifo not implemented");
}

#endif
#endif
