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

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_UTIME

#include <sys/types.h>
#ifndef _WIN32
#include <utime.h>
#else
#include <sys/utime.h>
#endif

CAMLprim value unix_utimes(value path, value atime, value mtime)
{
  CAMLparam3(path, atime, mtime);
  struct utimbuf times, * t;
  char * p;
  int ret;
  times.actime = Double_val(atime);
  times.modtime = Double_val(mtime);
  if (times.actime || times.modtime)
    t = &times;
  else
    t = (struct utimbuf *) NULL;
  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = utime(p, t);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) uerror("utimes", path);
  CAMLreturn(Val_unit);
}

#else

#ifdef HAS_UTIMES

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value unix_utimes(value path, value atime, value mtime)
{
  CAMLparam3(path, atime, mtime);
  struct timeval tv[2], * t;
  char * p;
  int ret;
  double at = Double_val(atime);
  double mt = Double_val(mtime);
  tv[0].tv_sec = at;
  tv[0].tv_usec = (at - tv[0].tv_sec) * 1000000;
  tv[1].tv_sec = mt;
  tv[1].tv_usec = (mt - tv[1].tv_sec) * 1000000;
  if (tv[0].tv_sec || tv[1].tv_sec)
    t = tv;
  else
    t = (struct timeval *) NULL;
  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = utimes(p, t);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) uerror("utimes", path);
  CAMLreturn(Val_unit);
}

#else

CAMLprim value unix_utimes(value path, value atime, value mtime)
{ invalid_argument("utimes not implemented"); }

#endif
#endif
