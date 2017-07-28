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
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>

#ifdef HAS_SYMLINK

#include <sys/param.h>
#include "unixsupport.h"

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 512
#endif
#endif

CAMLprim value unix_readlink(value path)
{
  CAMLparam1(path);
  char buffer[PATH_MAX];
  int len;
  char * p;
  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  len = readlink(p, buffer, sizeof(buffer) - 1);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (len == -1) uerror("readlink", path);
  buffer[len] = '\0';
  CAMLreturn(copy_string(buffer));
}

#else

CAMLprim value unix_readlink(value path)
{ invalid_argument("readlink not implemented"); }

#endif
