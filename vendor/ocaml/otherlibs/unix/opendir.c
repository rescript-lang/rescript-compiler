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
#include <caml/signals.h>
#include "unixsupport.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

CAMLprim value unix_opendir(value path)
{
  CAMLparam1(path);
  DIR * d;
  value res;
  char * p;

  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  d = opendir(p);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (d == (DIR *) NULL) uerror("opendir", path);
  res = alloc_small(1, Abstract_tag);
  DIR_Val(res) = d;
  CAMLreturn(res);
}
