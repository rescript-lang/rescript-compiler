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
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

#if !defined (_WIN32) && !macintosh
#include <sys/param.h>
#endif

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 512
#endif
#endif

#ifdef HAS_GETCWD

CAMLprim value unix_getcwd(value unit)
{
  char_os buff[PATH_MAX];
  char_os * ret;
  ret = getcwd_os(buff, sizeof(buff)/sizeof(*buff));
  if (ret == 0) uerror("getcwd", Nothing);
  return caml_copy_string_of_os(buff);
}

#else

CAMLprim value unix_getcwd(value unit)
{ caml_invalid_argument("getcwd not implemented"); }

#endif
