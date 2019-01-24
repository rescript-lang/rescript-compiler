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

#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"
#include <fcntl.h>

CAMLprim value unix_pipe(value cloexec, value vunit)
{
  int fd[2];
  value res;
#ifdef HAS_PIPE2
  if (pipe2(fd, unix_cloexec_p(cloexec) ? O_CLOEXEC : 0) == -1)
    uerror("pipe", Nothing);
#else
  if (pipe(fd) == -1) uerror("pipe", Nothing);
  if (unix_cloexec_p(cloexec)) {
    unix_set_cloexec(fd[0], "pipe", Nothing);
    unix_set_cloexec(fd[1], "pipe", Nothing);
  }
#endif
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);
  return res;
}
