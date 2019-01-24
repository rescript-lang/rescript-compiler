/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
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
#include <caml/memory.h>
#include <caml/alloc.h>
#include "unixsupport.h"
#include <fcntl.h>

/* PR#4749: pick a size that matches that of I/O buffers */
#define SIZEBUF 4096

CAMLprim value unix_pipe(value cloexec, value unit)
{
  SECURITY_ATTRIBUTES attr;
  HANDLE readh, writeh;
  value readfd = Val_unit, writefd = Val_unit, res;

  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = unix_cloexec_p(cloexec) ? FALSE : TRUE;
  if (! CreatePipe(&readh, &writeh, &attr, SIZEBUF)) {
    win32_maperr(GetLastError());
    uerror("pipe", Nothing);
  }
  Begin_roots2(readfd, writefd)
    readfd = win_alloc_handle(readh);
    writefd = win_alloc_handle(writeh);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = readfd;
    Field(res, 1) = writefd;
  End_roots();
  return res;
}
