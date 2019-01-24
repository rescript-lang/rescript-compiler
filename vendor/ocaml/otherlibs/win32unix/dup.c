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
#include "unixsupport.h"

CAMLprim value unix_dup(value cloexec, value fd)
{
  HANDLE newh;
  value newfd;
  int kind = Descr_kind_val(fd);
  if (! DuplicateHandle(GetCurrentProcess(), Handle_val(fd),
                        GetCurrentProcess(), &newh,
                        0L,
                        unix_cloexec_p(cloexec) ? FALSE : TRUE,
                        DUPLICATE_SAME_ACCESS)) {
    win32_maperr(GetLastError());
    return -1;
  }
  newfd = win_alloc_handle(newh);
  Descr_kind_val(newfd) = kind;
  return newfd;
}
