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
#include "unixsupport.h"
#include <fcntl.h>

CAMLprim value unix_dup2(value cloexec, value fd1, value fd2)
{
  if (Int_val(fd2) == Int_val(fd1)) {
    /* In this case, dup3 fails and dup2 does nothing. */
    /* Just apply the cloexec flag to fd2, if it is given. */
    if (Is_block(cloexec)) {
      if (Bool_val(Field(cloexec, 0)))
        unix_set_cloexec(Int_val(fd2), "dup2", Nothing);
      else
        unix_clear_cloexec(Int_val(fd2), "dup2", Nothing);
    }
  } else {
#ifdef HAS_DUP3
    if (dup3(Int_val(fd1), Int_val(fd2),
             unix_cloexec_p(cloexec) ? O_CLOEXEC : 0) == -1)
      uerror("dup2", Nothing);
#else
    if (dup2(Int_val(fd1), Int_val(fd2)) == -1) uerror("dup2", Nothing);
    if (unix_cloexec_p(cloexec))
      unix_set_cloexec(Int_val(fd2), "dup2", Nothing);
#endif
  }
  return Val_unit;
}
