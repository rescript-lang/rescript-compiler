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

CAMLprim value unix_dup(value cloexec, value fd)
{
  int ret;
#ifdef F_DUPFD_CLOEXEC
  ret = fcntl(Int_val(fd),
              (unix_cloexec_p(cloexec) ? F_DUPFD_CLOEXEC : F_DUPFD),
              0);
#else
  ret = dup(Int_val(fd));
#endif
  if (ret == -1) uerror("dup", Nothing);
#ifndef F_DUPFD_CLOEXEC
  if (unix_cloexec_p(cloexec)) unix_set_cloexec(ret, "dup", Nothing);
#endif
  return Val_int(ret);
}
