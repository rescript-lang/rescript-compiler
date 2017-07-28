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
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif

CAMLprim value unix_set_nonblock(value fd)
{
  int retcode;
  retcode = fcntl(Int_val(fd), F_GETFL, 0);
  if (retcode == -1 ||
      fcntl(Int_val(fd), F_SETFL, retcode | O_NONBLOCK) == -1)
    uerror("set_nonblock", Nothing);
  return Val_unit;
}

CAMLprim value unix_clear_nonblock(value fd)
{
  int retcode;
  retcode = fcntl(Int_val(fd), F_GETFL, 0);
  if (retcode == -1 ||
      fcntl(Int_val(fd), F_SETFL, retcode & ~O_NONBLOCK) == -1)
    uerror("clear_nonblock", Nothing);
  return Val_unit;
}

#ifdef FD_CLOEXEC

CAMLprim value unix_set_close_on_exec(value fd)
{
  int retcode;
  retcode = fcntl(Int_val(fd), F_GETFD, 0);
  if (retcode == -1 ||
      fcntl(Int_val(fd), F_SETFD, retcode | FD_CLOEXEC) == -1)
    uerror("set_close_on_exec", Nothing);
  return Val_unit;
}

CAMLprim value unix_clear_close_on_exec(value fd)
{
  int retcode;
  retcode = fcntl(Int_val(fd), F_GETFD, 0);
  if (retcode == -1 ||
      fcntl(Int_val(fd), F_SETFD, retcode & ~FD_CLOEXEC) == -1)
    uerror("clear_close_on_exec", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_set_close_on_exec(value fd)
{ invalid_argument("set_close_on_exec not implemented"); }

CAMLprim value unix_clear_close_on_exec(value fd)
{ invalid_argument("clear_close_on_exec not implemented"); }

#endif
