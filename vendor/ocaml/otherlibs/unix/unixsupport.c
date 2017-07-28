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
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "unixsupport.h"
#include "cst2constr.h"
#include <errno.h>

#ifndef E2BIG
#define E2BIG (-1)
#endif
#ifndef EACCES
#define EACCES (-1)
#endif
#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EBADF
#define EBADF (-1)
#endif
#ifndef EBUSY
#define EBUSY (-1)
#endif
#ifndef ECHILD
#define ECHILD (-1)
#endif
#ifndef EDEADLK
#define EDEADLK (-1)
#endif
#ifndef EDOM
#define EDOM (-1)
#endif
#ifndef EEXIST
#define EEXIST (-1)
#endif

#ifndef EFAULT
#define EFAULT (-1)
#endif
#ifndef EFBIG
#define EFBIG (-1)
#endif
#ifndef EINTR
#define EINTR (-1)
#endif
#ifndef EINVAL
#define EINVAL (-1)
#endif
#ifndef EIO
#define EIO (-1)
#endif
#ifndef EISDIR
#define EISDIR (-1)
#endif
#ifndef EMFILE
#define EMFILE (-1)
#endif
#ifndef EMLINK
#define EMLINK (-1)
#endif
#ifndef ENAMETOOLONG
#define ENAMETOOLONG (-1)
#endif
#ifndef ENFILE
#define ENFILE (-1)
#endif
#ifndef ENODEV
#define ENODEV (-1)
#endif
#ifndef ENOENT
#define ENOENT (-1)
#endif
#ifndef ENOEXEC
#define ENOEXEC (-1)
#endif
#ifndef ENOLCK
#define ENOLCK (-1)
#endif
#ifndef ENOMEM
#define ENOMEM (-1)
#endif
#ifndef ENOSPC
#define ENOSPC (-1)
#endif
#ifndef ENOSYS
#define ENOSYS (-1)
#endif
#ifndef ENOTDIR
#define ENOTDIR (-1)
#endif
#ifndef ENOTEMPTY
#define ENOTEMPTY (-1)
#endif
#ifndef ENOTTY
#define ENOTTY (-1)
#endif
#ifndef ENXIO
#define ENXIO (-1)
#endif
#ifndef EPERM
#define EPERM (-1)
#endif
#ifndef EPIPE
#define EPIPE (-1)
#endif
#ifndef ERANGE
#define ERANGE (-1)
#endif
#ifndef EROFS
#define EROFS (-1)
#endif
#ifndef ESPIPE
#define ESPIPE (-1)
#endif
#ifndef ESRCH
#define ESRCH (-1)
#endif
#ifndef EXDEV
#define EXDEV (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif
#ifndef EINPROGRESS
#define EINPROGRESS (-1)
#endif
#ifndef EALREADY
#define EALREADY (-1)
#endif
#ifndef ENOTSOCK
#define ENOTSOCK (-1)
#endif
#ifndef EDESTADDRREQ
#define EDESTADDRREQ (-1)
#endif
#ifndef EMSGSIZE
#define EMSGSIZE (-1)
#endif
#ifndef EPROTOTYPE
#define EPROTOTYPE (-1)
#endif
#ifndef ENOPROTOOPT
#define ENOPROTOOPT (-1)
#endif
#ifndef EPROTONOSUPPORT
#define EPROTONOSUPPORT (-1)
#endif
#ifndef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT (-1)
#endif
#ifndef EOPNOTSUPP
#  ifdef ENOTSUP
#    define EOPNOTSUPP ENOTSUP
#  else
#    define EOPNOTSUPP (-1)
#  endif
#endif
#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT (-1)
#endif
#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT (-1)
#endif
#ifndef EADDRINUSE
#define EADDRINUSE (-1)
#endif
#ifndef EADDRNOTAVAIL
#define EADDRNOTAVAIL (-1)
#endif
#ifndef ENETDOWN
#define ENETDOWN (-1)
#endif
#ifndef ENETUNREACH
#define ENETUNREACH (-1)
#endif
#ifndef ENETRESET
#define ENETRESET (-1)
#endif
#ifndef ECONNABORTED
#define ECONNABORTED (-1)
#endif
#ifndef ECONNRESET
#define ECONNRESET (-1)
#endif
#ifndef ENOBUFS
#define ENOBUFS (-1)
#endif
#ifndef EISCONN
#define EISCONN (-1)
#endif
#ifndef ENOTCONN
#define ENOTCONN (-1)
#endif
#ifndef ESHUTDOWN
#define ESHUTDOWN (-1)
#endif
#ifndef ETOOMANYREFS
#define ETOOMANYREFS (-1)
#endif
#ifndef ETIMEDOUT
#define ETIMEDOUT (-1)
#endif
#ifndef ECONNREFUSED
#define ECONNREFUSED (-1)
#endif
#ifndef EHOSTDOWN
#define EHOSTDOWN (-1)
#endif
#ifndef EHOSTUNREACH
#define EHOSTUNREACH (-1)
#endif
#ifndef ENOTEMPTY
#define ENOTEMPTY (-1)
#endif
#ifndef ELOOP
#define ELOOP (-1)
#endif
#ifndef EOVERFLOW
#define EOVERFLOW (-1)
#endif

int error_table[] = {
  E2BIG, EACCES, EAGAIN, EBADF, EBUSY, ECHILD, EDEADLK, EDOM,
  EEXIST, EFAULT, EFBIG, EINTR, EINVAL, EIO, EISDIR, EMFILE, EMLINK,
  ENAMETOOLONG, ENFILE, ENODEV, ENOENT, ENOEXEC, ENOLCK, ENOMEM, ENOSPC,
  ENOSYS, ENOTDIR, ENOTEMPTY, ENOTTY, ENXIO, EPERM, EPIPE, ERANGE,
  EROFS, ESPIPE, ESRCH, EXDEV, EWOULDBLOCK, EINPROGRESS, EALREADY,
  ENOTSOCK, EDESTADDRREQ, EMSGSIZE, EPROTOTYPE, ENOPROTOOPT,
  EPROTONOSUPPORT, ESOCKTNOSUPPORT, EOPNOTSUPP, EPFNOSUPPORT,
  EAFNOSUPPORT, EADDRINUSE, EADDRNOTAVAIL, ENETDOWN, ENETUNREACH,
  ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS, EISCONN, ENOTCONN,
  ESHUTDOWN, ETOOMANYREFS, ETIMEDOUT, ECONNREFUSED, EHOSTDOWN,
  EHOSTUNREACH, ELOOP, EOVERFLOW /*, EUNKNOWNERR */
};

static value * unix_error_exn = NULL;

value unix_error_of_code (int errcode)
{
  int errconstr;
  value err;

#if defined(ENOTSUP) && (EOPNOTSUPP != ENOTSUP)
  if (errcode == ENOTSUP)
    errcode = EOPNOTSUPP;
#endif

  errconstr =
      cst_to_constr(errcode, error_table, sizeof(error_table)/sizeof(int), -1);
  if (errconstr == Val_int(-1)) {
    err = alloc_small(1, 0);
    Field(err, 0) = Val_int(errcode);
  } else {
    err = errconstr;
  }
  return err;
}

extern int code_of_unix_error (value error)
{
  if (Is_block(error)) {
    return Int_val(Field(error, 0));
  } else {
    return error_table[Int_val(error)];
  }
}

void unix_error(int errcode, char *cmdname, value cmdarg)
{
  value res;
  value name = Val_unit, err = Val_unit, arg = Val_unit;

  Begin_roots3 (name, err, arg);
    arg = cmdarg == Nothing ? copy_string("") : cmdarg;
    name = copy_string(cmdname);
    err = unix_error_of_code (errcode);
    if (unix_error_exn == NULL) {
      unix_error_exn = caml_named_value("Unix.Unix_error");
      if (unix_error_exn == NULL)
        invalid_argument("Exception Unix.Unix_error not initialized,"
                         " please link unix.cma");
    }
    res = alloc_small(4, 0);
    Field(res, 0) = *unix_error_exn;
    Field(res, 1) = err;
    Field(res, 2) = name;
    Field(res, 3) = arg;
  End_roots();
  mlraise(res);
}

void uerror(char *cmdname, value cmdarg)
{
  unix_error(errno, cmdname, cmdarg);
}
