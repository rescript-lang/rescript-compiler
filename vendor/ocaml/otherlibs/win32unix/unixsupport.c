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

#include <stddef.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include "unixsupport.h"
#include "cst2constr.h"
#include <errno.h>

/* Heap-allocation of Windows file handles */

static int win_handle_compare(value v1, value v2)
{
  HANDLE h1 = Handle_val(v1);
  HANDLE h2 = Handle_val(v2);
  return h1 == h2 ? 0 : h1 < h2 ? -1 : 1;
}

static intnat win_handle_hash(value v)
{
  return (intnat) Handle_val(v);
}

static struct custom_operations win_handle_ops = {
  "_handle",
  custom_finalize_default,
  win_handle_compare,
  win_handle_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

value win_alloc_handle(HANDLE h)
{
  value res = alloc_custom(&win_handle_ops, sizeof(struct filedescr), 0, 1);
  Handle_val(res) = h;
  Descr_kind_val(res) = KIND_HANDLE;
  CRT_fd_val(res) = NO_CRT_FD;
  Flags_fd_val(res) = FLAGS_FD_IS_BLOCKING;
  return res;
}

value win_alloc_socket(SOCKET s)
{
  value res = alloc_custom(&win_handle_ops, sizeof(struct filedescr), 0, 1);
  Socket_val(res) = s;
  Descr_kind_val(res) = KIND_SOCKET;
  CRT_fd_val(res) = NO_CRT_FD;
  Flags_fd_val(res) = FLAGS_FD_IS_BLOCKING;
  return res;
}

#if 0
/* PR#4750: this function is no longer used */
value win_alloc_handle_or_socket(HANDLE h)
{
  value res = win_alloc_handle(h);
  int opt;
  int optlen = sizeof(opt);
  if (getsockopt((SOCKET) h, SOL_SOCKET, SO_TYPE, (char *)&opt, &optlen) == 0)
    Descr_kind_val(res) = KIND_SOCKET;
  return res;
}
#endif

/* Mapping of Windows error codes to POSIX error codes */

struct error_entry { DWORD win_code; int range; int posix_code; };

static struct error_entry win_error_table[] = {
  { ERROR_INVALID_FUNCTION, 0, EINVAL},
  { ERROR_FILE_NOT_FOUND, 0, ENOENT},
  { ERROR_PATH_NOT_FOUND, 0, ENOENT},
  { ERROR_TOO_MANY_OPEN_FILES, 0, EMFILE},
  { ERROR_ACCESS_DENIED, 0, EACCES},
  { ERROR_INVALID_HANDLE, 0, EBADF},
  { ERROR_ARENA_TRASHED, 0, ENOMEM},
  { ERROR_NOT_ENOUGH_MEMORY, 0, ENOMEM},
  { ERROR_INVALID_BLOCK, 0, ENOMEM},
  { ERROR_BAD_ENVIRONMENT, 0, E2BIG},
  { ERROR_BAD_FORMAT, 0, ENOEXEC},
  { ERROR_INVALID_ACCESS, 0, EINVAL},
  { ERROR_INVALID_DATA, 0, EINVAL},
  { ERROR_INVALID_DRIVE, 0, ENOENT},
  { ERROR_CURRENT_DIRECTORY, 0, EACCES},
  { ERROR_NOT_SAME_DEVICE, 0, EXDEV},
  { ERROR_NO_MORE_FILES, 0, ENOENT},
  { ERROR_LOCK_VIOLATION, 0, EACCES},
  { ERROR_BAD_NETPATH, 0, ENOENT},
  { ERROR_NETWORK_ACCESS_DENIED, 0, EACCES},
  { ERROR_BAD_NET_NAME, 0, ENOENT},
  { ERROR_FILE_EXISTS, 0, EEXIST},
  { ERROR_CANNOT_MAKE, 0, EACCES},
  { ERROR_FAIL_I24, 0, EACCES},
  { ERROR_INVALID_PARAMETER, 0, EINVAL},
  { ERROR_NO_PROC_SLOTS, 0, EAGAIN},
  { ERROR_DRIVE_LOCKED, 0, EACCES},
  { ERROR_BROKEN_PIPE, 0, EPIPE},
  { ERROR_NO_DATA, 0, EPIPE},
  { ERROR_DISK_FULL, 0, ENOSPC},
  { ERROR_INVALID_TARGET_HANDLE, 0, EBADF},
  { ERROR_INVALID_HANDLE, 0, EINVAL},
  { ERROR_WAIT_NO_CHILDREN, 0, ECHILD},
  { ERROR_CHILD_NOT_COMPLETE, 0, ECHILD},
  { ERROR_DIRECT_ACCESS_HANDLE, 0, EBADF},
  { ERROR_NEGATIVE_SEEK, 0, EINVAL},
  { ERROR_SEEK_ON_DEVICE, 0, EACCES},
  { ERROR_DIR_NOT_EMPTY, 0, ENOTEMPTY},
  { ERROR_NOT_LOCKED, 0, EACCES},
  { ERROR_BAD_PATHNAME, 0, ENOENT},
  { ERROR_MAX_THRDS_REACHED, 0, EAGAIN},
  { ERROR_LOCK_FAILED, 0, EACCES},
  { ERROR_ALREADY_EXISTS, 0, EEXIST},
  { ERROR_FILENAME_EXCED_RANGE, 0, ENOENT},
  { ERROR_NESTING_NOT_ALLOWED, 0, EAGAIN},
  { ERROR_NOT_ENOUGH_QUOTA, 0, ENOMEM},
  { ERROR_INVALID_STARTING_CODESEG,
    ERROR_INFLOOP_IN_RELOC_CHAIN - ERROR_INVALID_STARTING_CODESEG,
    ENOEXEC },
  { ERROR_WRITE_PROTECT,
    ERROR_SHARING_BUFFER_EXCEEDED - ERROR_WRITE_PROTECT,
    EACCES },
  { WSAEINVAL, 0, EINVAL },
  { WSAEACCES, 0, EACCES },
  { WSAEBADF, 0, EBADF },
  { WSAEFAULT, 0, EFAULT },
  { WSAEINTR, 0, EINTR },
  { WSAEINVAL, 0, EINVAL },
  { WSAEMFILE, 0, EMFILE },
#ifdef WSANAMETOOLONG
  { WSANAMETOOLONG, 0, ENAMETOOLONG },
#endif
#ifdef WSAENFILE
  { WSAENFILE, 0, ENFILE },
#endif
  { WSAENOTEMPTY, 0, ENOTEMPTY },
  { 0, -1, 0 }
};

void win32_maperr(DWORD errcode)
{
  int i;

  for (i = 0; win_error_table[i].range >= 0; i++) {
    if (errcode >= win_error_table[i].win_code &&
        errcode <= win_error_table[i].win_code + win_error_table[i].range) {
      errno = win_error_table[i].posix_code;
      return;
    }
  }
  /* Not found: save original error code, negated so that we can
     recognize it in unix_error_message */
  errno = -errcode;
}

/* Windows socket errors */

#define EWOULDBLOCK             -WSAEWOULDBLOCK
#define EINPROGRESS             -WSAEINPROGRESS
#define EALREADY                -WSAEALREADY
#define ENOTSOCK                -WSAENOTSOCK
#define EDESTADDRREQ            -WSAEDESTADDRREQ
#define EMSGSIZE                -WSAEMSGSIZE
#define EPROTOTYPE              -WSAEPROTOTYPE
#define ENOPROTOOPT             -WSAENOPROTOOPT
#define EPROTONOSUPPORT         -WSAEPROTONOSUPPORT
#define ESOCKTNOSUPPORT         -WSAESOCKTNOSUPPORT
#define EOPNOTSUPP              -WSAEOPNOTSUPP
#define EPFNOSUPPORT            -WSAEPFNOSUPPORT
#define EAFNOSUPPORT            -WSAEAFNOSUPPORT
#define EADDRINUSE              -WSAEADDRINUSE
#define EADDRNOTAVAIL           -WSAEADDRNOTAVAIL
#define ENETDOWN                -WSAENETDOWN
#define ENETUNREACH             -WSAENETUNREACH
#define ENETRESET               -WSAENETRESET
#define ECONNABORTED            -WSAECONNABORTED
#define ECONNRESET              -WSAECONNRESET
#define ENOBUFS                 -WSAENOBUFS
#define EISCONN                 -WSAEISCONN
#define ENOTCONN                -WSAENOTCONN
#define ESHUTDOWN               -WSAESHUTDOWN
#define ETOOMANYREFS            -WSAETOOMANYREFS
#define ETIMEDOUT               -WSAETIMEDOUT
#define ECONNREFUSED            -WSAECONNREFUSED
#define ELOOP                   -WSAELOOP
#define EHOSTDOWN               -WSAEHOSTDOWN
#define EHOSTUNREACH            -WSAEHOSTUNREACH
#define EPROCLIM                -WSAEPROCLIM
#define EUSERS                  -WSAEUSERS
#define EDQUOT                  -WSAEDQUOT
#define ESTALE                  -WSAESTALE
#define EREMOTE                 -WSAEREMOTE

#define EOVERFLOW -ERROR_ARITHMETIC_OVERFLOW
#define EACCESS EACCES

int error_table[] = {
  E2BIG, EACCESS, EAGAIN, EBADF, EBUSY, ECHILD, EDEADLK, EDOM,
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

void unix_error(int errcode, char *cmdname, value cmdarg)
{
  value res;
  value name = Val_unit, err = Val_unit, arg = Val_unit;
  int errconstr;

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

void uerror(cmdname, cmdarg)
     char * cmdname;
     value cmdarg;
{
  unix_error(errno, cmdname, cmdarg);
}
