/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_UNIXSUPPORT_H
#define CAML_UNIXSUPPORT_H

#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
#include <direct.h>
#include <process.h>
#include <sys/types.h>
#include <winsock2.h>
#ifdef HAS_IPV6
#include <ws2tcpip.h>
#include <wspiapi.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;                   /* Real windows handle */
  enum { KIND_HANDLE, KIND_SOCKET } kind;
  int crt_fd;             /* C runtime descriptor */
  unsigned int flags_fd;  /* See FLAGS_FD_* */
};

#define Handle_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v) (((struct filedescr *) Data_custom_val(v))->fd.socket)
#define Descr_kind_val(v) (((struct filedescr *) Data_custom_val(v))->kind)
#define CRT_fd_val(v) (((struct filedescr *) Data_custom_val(v))->crt_fd)
#define Flags_fd_val(v) (((struct filedescr *) Data_custom_val(v))->flags_fd)

/* extern value win_alloc_handle_or_socket(HANDLE); */
extern value win_alloc_handle(HANDLE);
extern value win_alloc_socket(SOCKET);
extern int win_CRT_fd_of_filedescr(value handle);

#define NO_CRT_FD (-1)
#define Nothing ((value) 0)

extern void win32_maperr(DWORD errcode);
extern value unix_error_of_code (int errcode);
extern void unix_error (int errcode, char * cmdname, value arg);
extern void uerror (char * cmdname, value arg);
extern value unix_freeze_buffer (value);

/* Information stored in flags_fd, describing more precisely the socket
 * and its status. The whole flags_fd is initialized to 0.
 */

/* Blocking or nonblocking.  By default a filedescr is in blocking state */
#define FLAGS_FD_IS_BLOCKING (1<<0)

#define UNIX_BUFFER_SIZE 65536

#ifdef __cplusplus
}
#endif

#endif /* CAML_UNIXSUPPORT_H */
