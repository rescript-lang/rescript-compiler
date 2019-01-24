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

CAMLnoreturn_start
extern void unix_error (int errcode, const char * cmdname, value arg)
CAMLnoreturn_end;

CAMLnoreturn_start
extern void uerror (const char * cmdname, value arg)
CAMLnoreturn_end;

extern void caml_unix_check_path(value path, const char * cmdname);
extern value unix_freeze_buffer (value);
extern wchar_t ** cstringvect(value arg, char * cmdname);
extern void cstringvect_free(wchar_t **);

extern int unix_cloexec_default;
extern int unix_cloexec_p(value cloexec);

/* Information stored in flags_fd, describing more precisely the socket
 * and its status. The whole flags_fd is initialized to 0.
 */

/* Blocking or nonblocking.  By default a filedescr is in blocking state */
#define FLAGS_FD_IS_BLOCKING (1<<0)

#define UNIX_BUFFER_SIZE 65536

#ifdef __cplusplus
}
#endif

/*
 * This structure is defined inconsistently. mingw64 has it in ntdef.h (which
 * doesn't look like a primary header) and technically it's part of ntifs.h in
 * the WDK. Requiring the WDK is a bit extreme, so the definition is taken from
 * ntdef.h. Both ntdef.h and ntifs.h define REPARSE_DATA_BUFFER_HEADER_SIZE
 */
#ifndef REPARSE_DATA_BUFFER_HEADER_SIZE
typedef struct _REPARSE_DATA_BUFFER
{
  ULONG  ReparseTag;
  USHORT ReparseDataLength;
  USHORT Reserved;
  union
  {
    struct
    {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      ULONG  Flags;
      WCHAR  PathBuffer[1];
    } SymbolicLinkReparseBuffer;
    struct
    {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      WCHAR  PathBuffer[1];
    } MountPointReparseBuffer;
    struct
    {
      UCHAR  DataBuffer[1];
    } GenericReparseBuffer;
  };
} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;
#endif

#define EXECV_CAST (const char_os * const *)

#endif /* CAML_UNIXSUPPORT_H */
