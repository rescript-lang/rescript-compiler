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

#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value unix_write(value fd, value buf, value vofs, value vlen)
{
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    while (len > 0) {
      numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
      memmove (iobuf, &Byte(buf, ofs), numbytes);
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        enter_blocking_section();
        ret = send(s, iobuf, numbytes, 0);
        if (ret == SOCKET_ERROR) err = WSAGetLastError();
        leave_blocking_section();
        numwritten = ret;
      } else {
        HANDLE h = Handle_val(fd);
        enter_blocking_section();
        if (! WriteFile(h, iobuf, numbytes, &numwritten, NULL))
          err = GetLastError();
        leave_blocking_section();
      }
      if (err) {
        win32_maperr(err);
        uerror("write", Nothing);
      }
      written += numwritten;
      ofs += numwritten;
      len -= numwritten;
    }
  End_roots();
  return Val_long(written);
}

CAMLprim value unix_single_write(value fd, value buf, value vofs, value vlen)
{
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    if (len > 0) {
      numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
      memmove (iobuf, &Byte(buf, ofs), numbytes);
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        enter_blocking_section();
        ret = send(s, iobuf, numbytes, 0);
        if (ret == SOCKET_ERROR) err = WSAGetLastError();
        leave_blocking_section();
        numwritten = ret;
      } else {
        HANDLE h = Handle_val(fd);
        enter_blocking_section();
        if (! WriteFile(h, iobuf, numbytes, &numwritten, NULL))
          err = GetLastError();
        leave_blocking_section();
      }
      if (err) {
        win32_maperr(err);
        uerror("single_write", Nothing);
      }
      written = numwritten;
    }
  End_roots();
  return Val_long(written);
}
