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

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value unix_read(value fd, value buf, value ofs, value vlen)
{
  intnat len;
  DWORD numbytes, numread;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  Begin_root (buf);
    len = Long_val(vlen);
    numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
    if (Descr_kind_val(fd) == KIND_SOCKET) {
      int ret;
      SOCKET s = Socket_val(fd);
      enter_blocking_section();
      ret = recv(s, iobuf, numbytes, 0);
      if (ret == SOCKET_ERROR) err = WSAGetLastError();
      leave_blocking_section();
      numread = ret;
    } else {
      HANDLE h = Handle_val(fd);
      enter_blocking_section();
      if (! ReadFile(h, iobuf, numbytes, &numread, NULL))
        err = GetLastError();
      leave_blocking_section();
    }
    if (err) {
      win32_maperr(err);
      uerror("read", Nothing);
    }
    memmove (&Byte(buf, Long_val(ofs)), iobuf, numread);
  End_roots();
  return Val_int(numread);
}
