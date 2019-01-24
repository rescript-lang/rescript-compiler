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
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include "socketaddr.h"

CAMLprim value unix_accept(value cloexec, value sock)
{
  SOCKET sconn = Socket_val(sock);
  SOCKET snew;
  value fd = Val_unit, adr = Val_unit, res;
  union sock_addr_union addr;
  socklen_param_type addr_len;
  DWORD err = 0;

  addr_len = sizeof(sock_addr);
  caml_enter_blocking_section();
  snew = accept(sconn, &addr.s_gen, &addr_len);
  if (snew == INVALID_SOCKET) err = WSAGetLastError ();
  caml_leave_blocking_section();
  if (snew == INVALID_SOCKET) {
    win32_maperr(err);
    uerror("accept", Nothing);
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) snew,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);
  Begin_roots2 (fd, adr)
    fd = win_alloc_socket(snew);
    adr = alloc_sockaddr(&addr, addr_len, snew);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = fd;
    Field(res, 1) = adr;
  End_roots();
  return res;
}
