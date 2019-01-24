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

#include <caml/mlvalues.h>
#include "unixsupport.h"

int socket_domain_table[] = {
  PF_UNIX, PF_INET,
#if defined(HAS_IPV6)
  PF_INET6
#else
  0
#endif
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

CAMLprim value unix_socket(value cloexec, value domain, value type, value proto)
{
  SOCKET s;

  #ifndef HAS_IPV6
  /* IPv6 requires WinSock2, we must raise an error on PF_INET6 */
  if (Int_val(domain) >= sizeof(socket_domain_table)/sizeof(int)) {
    win32_maperr(WSAEPFNOSUPPORT);
    uerror("socket", Nothing);
  }
  #endif

  s = socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto));
  if (s == INVALID_SOCKET) {
    win32_maperr(WSAGetLastError());
    uerror("socket", Nothing);
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) s,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);
  return win_alloc_socket(s);
}
