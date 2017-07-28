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

#include <caml/mlvalues.h>
#include "unixsupport.h"
#include <mswsock.h>   // for SO_OPENTYPE and SO_SYNCHRONOUS_NONALERT

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

CAMLprim value unix_socket(domain, type, proto)
     value domain, type, proto;
{
  SOCKET s;
  int oldvalue, oldvaluelen, newvalue, retcode;

  #ifndef HAS_IPV6
  /* IPv6 requires WinSock2, we must raise an error on PF_INET6 */
  if (Int_val(domain) >= sizeof(socket_domain_table)/sizeof(int)) {
    win32_maperr(WSAEPFNOSUPPORT);
    uerror("socket", Nothing);
  }
  #endif

  oldvaluelen = sizeof(oldvalue);
  retcode = getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
                       (char *) &oldvalue, &oldvaluelen);
  if (retcode == 0) {
    /* Set sockets to synchronous mode */
    newvalue = SO_SYNCHRONOUS_NONALERT;
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
               (char *) &newvalue, sizeof(newvalue));
  }
  s = socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto));
  if (retcode == 0) {
    /* Restore initial mode */
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
               (char *) &oldvalue, oldvaluelen);
  }
  if (s == INVALID_SOCKET) {
    win32_maperr(WSAGetLastError());
    uerror("socket", Nothing);
  }
  return win_alloc_socket(s);
}
