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

#ifdef HAS_SOCKETS

#include <sys/types.h>
#include <sys/socket.h>

int socket_domain_table[] = {
  PF_UNIX, PF_INET,
#if defined(HAS_IPV6)
  PF_INET6
#elif defined(PF_UNDEF)
  PF_UNDEF
#else
  0
#endif
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

CAMLprim value unix_socket(value domain, value type, value proto)
{
  int retcode;
  retcode = socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto));
  if (retcode == -1) uerror("socket", Nothing);
  return Val_int(retcode);

}

#else

CAMLprim value unix_socket(value domain, value type, value proto)
{ invalid_argument("socket not implemented"); }

#endif
