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

#define _GNU_SOURCE
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

CAMLprim value unix_socket(value cloexec, value domain,
                           value type, value proto)
{
  int retcode;
  int ty = socket_type_table[Int_val(type)];
#ifdef SOCK_CLOEXEC
  if (unix_cloexec_p(cloexec)) ty |= SOCK_CLOEXEC;
#endif
  retcode = socket(socket_domain_table[Int_val(domain)],
                   ty, Int_val(proto));
  if (retcode == -1) uerror("socket", Nothing);
#ifndef SOCK_CLOEXEC
  if (unix_cloexec_p(cloexec))
    unix_set_cloexec(retcode, "socket", Nothing);
#endif
  return Val_int(retcode);
}

#else

CAMLprim value unix_socket(value cloexec, value domain,
                           value type,value proto)
{ caml_invalid_argument("socket not implemented"); }

#endif
