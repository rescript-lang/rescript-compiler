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

#ifndef CAML_SOCKETADDR_H
#define CAML_SOCKETADDR_H

#include "caml/misc.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>

union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
#ifdef HAS_IPV6
  struct sockaddr_in6 s_inet6;
#endif
};

#ifdef HAS_SOCKLEN_T
typedef socklen_t socklen_param_type;
#else
typedef int socklen_param_type;
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern void get_sockaddr (value mladdr,
                          union sock_addr_union * addr /*out*/,
                          socklen_param_type * addr_len /*out*/);
CAMLexport value alloc_sockaddr (union sock_addr_union * addr /*in*/,
                      socklen_param_type addr_len, int close_on_error);
CAMLexport value alloc_inet_addr (struct in_addr * inaddr);
#define GET_INET_ADDR(v) (*((struct in_addr *) (v)))

#ifdef HAS_IPV6
CAMLexport value alloc_inet6_addr (struct in6_addr * inaddr);
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))
#endif

#ifdef __cplusplus
}
#endif

#endif /* CAML_SOCKETADDR_H */
