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

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

CAMLprim value unix_inet_addr_of_string(value s)
{
#if defined(HAS_IPV6)
#ifdef _WIN32
  CAMLparam1(s);
  CAMLlocal1(vres);
  struct addrinfo hints;
  struct addrinfo * res;
  int retcode;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_flags = AI_NUMERICHOST;
  retcode = getaddrinfo(String_val(s), NULL, &hints, &res);
  if (retcode != 0) failwith("inet_addr_of_string");
  switch (res->ai_addr->sa_family) {
  case AF_INET:
    {
      vres =
        alloc_inet_addr(&((struct sockaddr_in *) res->ai_addr)->sin_addr);
      break;
    }
  case AF_INET6:
    {
      vres =
        alloc_inet6_addr(&((struct sockaddr_in6 *) res->ai_addr)->sin6_addr);
      break;
    }
  default:
    {
      freeaddrinfo(res);
      failwith("inet_addr_of_string");
    }
  }
  freeaddrinfo(res);
  CAMLreturn (vres);
#else
  struct in_addr address;
  struct in6_addr address6;
  if (inet_pton(AF_INET, String_val(s), &address) > 0)
    return alloc_inet_addr(&address);
  else if (inet_pton(AF_INET6, String_val(s), &address6) > 0)
    return alloc_inet6_addr(&address6);
  else
    failwith("inet_addr_of_string");
#endif
#elif defined(HAS_INET_ATON)
  struct in_addr address;
  if (inet_aton(String_val(s), &address) == 0)
    failwith("inet_addr_of_string");
  return alloc_inet_addr(&address);
#else
  struct in_addr address;
  address.s_addr = inet_addr(String_val(s));
  if (address.s_addr == (uint32) -1) failwith("inet_addr_of_string");
  return alloc_inet_addr(&address);
#endif
}

#else

CAMLprim value unix_inet_addr_of_string(value s)
{ invalid_argument("inet_addr_of_string not implemented"); }

#endif
