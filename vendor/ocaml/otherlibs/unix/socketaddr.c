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

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <errno.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

#ifdef _WIN32
#undef EAFNOSUPPORT
#define EAFNOSUPPORT WSAEAFNOSUPPORT
#endif

CAMLexport value alloc_inet_addr(struct in_addr * a)
{
  value res;
  /* Use a string rather than an abstract block so that it can be
     marshaled safely.  Remember that a is in network byte order,
     hence is marshaled in an endian-independent manner. */
  res = caml_alloc_initialized_string(4, (char *)a);
  return res;
}

#ifdef HAS_IPV6

CAMLexport value alloc_inet6_addr(struct in6_addr * a)
{
  value res;
  res = caml_alloc_initialized_string(16, (char *)a);
  return res;
}

#endif

void get_sockaddr(value mladr,
                  union sock_addr_union * adr /*out*/,
                  socklen_param_type * adr_len /*out*/)
{
  switch(Tag_val(mladr)) {
#ifndef _WIN32
  case 0:                       /* ADDR_UNIX */
    { value path;
      mlsize_t len;
      path = Field(mladr, 0);
      len = caml_string_length(path);
      adr->s_unix.sun_family = AF_UNIX;
      if (len >= sizeof(adr->s_unix.sun_path)) {
        unix_error(ENAMETOOLONG, "", path);
      }
      /* "Abstract" sockets in Linux have names starting with '\0' */
      if (Byte(path, 0) != 0 && ! caml_string_is_c_safe(path)) {
        unix_error(ENOENT, "", path);
      }
      memmove (adr->s_unix.sun_path, String_val(path), len + 1);
      *adr_len =
        ((char *)&(adr->s_unix.sun_path) - (char *)&(adr->s_unix))
        + len;
      break;
    }
#endif
  case 1:                       /* ADDR_INET */
#ifdef HAS_IPV6
    if (caml_string_length(Field(mladr, 0)) == 16) {
      memset(&adr->s_inet6, 0, sizeof(struct sockaddr_in6));
      adr->s_inet6.sin6_family = AF_INET6;
      adr->s_inet6.sin6_addr = GET_INET6_ADDR(Field(mladr, 0));
      adr->s_inet6.sin6_port = htons(Int_val(Field(mladr, 1)));
#ifdef SIN6_LEN
      adr->s_inet6.sin6_len = sizeof(struct sockaddr_in6);
#endif
      *adr_len = sizeof(struct sockaddr_in6);
      break;
    }
#endif
    memset(&adr->s_inet, 0, sizeof(struct sockaddr_in));
    adr->s_inet.sin_family = AF_INET;
    adr->s_inet.sin_addr = GET_INET_ADDR(Field(mladr, 0));
    adr->s_inet.sin_port = htons(Int_val(Field(mladr, 1)));
#ifdef SIN6_LEN
    adr->s_inet.sin_len = sizeof(struct sockaddr_in);
#endif
    *adr_len = sizeof(struct sockaddr_in);
    break;
  }
}

value alloc_sockaddr(union sock_addr_union * adr /*in*/,
                     socklen_param_type adr_len, int close_on_error)
{
  value res;
  switch(adr->s_gen.sa_family) {
#ifndef _WIN32
  case AF_UNIX:
    { value n;
      /* Based on recommendation in section BUGS of Linux unix(7). See
         http://man7.org/linux/man-pages/man7/unix.7.html */
      mlsize_t path_length =
        strnlen(adr->s_unix.sun_path,
                adr_len - offsetof(struct sockaddr_un, sun_path));
      n = caml_alloc_initialized_string(path_length, (char *)adr->s_unix.sun_path);
      Begin_root (n);
        res = caml_alloc_small(1, 0);
        Field(res,0) = n;
      End_roots();
      break;
    }
#endif
  case AF_INET:
    { value a = alloc_inet_addr(&adr->s_inet.sin_addr);
      Begin_root (a);
        res = caml_alloc_small(2, 1);
        Field(res,0) = a;
        Field(res,1) = Val_int(ntohs(adr->s_inet.sin_port));
      End_roots();
      break;
    }
#ifdef HAS_IPV6
  case AF_INET6:
    { value a = alloc_inet6_addr(&adr->s_inet6.sin6_addr);
      Begin_root (a);
        res = caml_alloc_small(2, 1);
        Field(res,0) = a;
        Field(res,1) = Val_int(ntohs(adr->s_inet6.sin6_port));
      End_roots();
      break;
    }
#endif
  default:
    if (close_on_error != -1) close (close_on_error);
    unix_error(EAFNOSUPPORT, "", Nothing);
  }
  return res;
}

#endif
