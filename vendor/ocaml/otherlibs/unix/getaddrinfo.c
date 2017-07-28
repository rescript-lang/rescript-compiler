/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2004 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include "cst2constr.h"

#if defined(HAS_SOCKETS) && defined(HAS_IPV6)

#include "socketaddr.h"
#ifndef _WIN32
#include <sys/types.h>
#include <netdb.h>
#endif

extern int socket_domain_table[]; /* from socket.c */
extern int socket_type_table[];   /* from socket.c */

static value convert_addrinfo(struct addrinfo * a)
{
  CAMLparam0();
  CAMLlocal3(vres,vaddr,vcanonname);
  union sock_addr_union sa;
  socklen_param_type len;

  len = a->ai_addrlen;
  if (len > sizeof(sa)) len = sizeof(sa);
  memcpy(&sa.s_gen, a->ai_addr, len);
  vaddr = alloc_sockaddr(&sa, len, -1);
  vcanonname = copy_string(a->ai_canonname == NULL ? "" : a->ai_canonname);
  vres = alloc_small(5, 0);
  Field(vres, 0) = cst_to_constr(a->ai_family, socket_domain_table, 3, 0);
  Field(vres, 1) = cst_to_constr(a->ai_socktype, socket_type_table, 4, 0);
  Field(vres, 2) = Val_int(a->ai_protocol);
  Field(vres, 3) = vaddr;
  Field(vres, 4) = vcanonname;
  CAMLreturn(vres);
}

CAMLprim value unix_getaddrinfo(value vnode, value vserv, value vopts)
{
  CAMLparam3(vnode, vserv, vopts);
  CAMLlocal3(vres, v, e);
  char * node, * serv;
  struct addrinfo hints;
  struct addrinfo * res, * r;
  int retcode;

  /* Extract "node" parameter */
  if (caml_string_length(vnode) == 0) {
    node = NULL;
  } else {
    node = caml_strdup(String_val(vnode));
  }
  /* Extract "service" parameter */
  if (caml_string_length(vserv) == 0) {
    serv = NULL;
  } else {
    serv = caml_strdup(String_val(vserv));
  }
  /* Parse options, set hints */
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  for (/*nothing*/; Is_block(vopts); vopts = Field(vopts, 1)) {
    v = Field(vopts, 0);
    if (Is_block(v))
      switch (Tag_val(v)) {
      case 0:                   /* AI_FAMILY of socket_domain */
        hints.ai_family = socket_domain_table[Int_val(Field(v, 0))];
        break;
      case 1:                   /* AI_SOCKTYPE of socket_type */
        hints.ai_socktype = socket_type_table[Int_val(Field(v, 0))];
        break;
      case 2:                   /* AI_PROTOCOL of int */
        hints.ai_protocol = Int_val(Field(v, 0));
        break;
      }
    else
      switch (Int_val(v)) {
      case 0:                   /* AI_NUMERICHOST */
        hints.ai_flags |= AI_NUMERICHOST; break;
      case 1:                   /* AI_CANONNAME */
        hints.ai_flags |= AI_CANONNAME; break;
      case 2:                   /* AI_PASSIVE */
        hints.ai_flags |= AI_PASSIVE; break;
      }
  }
  /* Do the call */
  enter_blocking_section();
  retcode = getaddrinfo(node, serv, &hints, &res);
  leave_blocking_section();
  if (node != NULL) stat_free(node);
  if (serv != NULL) stat_free(serv);
  /* Convert result */
  vres = Val_int(0);
  if (retcode == 0) {
    for (r = res; r != NULL; r = r->ai_next) {
      e = convert_addrinfo(r);
      v = alloc_small(2, 0);
      Field(v, 0) = e;
      Field(v, 1) = vres;
      vres = v;
    }
    freeaddrinfo(res);
  }
  CAMLreturn(vres);
}

#else

CAMLprim value unix_getaddrinfo(value vnode, value vserv, value vopts)
{ invalid_argument("getaddrinfo not implemented"); }

#endif
