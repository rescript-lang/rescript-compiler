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
#include <caml/signals.h>
#include "unixsupport.h"

#if defined(HAS_SOCKETS) && defined(HAS_IPV6)

#include "socketaddr.h"
#ifndef _WIN32
#include <sys/types.h>
#include <netdb.h>
#endif

static int getnameinfo_flag_table[] = {
  NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD, NI_NUMERICSERV, NI_DGRAM
};

CAMLprim value unix_getnameinfo(value vaddr, value vopts)
{
  CAMLparam0();
  CAMLlocal3(vhost, vserv, vres);
  union sock_addr_union addr;
  socklen_param_type addr_len;
  char host[4096];
  char serv[1024];
  int opts, retcode;

  get_sockaddr(vaddr, &addr, &addr_len);
  opts = convert_flag_list(vopts, getnameinfo_flag_table);
  enter_blocking_section();
  retcode =
    getnameinfo((const struct sockaddr *) &addr.s_gen, addr_len,
                host, sizeof(host), serv, sizeof(serv), opts);
  leave_blocking_section();
  if (retcode != 0) raise_not_found(); /* TODO: detailed error reporting? */
  vhost = copy_string(host);
  vserv = copy_string(serv);
  vres = alloc_small(2, 0);
  Field(vres, 0) = vhost;
  Field(vres, 1) = vserv;
  CAMLreturn(vres);
}

#else

CAMLprim value unix_getnameinfo(value vaddr, value vopts)
{ invalid_argument("getnameinfo not implemented"); }

#endif
