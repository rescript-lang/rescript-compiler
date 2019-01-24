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
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

CAMLprim value unix_accept(value cloexec, value sock)
{
  int retcode;
  value res;
  value a;
  union sock_addr_union addr;
  socklen_param_type addr_len;
  int clo = unix_cloexec_p(cloexec);

  addr_len = sizeof(addr);
  caml_enter_blocking_section();
#if defined(HAS_ACCEPT4) && defined(SOCK_CLOEXEC)
  retcode = accept4(Int_val(sock), &addr.s_gen, &addr_len,
                    clo ? SOCK_CLOEXEC : 0);
#else
  retcode = accept(Int_val(sock), &addr.s_gen, &addr_len);
#endif
  caml_leave_blocking_section();
  if (retcode == -1) uerror("accept", Nothing);
#if !(defined(HAS_ACCEPT4) && defined(SOCK_CLOEXEC))
  if (clo) unix_set_cloexec(retcode, "accept", Nothing);
#endif
  a = alloc_sockaddr(&addr, addr_len, retcode);
  Begin_root (a);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = Val_int(retcode);
    Field(res, 1) = a;
  End_roots();
  return res;
}

#else

CAMLprim value unix_accept(value cloexec, value sock)
{ caml_invalid_argument("accept not implemented"); }

#endif
