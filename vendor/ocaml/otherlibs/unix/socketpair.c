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
#include <caml/alloc.h>
#include <caml/fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/socket.h>

extern int socket_domain_table[], socket_type_table[];

CAMLprim value unix_socketpair(value domain, value type, value proto)
{
  int sv[2];
  value res;
  if (socketpair(socket_domain_table[Int_val(domain)],
                 socket_type_table[Int_val(type)],
                 Int_val(proto), sv) == -1)
    uerror("socketpair", Nothing);
  res = alloc_small(2, 0);
  Field(res,0) = Val_int(sv[0]);
  Field(res,1) = Val_int(sv[1]);
  return res;
}

#else

CAMLprim value unix_socketpair(value domain, value type, value proto)
{ invalid_argument("socketpair not implemented"); }

#endif
