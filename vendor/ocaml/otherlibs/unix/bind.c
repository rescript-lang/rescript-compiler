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

#include "socketaddr.h"

CAMLprim value unix_bind(value socket, value address)
{
  int ret;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  get_sockaddr(address, &addr, &addr_len);
  ret = bind(Int_val(socket), &addr.s_gen, addr_len);
  if (ret == -1) uerror("bind", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_bind(value socket, value address)
{ invalid_argument("bind not implemented"); }

#endif
