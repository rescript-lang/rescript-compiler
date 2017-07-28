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

#include <sys/socket.h>

CAMLprim value unix_listen(value sock, value backlog)
{
  if (listen(Int_val(sock), Int_val(backlog)) == -1) uerror("listen", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_listen(value sock, value backlog)
{ invalid_argument("listen not implemented"); }

#endif
