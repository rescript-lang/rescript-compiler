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
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_FCHMOD

CAMLprim value unix_fchown(value fd, value uid, value gid)
{
  int result;
  caml_enter_blocking_section();
  result = fchown(Int_val(fd), Int_val(uid), Int_val(gid));
  caml_leave_blocking_section();
  if (result == -1) uerror("fchown", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_fchown(value fd, value uid, value gid)
{ invalid_argument("fchown not implemented"); }

#endif
