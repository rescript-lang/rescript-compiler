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

#include <sys/types.h>
#include <sys/stat.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_FCHMOD

CAMLprim value unix_fchmod(value fd, value perm)
{
  int result;
  caml_enter_blocking_section();
  result = fchmod(Int_val(fd), Int_val(perm));
  caml_leave_blocking_section();
  if (result == -1) uerror("fchmod", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_fchmod(value fd, value perm)
{ invalid_argument("fchmod not implemented"); }

#endif
