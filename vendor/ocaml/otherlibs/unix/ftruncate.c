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
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/io.h>
#include <caml/signals.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#ifdef HAS_TRUNCATE

CAMLprim value unix_ftruncate(value fd, value len)
{
  int result;
  caml_enter_blocking_section();
  result = ftruncate(Int_val(fd), Long_val(len));
  caml_leave_blocking_section();
  if (result == -1) uerror("ftruncate", Nothing);
  return Val_unit;
}

CAMLprim value unix_ftruncate_64(value fd, value len)
{
  int result;
  file_offset ofs = File_offset_val(len);
  caml_enter_blocking_section();
  result = ftruncate(Int_val(fd), ofs);
  caml_leave_blocking_section();
  if (result == -1) uerror("ftruncate", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_ftruncate(value fd, value len)
{ invalid_argument("ftruncate not implemented"); }

CAMLprim value unix_ftruncate_64(value fd, value len)
{ invalid_argument("ftruncate not implemented"); }

#endif
