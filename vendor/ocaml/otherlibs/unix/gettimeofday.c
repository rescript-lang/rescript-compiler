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

#ifdef HAS_GETTIMEOFDAY

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value unix_gettimeofday(value unit)
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) uerror("gettimeofday", Nothing);
  return copy_double((double) tp.tv_sec + (double) tp.tv_usec / 1e6);
}

#else

CAMLprim value unix_gettimeofday(value unit)
{ invalid_argument("gettimeofday not implemented"); }

#endif
