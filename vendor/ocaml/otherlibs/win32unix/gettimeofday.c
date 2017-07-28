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
#include <time.h>

#include "unixsupport.h"

/* Unix epoch as a Windows timestamp in hundreds of ns */
#define epoch_ft 116444736000000000.0;

CAMLprim value unix_gettimeofday(value unit)
{
  FILETIME ft;
  double tm;
  GetSystemTimeAsFileTime(&ft);
  tm = *(uint64 *)&ft - epoch_ft; /* shift to Epoch-relative time */
  return copy_double(tm * 1e-7);  /* tm is in 100ns */
}
