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
#include <caml/memory.h>
#include "unixsupport.h"
#include <time.h>
#include <sys/types.h>
#include <sys/times.h>
#ifdef HAS_GETRUSAGE
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifndef CLK_TCK
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK 60
#endif
#endif

CAMLprim value unix_times(value unit)
{
#ifdef HAS_GETRUSAGE

  value res;
  struct rusage ru;

  res = alloc_small(4 * Double_wosize, Double_array_tag);

  getrusage (RUSAGE_SELF, &ru);
  Store_double_field (res, 0, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field (res, 1, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);
  getrusage (RUSAGE_CHILDREN, &ru);
  Store_double_field (res, 2, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field (res, 3, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);
  return res;

#else

  value res;
  struct tms buffer;

  times(&buffer);
  res = alloc_small(4 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, (double) buffer.tms_utime / CLK_TCK);
  Store_double_field(res, 1, (double) buffer.tms_stime / CLK_TCK);
  Store_double_field(res, 2, (double) buffer.tms_cutime / CLK_TCK);
  Store_double_field(res, 3, (double) buffer.tms_cstime / CLK_TCK);
  return res;

#endif
}
