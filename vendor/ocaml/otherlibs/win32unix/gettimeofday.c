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
#if defined(_MSC_VER) && _MSC_VER < 1300
  /* This compiler can't cast uint64_t to double! Fortunately, this doesn't
     matter since SYSTEMTIME is only ever 63-bit (maximum value 31-Dec-30827
     23:59:59.999, and it requires some skill to set the clock past 2099!)
   */
  tm = *(int64_t *)&ft - epoch_ft; /* shift to Epoch-relative time */
#else
  tm = *(uint64_t *)&ft - epoch_ft; /* shift to Epoch-relative time */
#endif
  return caml_copy_double(tm * 1e-7);  /* tm is in 100ns */
}
