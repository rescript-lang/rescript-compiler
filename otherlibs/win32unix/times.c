/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 File contributed by Josh Berdine                    */
/*                                                                     */
/*  Copyright 2011 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"
#include <windows.h>


double to_sec(FILETIME ft) {
  ULARGE_INTEGER tmp;

  tmp.u.LowPart = ft.dwLowDateTime;
  tmp.u.HighPart = ft.dwHighDateTime;

  /* convert to seconds:
     GetProcessTimes returns number of 100-nanosecond intervals */
  return tmp.QuadPart / 1e7;
}


value unix_times(value unit) {
  value res;
  FILETIME creation, exit, stime, utime;

  if (!(GetProcessTimes(GetCurrentProcess(), &creation, &exit, &stime,
                        &utime))) {
    win32_maperr(GetLastError());
    uerror("times", Nothing);
  }

  res = alloc_small(4 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, to_sec(utime));
  Store_double_field(res, 1, to_sec(stime));
  Store_double_field(res, 2, 0);
  Store_double_field(res, 3, 0);
  return res;
}
