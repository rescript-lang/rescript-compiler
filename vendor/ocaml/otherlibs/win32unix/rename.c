/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Tracy Camp, PolyServe Inc., <campt@polyserve.com>     */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>
#include <caml/memory.h>
#include "unixsupport.h"

CAMLprim value unix_rename(value path1, value path2)
{
  wchar_t * wpath1, * wpath2;
  BOOL ok;

  caml_unix_check_path(path1, "rename");
  caml_unix_check_path(path2, "rename");
  wpath1 = caml_stat_strdup_to_utf16(String_val(path1));
  wpath2 = caml_stat_strdup_to_utf16(String_val(path2));
  ok = MoveFileEx(wpath1, wpath2,
                  MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
                  MOVEFILE_COPY_ALLOWED);
  caml_stat_free(wpath1);
  caml_stat_free(wpath2);
  if (! ok) {
    win32_maperr(GetLastError());
    uerror("rename", path1);
  }
  return Val_unit;
}
