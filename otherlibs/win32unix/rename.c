/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Contributed by Tracy Camp, PolyServe Inc., <campt@polyserve.com>   */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <caml/mlvalues.h>
#include "unixsupport.h"

CAMLprim value unix_rename(value path1, value path2)
{
  static int supports_MoveFileEx = -1; /* don't know yet */
  BOOL ok;

  if (supports_MoveFileEx < 0) {
    OSVERSIONINFO VersionInfo;
    VersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    supports_MoveFileEx =
      (GetVersionEx(&VersionInfo) != 0)
      && (VersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT);
  }
  if (supports_MoveFileEx > 0)
    ok = MoveFileEx(String_val(path1), String_val(path2),
                    MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
                    MOVEFILE_COPY_ALLOWED);
  else
    ok = MoveFile(String_val(path1), String_val(path2));
  if (! ok) {
    win32_maperr(GetLastError());
    uerror("rename", path1);
  }
  return Val_unit;
}
