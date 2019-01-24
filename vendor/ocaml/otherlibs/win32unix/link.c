/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  File contributed by Lionel Fourquaux                  */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include "unixsupport.h"
#include <windows.h>

typedef
BOOL (WINAPI *tCreateHardLink)(
  LPCWSTR lpFileName,
  LPCWSTR lpExistingFileName,
  LPSECURITY_ATTRIBUTES lpSecurityAttributes
);

CAMLprim value unix_link(value path1, value path2)
{
  HMODULE hModKernel32;
  tCreateHardLink pCreateHardLink;
  BOOL result;
  wchar_t * wpath1, * wpath2;
  hModKernel32 = GetModuleHandle(L"KERNEL32.DLL");
  pCreateHardLink =
    (tCreateHardLink) GetProcAddress(hModKernel32, "CreateHardLinkW");
  if (pCreateHardLink == NULL)
    caml_invalid_argument("Unix.link not implemented");
  caml_unix_check_path(path1, "link");
  caml_unix_check_path(path2, "link");

  wpath1 = caml_stat_strdup_to_utf16(String_val(path1));
  wpath2 = caml_stat_strdup_to_utf16(String_val(path2));

  result = pCreateHardLink(wpath2, wpath1, NULL);

  caml_stat_free(wpath1);
  caml_stat_free(wpath2);

  if (! result) {
    win32_maperr(GetLastError());
    uerror("link", path2);
  }
  return Val_unit;
}
