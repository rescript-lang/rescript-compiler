/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*    Pascal Cuoq and Xavier Leroy, projet Cristal, INRIA Rocquencourt    */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <errno.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

CAMLprim value win_findfirst(value name)
{
  HANDLE h;
  value v;
  WIN32_FIND_DATAW fileinfo;
  value valname = Val_unit;
  value valh = Val_unit;
  wchar_t * wname;

  caml_unix_check_path(name, "opendir");
  Begin_roots2 (valname,valh);
    wname = caml_stat_strdup_to_utf16(String_val(name));
    h = FindFirstFile(wname,&fileinfo);
    caml_stat_free(wname);
    if (h == INVALID_HANDLE_VALUE) {
      DWORD err = GetLastError();
      if (err == ERROR_NO_MORE_FILES)
        caml_raise_end_of_file();
      else {
        win32_maperr(err);
        uerror("opendir", Nothing);
      }
    }
    valname = caml_copy_string_of_utf16(fileinfo.cFileName);
    valh = win_alloc_handle(h);
    v = caml_alloc_small(2, 0);
    Field(v,0) = valname;
    Field(v,1) = valh;
  End_roots();
  return v;
}

CAMLprim value win_findnext(value valh)
{
  WIN32_FIND_DATAW fileinfo;
  BOOL retcode;

  retcode = FindNextFile(Handle_val(valh), &fileinfo);
  if (!retcode) {
    DWORD err = GetLastError();
    if (err == ERROR_NO_MORE_FILES)
      caml_raise_end_of_file();
    else {
      win32_maperr(err);
      uerror("readdir", Nothing);
    }
  }
  return caml_copy_string_of_utf16(fileinfo.cFileName);
}

CAMLprim value win_findclose(value valh)
{
  if (! FindClose(Handle_val(valh))) {
    win32_maperr(GetLastError());
    uerror("closedir", Nothing);
  }
  return Val_unit;
}
