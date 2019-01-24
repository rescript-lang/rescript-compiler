/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                David Allsopp, MetaStack Solutions Ltd.                 */
/*                                                                        */
/*   Copyright 2015 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "unixsupport.h"
#include <errno.h>
#include <winioctl.h>

CAMLprim value unix_readlink(value opath)
{
  CAMLparam1(opath);
  CAMLlocal1(result);
  HANDLE h;
  wchar_t* path;
  DWORD attributes;
  caml_unix_check_path(opath, "readlink");
  path = caml_stat_strdup_to_utf16(String_val(opath));

  caml_enter_blocking_section();
  attributes = GetFileAttributes(path);
  caml_leave_blocking_section();

  if (attributes == INVALID_FILE_ATTRIBUTES) {
    caml_stat_free(path);
    win32_maperr(GetLastError());
    uerror("readlink", opath);
  }
  else if (!(attributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
    caml_stat_free(path);
    errno = EINVAL;
    uerror("readlink", opath);
  }
  else {
    caml_enter_blocking_section();
    if ((h = CreateFile(path,
                        FILE_READ_ATTRIBUTES,
                        FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
                        NULL,
                        OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
                        NULL)) == INVALID_HANDLE_VALUE) {
      caml_leave_blocking_section();
      caml_stat_free(path);
      errno = ENOENT;
      uerror("readlink", opath);
    }
    else {
      char buffer[16384];
      DWORD read;
      REPARSE_DATA_BUFFER* point;

      caml_stat_free(path);

      if (DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, NULL, 0, buffer, 16384, &read, NULL)) {
        caml_leave_blocking_section();
        point = (REPARSE_DATA_BUFFER*)buffer;
        if (point->ReparseTag == IO_REPARSE_TAG_SYMLINK) {
          int cbLen = point->SymbolicLinkReparseBuffer.SubstituteNameLength / sizeof(WCHAR);
          int len;
          len = win_wide_char_to_multi_byte(point->SymbolicLinkReparseBuffer.PathBuffer + point->SymbolicLinkReparseBuffer.SubstituteNameOffset / sizeof(WCHAR), cbLen, NULL, 0);
          result = caml_alloc_string(len);
          win_wide_char_to_multi_byte(point->SymbolicLinkReparseBuffer.PathBuffer + point->SymbolicLinkReparseBuffer.SubstituteNameOffset / sizeof(WCHAR),
            cbLen,
            String_val(result),
            len);
          CloseHandle(h);
        }
        else {
          errno = EINVAL;
          CloseHandle(h);
          uerror("readline", opath);
        }
      }
      else {
        caml_leave_blocking_section();
        win32_maperr(GetLastError());
        CloseHandle(h);
        uerror("readlink", opath);
      }
    }
  }

  CAMLreturn(result);
}
