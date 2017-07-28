/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*   Pascal Cuoq and Xavier Leroy, projet Cristal, INRIA Rocquencourt  */
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
#include <caml/signals.h>
#include "unixsupport.h"
#include <windows.h>
#include <sys/types.h>

static value alloc_process_status(HANDLE pid, int status)
{
  value res, st;

  st = alloc(1, 0);
  Field(st, 0) = Val_int(status);
  Begin_root (st);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_long((intnat) pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

enum { CAML_WNOHANG = 1, CAML_WUNTRACED = 2 };

static int wait_flag_table[] = { CAML_WNOHANG, CAML_WUNTRACED };

CAMLprim value win_waitpid(value vflags, value vpid_req)
{
  int flags;
  DWORD status, retcode;
  HANDLE pid_req = (HANDLE) Long_val(vpid_req);
  DWORD err = 0;

  flags = convert_flag_list(vflags, wait_flag_table);
  if ((flags & CAML_WNOHANG) == 0) {
    enter_blocking_section();
    retcode = WaitForSingleObject(pid_req, INFINITE);
    if (retcode == WAIT_FAILED) err = GetLastError();
    leave_blocking_section();
    if (err) {
      win32_maperr(err);
      uerror("waitpid", Nothing);
    }
  }
  if (! GetExitCodeProcess(pid_req, &status)) {
    win32_maperr(GetLastError());
    uerror("waitpid", Nothing);
  }
  if (status == STILL_ACTIVE)
    return alloc_process_status((HANDLE) 0, 0);
  else {
    CloseHandle(pid_req);
    return alloc_process_status(pid_req, status);
  }
}
