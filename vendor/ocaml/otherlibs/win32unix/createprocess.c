/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
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
#include "unixsupport.h"
#include <windows.h>
#include <caml/osdeps.h>
#include <errno.h>

static int win_has_console(void);

static DWORD do_create_process_native(wchar_t * exefile, wchar_t * cmdline, wchar_t * env,
                                      HANDLE fd1, HANDLE fd2, HANDLE fd3, HANDLE * hProcess)
{
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  DWORD flags, err;
  HANDLE hp;

  err = ERROR_SUCCESS;
  /* Prepare stdin/stdout/stderr redirection */
  ZeroMemory(&si, sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  si.dwFlags = STARTF_USESTDHANDLES;
  /* Duplicate the handles fd1, fd2, fd3 to make sure they are inheritable */
  hp = GetCurrentProcess();
  if (! DuplicateHandle(hp, fd1, hp, &(si.hStdInput),
                        0, TRUE, DUPLICATE_SAME_ACCESS)) {
    err = GetLastError(); goto ret1;
  }
  if (! DuplicateHandle(hp, fd2, hp, &(si.hStdOutput),
                        0, TRUE, DUPLICATE_SAME_ACCESS)) {
    err = GetLastError(); goto ret2;
  }
  if (! DuplicateHandle(hp, fd3, hp, &(si.hStdError),
                        0, TRUE, DUPLICATE_SAME_ACCESS)) {
    err = GetLastError(); goto ret3;
  }
  /* If we do not have a console window, then we must create one
     before running the process (keep it hidden for apparence).
     If we are starting a GUI application, the newly created
     console should not matter. */
  if (win_has_console())
    flags = 0;
  else {
    flags = CREATE_NEW_CONSOLE;
    si.dwFlags = (STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES);
    si.wShowWindow = SW_HIDE;
  }
  flags |= CREATE_UNICODE_ENVIRONMENT;
  /* Create the process */
  if (! CreateProcess(exefile, cmdline, NULL, NULL,
                      TRUE, flags, env, NULL, &si, &pi)) {
    err = GetLastError(); goto ret4;
  }
  CloseHandle(pi.hThread);
 ret4:
  CloseHandle(si.hStdError);
 ret3:
  CloseHandle(si.hStdOutput);
 ret2:
  CloseHandle(si.hStdInput);
 ret1:
  *hProcess = (err == ERROR_SUCCESS) ? pi.hProcess : NULL;
  return err;
}

value win_create_process_native(value cmd, value cmdline, value env,
                               value fd1, value fd2, value fd3)
{
  wchar_t * exefile, * wcmdline, * wenv, * wcmd;
  HANDLE hProcess;
  DWORD err;
  int size;

  caml_unix_check_path(cmd, "create_process");
  if (! caml_string_is_c_safe(cmdline))
    unix_error(EINVAL, "create_process", cmdline);
  /* [env] is checked for null bytes at construction time, see unix.ml */

  wcmd = caml_stat_strdup_to_utf16(String_val(cmd));
  exefile = caml_search_exe_in_path(wcmd);
  caml_stat_free(wcmd);
  wcmdline = caml_stat_strdup_to_utf16(String_val(cmdline));

  if (env != Val_int(0)) {
    env = Field(env, 0);
    size = win_multi_byte_to_wide_char(String_val(env), caml_string_length(env), NULL, 0);
    wenv = caml_stat_alloc((size + 1)*sizeof(wchar_t));
    win_multi_byte_to_wide_char(String_val(env), caml_string_length(env), wenv, size);
    wenv[size] = 0;
  } else {
    wenv = NULL;
  }

  err = do_create_process_native(exefile, wcmdline, wenv,
                                 Handle_val(fd1), Handle_val(fd2), Handle_val(fd3), &hProcess);

  if (wenv != NULL) caml_stat_free(wenv);
  caml_stat_free(wcmdline);
  caml_stat_free(exefile);
  if (err != ERROR_SUCCESS) {
    win32_maperr(err);
    uerror("create_process", cmd);
  }
  /* Return the process handle as pseudo-PID
     (this is consistent with the wait() emulation in the MSVC C library */
  return Val_long(hProcess);
}

CAMLprim value win_create_process(value * argv, int argn)
{
  return win_create_process_native(argv[0], argv[1], argv[2],
                                   argv[3], argv[4], argv[5]);
}

static int win_has_console(void)
{
  HANDLE h, log;
  int i;

  h = CreateFile(L"CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
                 OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    return 0;
  } else {
    CloseHandle(h);
    return 1;
  }
}

CAMLprim value win_terminate_process(value v_pid)
{
  return (Val_bool(TerminateProcess((HANDLE) Long_val(v_pid), 0)));
}
