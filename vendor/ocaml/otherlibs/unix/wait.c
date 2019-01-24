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

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#include <sys/types.h>
#include <sys/wait.h>

#if !(defined(WIFEXITED) && defined(WEXITSTATUS) && defined(WIFSTOPPED) && \
      defined(WSTOPSIG) && defined(WTERMSIG))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) (((status) & 0xFF) == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status) & 0x3F)
#endif

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int pid, int status)
{
  value st, res;

  if (WIFEXITED(status)) {
    st = caml_alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = caml_alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  }
  else {
    st = caml_alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
  }
  Begin_root (st);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = Val_int(pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

CAMLprim value unix_wait(value unit)
{
  int pid, status;

  caml_enter_blocking_section();
  pid = wait(&status);
  caml_leave_blocking_section();
  if (pid == -1) uerror("wait", Nothing);
  return alloc_process_status(pid, status);
}

#if defined(HAS_WAITPID) || defined(HAS_WAIT4)

#ifndef HAS_WAITPID
#define waitpid(pid,status,opts) wait4(pid,status,opts,NULL)
#endif

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

CAMLprim value unix_waitpid(value flags, value pid_req)
{
  int pid, status, cv_flags;

  cv_flags = caml_convert_flag_list(flags, wait_flag_table);
  caml_enter_blocking_section();
  pid = waitpid(Int_val(pid_req), &status, cv_flags);
  caml_leave_blocking_section();
  if (pid == -1) uerror("waitpid", Nothing);
  return alloc_process_status(pid, status);
}

#else

CAMLprim value unix_waitpid(value flags, value pid_req)
{ caml_invalid_argument("waitpid not implemented"); }

#endif
