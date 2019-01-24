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
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include <string.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif

static int open_flag_table[15] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_NOCTTY, O_DSYNC, O_SYNC, O_RSYNC,
  0, /* O_SHARE_DELETE, Windows-only */
  0, /* O_CLOEXEC, treated specially */
  0  /* O_KEEPEXEC, treated specially */
};

enum { CLOEXEC = 1, KEEPEXEC = 2 };

static int open_cloexec_table[15] = {
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0,
  0,
  CLOEXEC, KEEPEXEC
};

CAMLprim value unix_open(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  int fd, cv_flags, clo_flags, cloexec;
  char * p;

  caml_unix_check_path(path, "open");
  cv_flags = caml_convert_flag_list(flags, open_flag_table);
  clo_flags = caml_convert_flag_list(flags, open_cloexec_table);
  if (clo_flags & CLOEXEC)
    cloexec = 1;
  else if (clo_flags & KEEPEXEC)
    cloexec = 0;
  else
    cloexec = unix_cloexec_default;
#if defined(O_CLOEXEC)
  if (cloexec) cv_flags |= O_CLOEXEC;
#endif
  p = caml_stat_strdup(String_val(path));
  /* open on a named FIFO can block (PR#1533) */
  caml_enter_blocking_section();
  fd = open(p, cv_flags, Int_val(perm));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (fd == -1) uerror("open", path);
#if !defined(O_CLOEXEC)
  if (cloexec) unix_set_cloexec(fd, "open", path);
#endif
  CAMLreturn (Val_int(fd));
}
