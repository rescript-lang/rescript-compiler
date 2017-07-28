/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
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
#ifndef O_CLOEXEC
#define NEED_CLOEXEC_EMULATION
#define O_CLOEXEC 0
#endif

static int open_flag_table[14] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_NOCTTY, O_DSYNC, O_SYNC, O_RSYNC,
  0, /* O_SHARE_DELETE, Windows-only */
  O_CLOEXEC
};

#ifdef NEED_CLOEXEC_EMULATION
static int open_cloexec_table[14] = {
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0,
  0,
  1
};
#endif

CAMLprim value unix_open(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  int fd, cv_flags;
  char * p;

  cv_flags = convert_flag_list(flags, open_flag_table);
  p = caml_strdup(String_val(path));
  /* open on a named FIFO can block (PR#1533) */
  enter_blocking_section();
  fd = open(p, cv_flags, Int_val(perm));
  leave_blocking_section();
  stat_free(p);
  if (fd == -1) uerror("open", path);
#if defined(NEED_CLOEXEC_EMULATION) && defined(FD_CLOEXEC)
  if (convert_flag_list(flags, open_cloexec_table) != 0) {
    int flags = fcntl(fd, F_GETFD, 0);
    if (flags == -1 ||
        fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1)
      uerror("open", path);
  }
#endif
  CAMLreturn (Val_int(fd));
}
