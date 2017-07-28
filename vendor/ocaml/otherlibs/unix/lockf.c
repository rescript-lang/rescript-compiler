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

#include <errno.h>
#include <fcntl.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include "unixsupport.h"

#if defined(F_GETLK) && defined(F_SETLK) && defined(F_SETLKW)

CAMLprim value unix_lockf(value fd, value cmd, value span)
{
  struct flock l;
  int ret;
  int fildes;
  long size;

  fildes = Int_val(fd);
  size = Long_val(span);
  l.l_whence = 1;
  if (size < 0) {
    l.l_start = size;
    l.l_len = -size;
  } else {
    l.l_start = 0L;
    l.l_len = size;
  }
  switch (Int_val(cmd)) {
  case 0: /* F_ULOCK */
    l.l_type = F_UNLCK;
    ret = fcntl(fildes, F_SETLK, &l);
    break;
  case 1: /* F_LOCK */
    l.l_type = F_WRLCK;
    enter_blocking_section();
    ret = fcntl(fildes, F_SETLKW, &l);
    leave_blocking_section();
    break;
  case 2: /* F_TLOCK */
    l.l_type = F_WRLCK;
    ret = fcntl(fildes, F_SETLK, &l);
    break;
  case 3: /* F_TEST */
    l.l_type = F_WRLCK;
    ret = fcntl(fildes, F_GETLK, &l);
    if (ret != -1) {
      if (l.l_type == F_UNLCK)
        ret = 0;
      else {
        errno = EACCES;
        ret = -1;
      }
    }
    break;
  case 4: /* F_RLOCK */
    l.l_type = F_RDLCK;
    enter_blocking_section();
    ret = fcntl(fildes, F_SETLKW, &l);
    leave_blocking_section();
    break;
  case 5: /* F_TRLOCK */
    l.l_type = F_RDLCK;
    ret = fcntl(fildes, F_SETLK, &l);
    break;
  default:
    errno = EINVAL;
    ret = -1;
  }
  if (ret == -1) uerror("lockf", Nothing);
  return Val_unit;
}

#else

#ifdef HAS_LOCKF
#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define F_ULOCK 0
#define F_LOCK 1
#define F_TLOCK 2
#define F_TEST 3
#endif

static int lock_command_table[] = {
  F_ULOCK, F_LOCK, F_TLOCK, F_TEST, F_LOCK, F_TLOCK
};

CAMLprim value unix_lockf(value fd, value cmd, value span)
{
  if (lockf(Int_val(fd), lock_command_table[Int_val(cmd)], Long_val(span))
      == -1) uerror("lockf", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_lockf(value fd, value cmd, value span)
{ invalid_argument("lockf not implemented"); }

#endif
#endif
