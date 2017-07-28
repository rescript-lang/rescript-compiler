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
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_SELECT

#include <sys/types.h>
#include <sys/time.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <string.h>
#include <unistd.h>
#include <errno.h>

static int fdlist_to_fdset(value fdlist, fd_set *fdset, int *maxfd)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    long fd = Long_val(Field(l, 0));
    /* PR#5563: harden against bad fds */
    if (fd < 0 || fd >= FD_SETSIZE) return -1;
    FD_SET((int) fd, fdset);
    if (fd > *maxfd) *maxfd = fd;
  }
  return 0;
}

static value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value l;
  value res = Val_int(0);

  Begin_roots2(l, res);
    for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
      int fd = Int_val(Field(l, 0));
      if (FD_ISSET(fd, fdset)) {
        value newres = alloc_small(2, 0);
        Field(newres, 0) = Val_int(fd);
        Field(newres, 1) = res;
        res = newres;
      }
    }
  End_roots();
  return res;
}

CAMLprim value unix_select(value readfds, value writefds, value exceptfds,
                           value timeout)
{
  fd_set read, write, except;
  int maxfd;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;

  Begin_roots3 (readfds, writefds, exceptfds);
    maxfd = -1;
    retcode  = fdlist_to_fdset(readfds, &read, &maxfd);
    retcode += fdlist_to_fdset(writefds, &write, &maxfd);
    retcode += fdlist_to_fdset(exceptfds, &except, &maxfd);
    /* PR#5563: if a bad fd was encountered, report EINVAL error */
    if (retcode != 0) unix_error(EINVAL, "select", Nothing);
    tm = Double_val(timeout);
    if (tm < 0.0)
      tvp = (struct timeval *) NULL;
    else {
      tv.tv_sec = (int) tm;
      tv.tv_usec = (int) (1e6 * (tm - tv.tv_sec));
      tvp = &tv;
    }
    enter_blocking_section();
    retcode = select(maxfd + 1, &read, &write, &except, tvp);
    leave_blocking_section();
    if (retcode == -1) uerror("select", Nothing);
    readfds = fdset_to_fdlist(readfds, &read);
    writefds = fdset_to_fdlist(writefds, &write);
    exceptfds = fdset_to_fdlist(exceptfds, &except);
    res = alloc_small(3, 0);
    Field(res, 0) = readfds;
    Field(res, 1) = writefds;
    Field(res, 2) = exceptfds;
  End_roots();
  return res;
}

#else

CAMLprim value unix_select(value readfds, value writefds, value exceptfds,
                           value timeout)
{ invalid_argument("select not implemented"); }

#endif
