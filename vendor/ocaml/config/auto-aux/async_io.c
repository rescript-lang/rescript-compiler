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

#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "s.h"

int signalled;

void sigio_handler(int arg)
{
  signalled = 1;
}

int main(void)
{
#if defined(SIGIO) && defined(FASYNC) && defined(F_SETFL) && defined(F_SETOWN)
  int p[2];
  int ret;
#define OUT 0
#define IN 1
  if (socketpair(PF_UNIX, SOCK_STREAM, 0, p) == -1) return 1;
  signalled = 0;
  signal(SIGIO, sigio_handler);
  ret = fcntl(p[OUT], F_GETFL, 0);
  fcntl(p[OUT], F_SETFL, ret | FASYNC);
  fcntl(p[OUT], F_SETOWN, getpid());
  switch(fork()) {
  case -1:
    return 1;
  case 0:
    close(p[OUT]);
    write(p[IN], "x", 1);
    sleep(1);
    exit(0);
  default:
    close(p[IN]);
    while(wait(NULL) == -1 && errno == EINTR) /*nothing*/;
  }
  if (signalled) return 0; else return 1;
#else
  return 1;
#endif
}
