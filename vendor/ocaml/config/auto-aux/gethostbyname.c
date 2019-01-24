/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef _REENTRANT
/* This helps detection on Digital Unix... */
#define _REENTRANT
#endif

#include <sys/types.h>
#include <netdb.h>

#ifdef SYS_netbsd
#error "this OS doesn't have gethostbyname_r"
#endif

int main(int argc, char ** argv)
{
#if NUM_ARGS == 5
  struct hostent *hp;
  struct hostent h;
  char buffer[1000];
  int h_errno;
  hp = gethostbyname_r("www.caml.org", &h, buffer, 10, &h_errno);
#elif NUM_ARGS == 6
  struct hostent *hp;
  struct hostent h;
  char buffer[1000];
  int h_errno;
  int rc;
  rc = gethostbyname_r("www.caml.org", &h, buffer, 10, &hp, &h_errno);
#endif
  return 0;
}
