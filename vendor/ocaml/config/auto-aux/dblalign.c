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
#include <signal.h>
#include <setjmp.h>

double foo;

void access_double(double *p)
{
  foo = *p;
}

jmp_buf failure;

void sig_handler(int sig)
{
  longjmp(failure, 1);
}

int main(void)
{
  long n[10];
  int res;
  signal(SIGSEGV, sig_handler);
#ifdef SIGBUS
  signal(SIGBUS, sig_handler);
#endif
  if(setjmp(failure) == 0) {
    access_double((double *) n);
    access_double((double *) (n+1));
    res = 0;
  } else {
    res = 1;
  }
  signal(SIGSEGV, SIG_DFL);
#ifdef SIGBUS
  signal(SIGBUS, SIG_DFL);
#endif
  exit(res);
}
