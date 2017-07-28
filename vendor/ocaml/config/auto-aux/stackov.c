/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <signal.h>
#include <sys/resource.h>

static char sig_alt_stack[SIGSTKSZ];
static char * system_stack_top;

#if defined(TARGET_i386) && defined(SYS_linux_elf)
static void segv_handler(int signo, struct sigcontext sc)
{
  char * fault_addr = (char *) sc.cr2;
#else
static void segv_handler(int signo, siginfo_t * info, void * context)
{
  char * fault_addr = (char *) info->si_addr;
#endif
  struct rlimit limit;

  if (getrlimit(RLIMIT_STACK, &limit) == 0 &&
      ((long) fault_addr & (sizeof(long) - 1)) == 0 &&
      fault_addr < system_stack_top &&
      fault_addr >= system_stack_top - limit.rlim_cur - 0x2000) {
    _exit(0);
  } else {
    _exit(4);
  }
}

int main(int argc, char ** argv)
{
  stack_t stk;
  struct sigaction act;

  stk.ss_sp = sig_alt_stack;
  stk.ss_size = SIGSTKSZ;
  stk.ss_flags = 0;
#if defined(TARGET_i386) && defined(SYS_linux_elf)
  act.sa_handler = (void (*)(int)) segv_handler;
  act.sa_flags = SA_ONSTACK | SA_NODEFER;
#else
  act.sa_sigaction = segv_handler;
  act.sa_flags = SA_SIGINFO | SA_ONSTACK | SA_NODEFER;
#endif
  sigemptyset(&act.sa_mask);
  system_stack_top = (char *) &act;
  if (sigaltstack(&stk, NULL) != 0) { perror("sigaltstack"); return 2; }
  if (sigaction(SIGSEGV, &act, NULL) != 0) { perror("sigaction"); return 2; }
  /* We used to trigger a stack overflow at this point to test whether
     the code above works, but this causes problems with POSIX threads
     on some BSD systems.  So, instead, we just test that all this
     code compiles, indicating that the required syscalls are there. */
  return 0;
}
