/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
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

/* Signal handling, code common to the bytecode and native systems */

#include <signal.h>
#include <errno.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/signals_machdep.h"
#include "caml/sys.h"

#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
#include "caml/spacetime.h"
#endif

#ifndef NSIG
#define NSIG 64
#endif

/* The set of pending signals (received but not yet processed) */

CAMLexport intnat volatile caml_signals_are_pending = 0;
CAMLexport intnat volatile caml_pending_signals[NSIG];

/* Execute all pending signals */

void caml_process_pending_signals(void)
{
  int i;

  if (caml_signals_are_pending) {
    caml_signals_are_pending = 0;
    for (i = 0; i < NSIG; i++) {
      if (caml_pending_signals[i]) {
        caml_pending_signals[i] = 0;
        caml_execute_signal(i, 0);
      }
    }
  }
}

/* Record the delivery of a signal, and arrange for it to be processed
   as soon as possible:
   - in bytecode: via caml_something_to_do, processed in caml_process_event
   - in native-code: by playing with the allocation limit, processed
       in caml_garbage_collection
*/

void caml_record_signal(int signal_number)
{
  caml_pending_signals[signal_number] = 1;
  caml_signals_are_pending = 1;
#ifndef NATIVE_CODE
  caml_something_to_do = 1;
#else
  caml_young_limit = caml_young_alloc_end;
#endif
}

/* Management of blocking sections. */

static intnat volatile caml_async_signal_mode = 0;

static void caml_enter_blocking_section_default(void)
{
  CAMLassert (caml_async_signal_mode == 0);
  caml_async_signal_mode = 1;
}

static void caml_leave_blocking_section_default(void)
{
  CAMLassert (caml_async_signal_mode == 1);
  caml_async_signal_mode = 0;
}

static int caml_try_leave_blocking_section_default(void)
{
  intnat res;
  Read_and_clear(res, caml_async_signal_mode);
  return res;
}

CAMLexport void (*caml_enter_blocking_section_hook)(void) =
   caml_enter_blocking_section_default;
CAMLexport void (*caml_leave_blocking_section_hook)(void) =
   caml_leave_blocking_section_default;
CAMLexport int (*caml_try_leave_blocking_section_hook)(void) =
   caml_try_leave_blocking_section_default;

CAMLexport void caml_enter_blocking_section(void)
{
  while (1){
    /* Process all pending signals now */
    caml_process_pending_signals();
    caml_enter_blocking_section_hook ();
    /* Check again for pending signals.
       If none, done; otherwise, try again */
    if (! caml_signals_are_pending) break;
    caml_leave_blocking_section_hook ();
  }
}

CAMLexport void caml_leave_blocking_section(void)
{
  int saved_errno;
  /* Save the value of errno (PR#5982). */
  saved_errno = errno;
  caml_leave_blocking_section_hook ();
  caml_process_pending_signals();
  errno = saved_errno;
}

/* Execute a signal handler immediately */

static value caml_signal_handlers = 0;

void caml_execute_signal(int signal_number, int in_signal_handler)
{
  value res;
  value handler;
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  void* saved_spacetime_trie_node_ptr;
#endif
#ifdef POSIX_SIGNALS
  sigset_t nsigs, sigs;
  /* Block the signal before executing the handler, and record in sigs
     the original signal mask */
  sigemptyset(&nsigs);
  sigaddset(&nsigs, signal_number);
  sigprocmask(SIG_BLOCK, &nsigs, &sigs);
#endif
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  /* We record the signal handler's execution separately, in the same
     trie used for finalisers. */
  saved_spacetime_trie_node_ptr
    = caml_spacetime_trie_node_ptr;
  caml_spacetime_trie_node_ptr
    = caml_spacetime_finaliser_trie_root;
#endif
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  /* Handled action may have no associated handler, which we interpret
     as meaning the signal should be handled by a call to exit.  This is
     is used to allow spacetime profiles to be completed on interrupt */
  if (caml_signal_handlers == 0) {
    res = caml_sys_exit(Val_int(2));
  } else {
    handler = Field(caml_signal_handlers, signal_number);
    if (!Is_block(handler)) {
      res = caml_sys_exit(Val_int(2));
    } else {
#else
  handler = Field(caml_signal_handlers, signal_number);
#endif
    res = caml_callback_exn(
             handler,
             Val_int(caml_rev_convert_signal_number(signal_number)));
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
    }
  }
  caml_spacetime_trie_node_ptr = saved_spacetime_trie_node_ptr;
#endif
#ifdef POSIX_SIGNALS
  if (! in_signal_handler) {
    /* Restore the original signal mask */
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  } else if (Is_exception_result(res)) {
    /* Restore the original signal mask and unblock the signal itself */
    sigdelset(&sigs, signal_number);
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  }
#endif
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
}

/* Arrange for a garbage collection to be performed as soon as possible */

int volatile caml_requested_major_slice = 0;
int volatile caml_requested_minor_gc = 0;

void caml_request_major_slice (void)
{
  caml_requested_major_slice = 1;
#ifndef NATIVE_CODE
  caml_something_to_do = 1;
#else
  caml_young_limit = caml_young_alloc_end;
  /* This is only moderately effective on ports that cache [caml_young_limit]
     in a register, since [caml_modify] is called directly, not through
     [caml_c_call], so it may take a while before the register is reloaded
     from [caml_young_limit]. */
#endif
}

void caml_request_minor_gc (void)
{
  caml_requested_minor_gc = 1;
#ifndef NATIVE_CODE
  caml_something_to_do = 1;
#else
  caml_young_limit = caml_young_alloc_end;
  /* Same remark as above in [caml_request_major_slice]. */
#endif
}

/* OS-independent numbering of signals */

#ifndef SIGABRT
#define SIGABRT -1
#endif
#ifndef SIGALRM
#define SIGALRM -1
#endif
#ifndef SIGFPE
#define SIGFPE -1
#endif
#ifndef SIGHUP
#define SIGHUP -1
#endif
#ifndef SIGILL
#define SIGILL -1
#endif
#ifndef SIGINT
#define SIGINT -1
#endif
#ifndef SIGKILL
#define SIGKILL -1
#endif
#ifndef SIGPIPE
#define SIGPIPE -1
#endif
#ifndef SIGQUIT
#define SIGQUIT -1
#endif
#ifndef SIGSEGV
#define SIGSEGV -1
#endif
#ifndef SIGTERM
#define SIGTERM -1
#endif
#ifndef SIGUSR1
#define SIGUSR1 -1
#endif
#ifndef SIGUSR2
#define SIGUSR2 -1
#endif
#ifndef SIGCHLD
#define SIGCHLD -1
#endif
#ifndef SIGCONT
#define SIGCONT -1
#endif
#ifndef SIGSTOP
#define SIGSTOP -1
#endif
#ifndef SIGTSTP
#define SIGTSTP -1
#endif
#ifndef SIGTTIN
#define SIGTTIN -1
#endif
#ifndef SIGTTOU
#define SIGTTOU -1
#endif
#ifndef SIGVTALRM
#define SIGVTALRM -1
#endif
#ifndef SIGPROF
#define SIGPROF -1
#endif
#ifndef SIGBUS
#define SIGBUS -1
#endif
#ifndef SIGPOLL
#define SIGPOLL -1
#endif
#ifndef SIGSYS
#define SIGSYS -1
#endif
#ifndef SIGTRAP
#define SIGTRAP -1
#endif
#ifndef SIGURG
#define SIGURG -1
#endif
#ifndef SIGXCPU
#define SIGXCPU -1
#endif
#ifndef SIGXFSZ
#define SIGXFSZ -1
#endif

static int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF, SIGBUS,
  SIGPOLL, SIGSYS, SIGTRAP, SIGURG, SIGXCPU, SIGXFSZ
};

CAMLexport int caml_convert_signal_number(int signo)
{
  if (signo < 0 && signo >= -(sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}

CAMLexport int caml_rev_convert_signal_number(int signo)
{
  int i;
  for (i = 0; i < sizeof(posix_signals) / sizeof(int); i++)
    if (signo == posix_signals[i]) return -i - 1;
  return signo;
}

/* Installation of a signal handler (as per [Sys.signal]) */

CAMLprim value caml_install_signal_handler(value signal_number, value action)
{
  CAMLparam2 (signal_number, action);
  CAMLlocal1 (res);
  int sig, act, oldact;

  sig = caml_convert_signal_number(Int_val(signal_number));
  if (sig < 0 || sig >= NSIG)
    caml_invalid_argument("Sys.signal: unavailable signal");
  switch(action) {
  case Val_int(0):              /* Signal_default */
    act = 0;
    break;
  case Val_int(1):              /* Signal_ignore */
    act = 1;
    break;
  default:                      /* Signal_handle */
    act = 2;
    break;
  }
  oldact = caml_set_signal_action(sig, act);
  switch (oldact) {
  case 0:                       /* was Signal_default */
    res = Val_int(0);
    break;
  case 1:                       /* was Signal_ignore */
    res = Val_int(1);
    break;
  case 2:                       /* was Signal_handle */
    #if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
      /* Handled action may have no associated handler
         which we treat as Signal_default */
      if (caml_signal_handlers == 0) {
        res = Val_int(0);
      } else {
        if (!Is_block(Field(caml_signal_handlers, sig))) {
          res = Val_int(0);
        } else {
          res = caml_alloc_small (1, 0);
          Field(res, 0) = Field(caml_signal_handlers, sig);
        }
      }
    #else
    res = caml_alloc_small (1, 0);
    Field(res, 0) = Field(caml_signal_handlers, sig);
    #endif
    break;
  default:                      /* error in caml_set_signal_action */
    caml_sys_error(NO_ARG);
  }
  if (Is_block(action)) {
    if (caml_signal_handlers == 0) {
      caml_signal_handlers = caml_alloc(NSIG, 0);
      caml_register_global_root(&caml_signal_handlers);
    }
    caml_modify(&Field(caml_signal_handlers, sig), Field(action, 0));
  }
  caml_process_pending_signals();
  CAMLreturn (res);
}
