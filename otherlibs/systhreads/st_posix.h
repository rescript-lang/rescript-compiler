/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2009 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* POSIX thread implementation of the "st" interface */

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#ifdef __sun
#define _POSIX_PTHREAD_SEMANTICS
#endif
#include <signal.h>
#include <sys/time.h>
#ifdef __linux__
#include <unistd.h>
#endif

#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif

typedef int st_retcode;

#define SIGPREEMPTION SIGVTALRM

/* OS-specific initialization */

static int st_initialize(void)
{
  return 0;
}

/* Thread creation.  Created in detached mode if [res] is NULL. */

typedef pthread_t st_thread_id;

static int st_thread_create(st_thread_id * res,
                            void * (*fn)(void *), void * arg)
{
  pthread_t thr;
  pthread_attr_t attr;
  int rc;

  pthread_attr_init(&attr);
  if (res == NULL) pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  rc = pthread_create(&thr, &attr, fn, arg);
  if (res != NULL) *res = thr;
  return rc;
}

#define ST_THREAD_FUNCTION void *

/* Cleanup at thread exit */

static INLINE void st_thread_cleanup(void)
{
  return;
}

/* Thread termination */

static void st_thread_exit(void)
{
  pthread_exit(NULL);
}

static void st_thread_kill(st_thread_id thr)
{
#if !defined(__ANDROID__)
  /* pthread_cancel is unsafe, as it does not allow the thread an opportunity
     to free shared resources such as mutexes. Thus, it is not implemented
     in Android's libc. */
  pthread_cancel(thr);
#endif
}

/* Scheduling hints */

static void INLINE st_thread_yield(void)
{
#ifndef __linux__
  /* sched_yield() doesn't do what we want in Linux 2.6 and up (PR#2663) */
  sched_yield();
#endif
}

/* Thread-specific state */

typedef pthread_key_t st_tlskey;

static int st_tls_newkey(st_tlskey * res)
{
  return pthread_key_create(res, NULL);
}

static INLINE void * st_tls_get(st_tlskey k)
{
  return pthread_getspecific(k);
}

static INLINE void st_tls_set(st_tlskey k, void * v)
{
  pthread_setspecific(k, v);
}

/* The master lock.  This is a mutex that is held most of the time,
   so we implement it in a slightly consoluted way to avoid
   all risks of busy-waiting.  Also, we count the number of waiting
   threads. */

typedef struct {
  pthread_mutex_t lock;         /* to protect contents  */
  int busy;                     /* 0 = free, 1 = taken */
  volatile int waiters;         /* number of threads waiting on master lock */
  pthread_cond_t is_free;       /* signaled when free */
} st_masterlock;

static void st_masterlock_init(st_masterlock * m)
{
  pthread_mutex_init(&m->lock, NULL);
  pthread_cond_init(&m->is_free, NULL);
  m->busy = 1;
  m->waiters = 0;
}

static void st_masterlock_acquire(st_masterlock * m)
{
  pthread_mutex_lock(&m->lock);
  while (m->busy) {
    m->waiters ++;
    pthread_cond_wait(&m->is_free, &m->lock);
    m->waiters --;
  }
  m->busy = 1;
  pthread_mutex_unlock(&m->lock);
}

static void st_masterlock_release(st_masterlock * m)
{
  pthread_mutex_lock(&m->lock);
  m->busy = 0;
  pthread_mutex_unlock(&m->lock);
  pthread_cond_signal(&m->is_free);
}

static INLINE int st_masterlock_waiters(st_masterlock * m)
{
  return m->waiters;
}

/* Mutexes */

typedef pthread_mutex_t * st_mutex;

static int st_mutex_create(st_mutex * res)
{
  int rc;
  st_mutex m = malloc(sizeof(pthread_mutex_t));
  if (m == NULL) return ENOMEM;
  rc = pthread_mutex_init(m, NULL);
  if (rc != 0) { free(m); return rc; }
  *res = m;
  return 0;
}

static int st_mutex_destroy(st_mutex m)
{
  int rc;
  rc = pthread_mutex_destroy(m);
  free(m);
  return rc;
}

static INLINE int st_mutex_lock(st_mutex m)
{
  return pthread_mutex_lock(m);
}

#define PREVIOUSLY_UNLOCKED 0
#define ALREADY_LOCKED EBUSY

static INLINE int st_mutex_trylock(st_mutex m)
{
  return pthread_mutex_trylock(m);
}

static INLINE int st_mutex_unlock(st_mutex m)
{
  return pthread_mutex_unlock(m);
}

/* Condition variables */

typedef pthread_cond_t * st_condvar;

static int st_condvar_create(st_condvar * res)
{
  int rc;
  st_condvar c = malloc(sizeof(pthread_cond_t));
  if (c == NULL) return ENOMEM;
  rc = pthread_cond_init(c, NULL);
  if (rc != 0) { free(c); return rc; }
  *res = c;
  return 0;
}

static int st_condvar_destroy(st_condvar c)
{
  int rc;
  rc = pthread_cond_destroy(c);
  free(c);
  return rc;
}

static INLINE int st_condvar_signal(st_condvar c)
{
  return pthread_cond_signal(c);
}

static INLINE int st_condvar_broadcast(st_condvar c)
{
  return pthread_cond_broadcast(c);
}

static INLINE int st_condvar_wait(st_condvar c, st_mutex m)
{
  return pthread_cond_wait(c, m);
}

/* Triggered events */

typedef struct st_event_struct {
  pthread_mutex_t lock;         /* to protect contents */
  int status;                   /* 0 = not triggered, 1 = triggered */
  pthread_cond_t triggered;     /* signaled when triggered */
} * st_event;

static int st_event_create(st_event * res)
{
  int rc;
  st_event e = malloc(sizeof(struct st_event_struct));
  if (e == NULL) return ENOMEM;
  rc = pthread_mutex_init(&e->lock, NULL);
  if (rc != 0) { free(e); return rc; }
  rc = pthread_cond_init(&e->triggered, NULL);
  if (rc != 0) { pthread_mutex_destroy(&e->lock); free(e); return rc; }
  e->status = 0;
  *res = e;
  return 0;
}

static int st_event_destroy(st_event e)
{
  int rc1, rc2;
  rc1 = pthread_mutex_destroy(&e->lock);
  rc2 = pthread_cond_destroy(&e->triggered);
  free(e);
  return rc1 != 0 ? rc1 : rc2;
}

static int st_event_trigger(st_event e)
{
  int rc;
  rc = pthread_mutex_lock(&e->lock);
  if (rc != 0) return rc;
  e->status = 1;
  rc = pthread_mutex_unlock(&e->lock);
  if (rc != 0) return rc;
  rc = pthread_cond_broadcast(&e->triggered);
  return rc;
}

static int st_event_wait(st_event e)
{
  int rc;
  rc = pthread_mutex_lock(&e->lock);
  if (rc != 0) return rc;
  while(e->status == 0) {
    rc = pthread_cond_wait(&e->triggered, &e->lock);
    if (rc != 0) return rc;
  }
  rc = pthread_mutex_unlock(&e->lock);
  return rc;
}

/* Reporting errors */

static void st_check_error(int retcode, char * msg)
{
  char * err;
  int errlen, msglen;
  value str;

  if (retcode == 0) return;
  if (retcode == ENOMEM) raise_out_of_memory();
  err = strerror(retcode);
  msglen = strlen(msg);
  errlen = strlen(err);
  str = alloc_string(msglen + 2 + errlen);
  memmove (&Byte(str, 0), msg, msglen);
  memmove (&Byte(str, msglen), ": ", 2);
  memmove (&Byte(str, msglen + 2), err, errlen);
  raise_sys_error(str);
}

/* The tick thread: posts a SIGPREEMPTION signal periodically */

static void * caml_thread_tick(void * arg)
{
  struct timeval timeout;
  sigset_t mask;

  /* Block all signals so that we don't try to execute an OCaml signal handler*/
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, NULL);
#if !defined(__ANDROID__)
  /* Allow async cancellation */
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
#endif
  while(1) {
    /* select() seems to be the most efficient way to suspend the
       thread for sub-second intervals */
    timeout.tv_sec = 0;
    timeout.tv_usec = Thread_timeout * 1000;
    select(0, NULL, NULL, NULL, &timeout);
    /* The preemption signal should never cause a callback, so don't
     go through caml_handle_signal(), just record signal delivery via
     caml_record_signal(). */
    caml_record_signal(SIGPREEMPTION);
  }
  return NULL;                  /* prevents compiler warning */
}

/* "At fork" processing */

#if defined(__ANDROID__)
/* Android's libc does not include declaration of pthread_atfork;
   however, it implements it since API level 10 (Gingerbread).
   The reason for the omission is that Android (GUI) applications
   are not supposed to fork at all, however this workaround is still
   included in case OCaml is used for an Android CLI utility. */
int pthread_atfork(void (*prepare)(void), void (*parent)(void), void (*child)(void));
#endif

static int st_atfork(void (*fn)(void))
{
  return pthread_atfork(NULL, NULL, fn);
}

/* Signal handling */

static void st_decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

#ifndef NSIG
#define NSIG 64
#endif

static value st_encode_sigset(sigset_t * set)
{
  value res = Val_int(0);
  int i;

  Begin_root(res)
    for (i = 1; i < NSIG; i++)
      if (sigismember(set, i) > 0) {
        value newcons = alloc_small(2, 0);
        Field(newcons, 0) = Val_int(caml_rev_convert_signal_number(i));
        Field(newcons, 1) = res;
        res = newcons;
      }
  End_roots();
  return res;
}

static int sigmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };

value caml_thread_sigmask(value cmd, value sigs) /* ML */
{
  int how;
  sigset_t set, oldset;
  int retcode;

  how = sigmask_cmd[Int_val(cmd)];
  st_decode_sigset(sigs, &set);
  enter_blocking_section();
  retcode = pthread_sigmask(how, &set, &oldset);
  leave_blocking_section();
  st_check_error(retcode, "Thread.sigmask");
  return st_encode_sigset(&oldset);
}

value caml_wait_signal(value sigs) /* ML */
{
#ifdef HAS_SIGWAIT
  sigset_t set;
  int retcode, signo;

  st_decode_sigset(sigs, &set);
  enter_blocking_section();
  retcode = sigwait(&set, &signo);
  leave_blocking_section();
  st_check_error(retcode, "Thread.wait_signal");
  return Val_int(signo);
#else
  invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);            /* not reached */
#endif
}
