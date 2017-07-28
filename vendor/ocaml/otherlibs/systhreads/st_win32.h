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

/* Win32 implementation of the "st" interface */

#define _WIN32_WINNT 0x0400
#include <windows.h>
#include <winerror.h>
#include <stdio.h>
#include <signal.h>

#define INLINE __inline

#if 1
#define TRACE(x)
#define TRACE1(x,y)
#else
#include <stdio.h>
#define TRACE(x) printf("%d: %s\n", GetCurrentThreadId(), x); fflush(stdout)
#define TRACE1(x,y) printf("%d: %s %p\n", GetCurrentThreadId(), x, (void *)y); \
                    fflush(stdout)
#endif

typedef DWORD st_retcode;

#define SIGPREEMPTION SIGTERM

/* Thread-local storage assocaiting a Win32 event to every thread. */
static DWORD st_thread_sem_key;

/* OS-specific initialization */

static DWORD st_initialize(void)
{
  st_thread_sem_key = TlsAlloc();
  if (st_thread_sem_key == TLS_OUT_OF_INDEXES)
    return GetLastError();
  else
    return 0;
}

/* Thread creation.  Created in detached mode if [res] is NULL. */

typedef HANDLE st_thread_id;

static DWORD st_thread_create(st_thread_id * res,
                              LPTHREAD_START_ROUTINE fn, void * arg)
{
  HANDLE h = CreateThread(NULL, 0, fn, arg, 0, NULL);
  TRACE1("st_thread_create", h);
  if (h == NULL) return GetLastError();
  if (res == NULL)
    CloseHandle(h);
  else
    *res = h;
  return 0;
}

#define ST_THREAD_FUNCTION DWORD WINAPI

/* Cleanup at thread exit */

static void st_thread_cleanup(void)
{
  HANDLE ev = (HANDLE) TlsGetValue(st_thread_sem_key);
  if (ev != NULL) CloseHandle(ev);
}

/* Thread termination */

static void st_thread_exit(void)
{
  TRACE("st_thread_exit");
  ExitThread(0);
}

static void st_thread_kill(st_thread_id thr)
{
  TRACE1("st_thread_kill", thr);
  TerminateThread(thr, 0);
  CloseHandle(thr);
}

/* Scheduling hints */

static INLINE void st_thread_yield(void)
{
  Sleep(0);
}

/* Thread-specific state */

typedef DWORD st_tlskey;

static DWORD st_tls_newkey(st_tlskey * res)
{
  *res = TlsAlloc();
  if (*res == TLS_OUT_OF_INDEXES)
    return GetLastError();
  else
    return 0;
}

static INLINE void * st_tls_get(st_tlskey k)
{
  return TlsGetValue(k);
}

static INLINE void st_tls_set(st_tlskey k, void * v)
{
  TlsSetValue(k, v);
}

/* The master lock.  */

typedef CRITICAL_SECTION st_masterlock;

static void st_masterlock_init(st_masterlock * m)
{
  TRACE("st_masterlock_init");
  InitializeCriticalSection(m);
  EnterCriticalSection(m);
}

static INLINE void st_masterlock_acquire(st_masterlock * m)
{
  TRACE("st_masterlock_acquire");
  EnterCriticalSection(m);
  TRACE("st_masterlock_acquire (done)");
}

static INLINE void st_masterlock_release(st_masterlock * m)
{
  LeaveCriticalSection(m);
  TRACE("st_masterlock_released");
}

static INLINE int st_masterlock_waiters(st_masterlock * m)
{
  return 1;                     /* info not maintained */
}

/* Mutexes */

typedef CRITICAL_SECTION * st_mutex;

static DWORD st_mutex_create(st_mutex * res)
{
  st_mutex m = malloc(sizeof(CRITICAL_SECTION));
  if (m == NULL) return ERROR_NOT_ENOUGH_MEMORY;
  InitializeCriticalSection(m);
  *res = m;
  return 0;
}

static DWORD st_mutex_destroy(st_mutex m)
{
  DeleteCriticalSection(m);
  free(m);
  return 0;
}

static INLINE DWORD st_mutex_lock(st_mutex m)
{
  TRACE1("st_mutex_lock", m);
  EnterCriticalSection(m);
  TRACE1("st_mutex_lock (done)", m);
  return 0;
}

/* Error codes with the 29th bit set are reserved for the application */

#define PREVIOUSLY_UNLOCKED 0
#define ALREADY_LOCKED (1<<29)

static INLINE DWORD st_mutex_trylock(st_mutex m)
{
  TRACE1("st_mutex_trylock", m);
  if (TryEnterCriticalSection(m)) {
    TRACE1("st_mutex_trylock (success)", m);
    return PREVIOUSLY_UNLOCKED;
  } else {
    TRACE1("st_mutex_trylock (failure)", m);
    return ALREADY_LOCKED;
  }
}

static INLINE DWORD st_mutex_unlock(st_mutex m)
{
  TRACE1("st_mutex_unlock", m);
  LeaveCriticalSection(m);
  return 0;
}

/* Condition variables */

/* A condition variable is just a list of threads currently
   waiting on this c.v.  Each thread is represented by its
   associated event. */

struct st_wait_list {
  HANDLE event;                  /* event of the first waiting thread */
  struct st_wait_list * next;
};

typedef struct st_condvar_struct {
  CRITICAL_SECTION lock;         /* protect the data structure */
  struct st_wait_list * waiters; /* list of threads waiting */
} * st_condvar;

static DWORD st_condvar_create(st_condvar * res)
{
  st_condvar c = malloc(sizeof(struct st_condvar_struct));
  if (c == NULL) return ERROR_NOT_ENOUGH_MEMORY;
  InitializeCriticalSection(&c->lock);
  c->waiters = NULL;
  *res = c;
  return 0;
}

static DWORD st_condvar_destroy(st_condvar c)
{
  TRACE1("st_condvar_destroy", c);
  DeleteCriticalSection(&c->lock);
  free(c);
  return 0;
}

static DWORD st_condvar_signal(st_condvar c)
{
  DWORD rc = 0;
  struct st_wait_list * curr, * next;

  TRACE1("st_condvar_signal", c);
  EnterCriticalSection(&c->lock);
  curr = c->waiters;
  if (curr != NULL) {
    next = curr->next;
    /* Wake up the first waiting thread */
    TRACE1("st_condvar_signal: waking up", curr->event);
    if (! SetEvent(curr->event)) rc = GetLastError();
    /* Remove it from the waiting list */
    c->waiters = next;
  }
  LeaveCriticalSection(&c->lock);
  return rc;
}

static DWORD st_condvar_broadcast(st_condvar c)
{
  DWORD rc = 0;
  struct st_wait_list * curr, * next;

  TRACE1("st_condvar_broadcast", c);
  EnterCriticalSection(&c->lock);
  /* Wake up all waiting threads */
  curr = c->waiters;
  while (curr != NULL) {
    next = curr->next;
    TRACE1("st_condvar_signal: waking up", curr->event);
    if (! SetEvent(curr->event)) rc = GetLastError();
    curr = next;
  }
  /* Remove them all from the waiting list */
  c->waiters = NULL;
  LeaveCriticalSection(&c->lock);
  return rc;
}

static DWORD st_condvar_wait(st_condvar c, st_mutex m)
{
  HANDLE ev;
  struct st_wait_list wait;

  TRACE1("st_condvar_wait", c);
  /* Recover (or create) the event associated with the calling thread */
  ev = (HANDLE) TlsGetValue(st_thread_sem_key);
  if (ev == 0) {
    ev = CreateEvent(NULL,
                     FALSE /*auto reset*/,
                     FALSE /*initially unset*/,
                     NULL);
    if (ev == NULL) return GetLastError();
    TlsSetValue(st_thread_sem_key, (void *) ev);
  }
  EnterCriticalSection(&c->lock);
  /* Insert the current thread in the waiting list (atomically) */
  wait.event = ev;
  wait.next = c->waiters;
  c->waiters = &wait;
  LeaveCriticalSection(&c->lock);
  /* Release the mutex m */
  LeaveCriticalSection(m);
  /* Wait for our event to be signaled.  There is no risk of lost
     wakeup, since we inserted ourselves on the waiting list of c
     before releasing m */
  TRACE1("st_condvar_wait: blocking on event", ev);
  if (WaitForSingleObject(ev, INFINITE) == WAIT_FAILED)
    return GetLastError();
  /* Reacquire the mutex m */
  TRACE1("st_condvar_wait: restarted, acquiring mutex", m);
  EnterCriticalSection(m);
  TRACE1("st_condvar_wait: acquired mutex", m);
  return 0;
}

/* Triggered events */

typedef HANDLE st_event;

static DWORD st_event_create(st_event * res)
{
  st_event m =
    CreateEvent(NULL, TRUE/*manual reset*/, FALSE/*initially unset*/, NULL);
  TRACE1("st_event_create", m);
  if (m == NULL) return GetLastError();
  *res = m;
  return 0;
}

static DWORD st_event_destroy(st_event e)
{
  TRACE1("st_event_destroy", e);
  if (CloseHandle(e))
    return 0;
  else
    return GetLastError();
}

static DWORD st_event_trigger(st_event e)
{
  TRACE1("st_event_trigger", e);
  if (SetEvent(e))
    return 0;
  else
    return GetLastError();
}

static DWORD st_event_wait(st_event e)
{
  TRACE1("st_event_wait", e);
  if (WaitForSingleObject(e, INFINITE) == WAIT_FAILED)
    return GetLastError();
  else
    return 0;
}

/* Reporting errors */

static void st_check_error(DWORD retcode, char * msg)
{
  char err[1024];
  int errlen, msglen;
  value str;

  if (retcode == 0) return;
  if (retcode == ERROR_NOT_ENOUGH_MEMORY) raise_out_of_memory();
  if (! FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
                      NULL,
                      retcode,
                      0,
                      err,
                      sizeof(err),
                      NULL)) {
    sprintf(err, "error code %lx", retcode);
  }
  msglen = strlen(msg);
  errlen = strlen(err);
  str = alloc_string(msglen + 2 + errlen);
  memmove (&Byte(str, 0), msg, msglen);
  memmove (&Byte(str, msglen), ": ", 2);
  memmove (&Byte(str, msglen + 2), err, errlen);
  raise_sys_error(str);
}

/* The tick thread: posts a SIGPREEMPTION signal periodically */

static DWORD WINAPI caml_thread_tick(void * arg)
{
  while(1) {
    Sleep(Thread_timeout);
    /* The preemption signal should never cause a callback, so don't
     go through caml_handle_signal(), just record signal delivery via
     caml_record_signal(). */
    caml_record_signal(SIGPREEMPTION);
  }
  return 0;                     /* prevents compiler warning */
}

/* "At fork" processing -- none under Win32 */

static DWORD st_atfork(void (*fn)(void))
{
  return 0;
}

/* Signal handling -- none under Win32 */

value caml_thread_sigmask(value cmd, value sigs) /* ML */
{
  invalid_argument("Thread.sigmask not implemented");
  return Val_int(0);            /* not reached */
}

value caml_wait_signal(value sigs) /* ML */
{
  invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);            /* not reached */
}
