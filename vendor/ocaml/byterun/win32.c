/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Win32-specific stuff */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include "caml/address_class.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/sys.h"

#include <flexdll.h>

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * caml_decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = caml_strdup(path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != ';'; n++) /*nothing*/;
    caml_ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

char * caml_search_in_path(struct ext_table * path, char * name)
{
  char * p, * dir, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    dir = path->contents[i];
    if (dir[0] == 0) continue;
         /* not sure what empty path components mean under Windows */
    fullname = caml_strconcat(3, dir, "\\", name);
    caml_gc_message(0x100, "Searching %s\n", (uintnat) fullname);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode))
      return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  caml_gc_message(0x100, "%s not found in search path\n", (uintnat) name);
  return caml_strdup(name);
}

CAMLexport char * caml_search_exe_in_path(char * name)
{
  char * fullname, * filepart;
  size_t fullnamelen;
  DWORD retcode;

  fullnamelen = strlen(name) + 1;
  if (fullnamelen < 256) fullnamelen = 256;
  while (1) {
    fullname = caml_stat_alloc(fullnamelen);
    retcode = SearchPath(NULL,              /* use system search path */
                         name,
                         ".exe",            /* add .exe extension if needed */
                         fullnamelen,
                         fullname,
                         &filepart);
    if (retcode == 0) {
      caml_gc_message(0x100, "%s not found in search path\n",
                      (uintnat) name);
      caml_stat_free(fullname);
      return caml_strdup(name);
    }
    if (retcode < fullnamelen)
      return fullname;
    caml_stat_free(fullname);
    fullnamelen = retcode + 1;
  }
}

char * caml_search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname;
  char * res;

  dllname = caml_strconcat(2, name, ".dll");
  res = caml_search_in_path(path, dllname);
  caml_stat_free(dllname);
  return res;
}

void * caml_dlopen(char * libname, int for_execution, int global)
{
  void *handle;
  int flags = (global ? FLEXDLL_RTLD_GLOBAL : 0);
  if (!for_execution) flags |= FLEXDLL_RTLD_NOEXEC;
  handle = flexdll_dlopen(libname, flags);
  if ((handle != NULL) && ((caml_verb_gc & 0x100) != 0)) {
    flexdll_dump_exports(handle);
    fflush(stdout);
  }
  return handle;
}

void caml_dlclose(void * handle)
{
  flexdll_dlclose(handle);
}

void * caml_dlsym(void * handle, char * name)
{
  return flexdll_dlsym(handle, name);
}

void * caml_globalsym(char * name)
{
  return flexdll_dlsym(flexdll_dlopen(NULL,0), name);
}

char * caml_dlerror(void)
{
  return flexdll_dlerror();
}

/* Proper emulation of signal(), including ctrl-C and ctrl-break */

typedef void (*sighandler)(int sig);
static int ctrl_handler_installed = 0;
static volatile sighandler ctrl_handler_action = SIG_DFL;

static BOOL WINAPI ctrl_handler(DWORD event)
{
  int saved_mode;

  /* Only ctrl-C and ctrl-Break are handled */
  if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
  /* Default behavior is to exit, which we get by not handling the event */
  if (ctrl_handler_action == SIG_DFL) return FALSE;
  /* Ignore behavior is to do nothing, which we get by claiming that we
     have handled the event */
  if (ctrl_handler_action == SIG_IGN) return TRUE;
  /* Win32 doesn't like it when we do a longjmp() at this point
     (it looks like we're running in a different thread than
     the main program!).  So, just record the signal. */
  caml_record_signal(SIGINT);
  /* We have handled the event */
  return TRUE;
}

sighandler caml_win32_signal(int sig, sighandler action)
{
  sighandler oldaction;

  if (sig != SIGINT) return signal(sig, action);
  if (! ctrl_handler_installed) {
    SetConsoleCtrlHandler(ctrl_handler, TRUE);
    ctrl_handler_installed = 1;
  }
  oldaction = ctrl_handler_action;
  ctrl_handler_action = action;
  return oldaction;
}

/* Expansion of @responsefile and *? file patterns in the command line */

static int argc;
static char ** argv;
static int argvsize;

static void store_argument(char * arg);
static void expand_argument(char * arg);
static void expand_pattern(char * arg);

static void out_of_memory(void)
{
  fprintf(stderr, "Out of memory while expanding command line\n");
  exit(2);
}

static void store_argument(char * arg)
{
  if (argc + 1 >= argvsize) {
    argvsize *= 2;
    argv = (char **) realloc(argv, argvsize * sizeof(char *));
    if (argv == NULL) out_of_memory();
  }
  argv[argc++] = arg;
}

static void expand_argument(char * arg)
{
  char * p;

  for (p = arg; *p != 0; p++) {
    if (*p == '*' || *p == '?') {
      expand_pattern(arg);
      return;
    }
  }
  store_argument(arg);
}

static void expand_pattern(char * pat)
{
  char * prefix, * p, * name;
  int handle;
  struct _finddata_t ffblk;
  size_t i;

  handle = _findfirst(pat, &ffblk);
  if (handle == -1) {
    store_argument(pat); /* a la Bourne shell */
    return;
  }
  prefix = caml_strdup(pat);
  for (i = strlen(prefix); i > 0; i--) {
    char c = prefix[i - 1];
    if (c == '\\' || c == '/' || c == ':') { prefix[i] = 0; break; }
  }
  do {
    name = caml_strconcat(2, prefix, ffblk.name);
    store_argument(name);
  } while (_findnext(handle, &ffblk) != -1);
  _findclose(handle);
  caml_stat_free(prefix);
}


CAMLexport void caml_expand_command_line(int * argcp, char *** argvp)
{
  int i;
  argc = 0;
  argvsize = 16;
  argv = (char **) malloc(argvsize * sizeof(char *));
  if (argv == NULL) out_of_memory();
  for (i = 0; i < *argcp; i++) expand_argument((*argvp)[i]);
  argv[argc] = NULL;
  *argcp = argc;
  *argvp = argv;
}

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

int caml_read_directory(char * dirname, struct ext_table * contents)
{
  size_t dirnamelen;
  char * template;
#if _MSC_VER <= 1200
  int h;
#else
  intptr_t h;
#endif
  struct _finddata_t fileinfo;

  dirnamelen = strlen(dirname);
  if (dirnamelen > 0 &&
      (dirname[dirnamelen - 1] == '/'
       || dirname[dirnamelen - 1] == '\\'
       || dirname[dirnamelen - 1] == ':'))
    template = caml_strconcat(2, dirname, "*.*");
  else
    template = caml_strconcat(2, dirname, "\\*.*");
  h = _findfirst(template, &fileinfo);
  if (h == -1) {
    caml_stat_free(template);
    return errno == ENOENT ? 0 : -1;
  }
  do {
    if (strcmp(fileinfo.name, ".") != 0 && strcmp(fileinfo.name, "..") != 0) {
      caml_ext_table_add(contents, caml_strdup(fileinfo.name));
    }
  } while (_findnext(h, &fileinfo) == 0);
  _findclose(h);
  caml_stat_free(template);
  return 0;
}

#ifndef NATIVE_CODE

/* Set up a new thread for control-C emulation and termination */

void caml_signal_thread(void * lpParam)
{
  char *endptr;
  HANDLE h;
  /* Get an hexa-code raw handle through the environment */
  h = (HANDLE) strtol(getenv("CAMLSIGPIPE"), &endptr, 16);
  while (1) {
    DWORD numread;
    BOOL ret;
    char iobuf[2];
    /* This shall always return a single character */
    ret = ReadFile(h, iobuf, 1, &numread, NULL);
    if (!ret || numread != 1) caml_sys_exit(Val_int(2));
    switch (iobuf[0]) {
    case 'C':
      caml_record_signal(SIGINT);
      break;
    case 'T':
      raise(SIGTERM);
      return;
    }
  }
}

#endif /* NATIVE_CODE */

#if defined(NATIVE_CODE) && !defined(_WIN64)

/* Handling of system stack overflow.
 * Based on code provided by Olivier Andrieu.

 * An EXCEPTION_STACK_OVERFLOW is signaled when the guard page at the
 * end of the stack has been accessed. Windows clears the PAGE_GUARD
 * protection (making it a regular PAGE_READWRITE) and then calls our
 * exception handler. This means that although we're handling an "out
 * of stack" condition, there is a bit of stack available to call
 * functions and allocate temporaries.
 *
 * PAGE_GUARD is a one-shot access protection mechanism: we need to
 * restore the PAGE_GUARD protection on this page otherwise the next
 * stack overflow won't be detected and the program will abruptly exit
 * with STATUS_ACCESS_VIOLATION.
 *
 * Visual Studio 2003 and later (_MSC_VER >= 1300) have a
 * _resetstkoflw() function that resets this protection.
 * Unfortunately, it cannot work when called directly from the
 * exception handler because at this point we are using the page that
 * is to be protected.
 *
 * A solution is to used an alternate stack when restoring the
 * protection. However it's not possible to use _resetstkoflw() then
 * since it determines the stack pointer by calling alloca(): it would
 * try to protect the alternate stack.
 *
 * Finally, we call caml_raise_stack_overflow; it will either call
 * caml_raise_exception which switches back to the normal stack, or
 * call caml_fatal_uncaught_exception which terminates the program
 * quickly.
 *
 * NB: The PAGE_GUARD protection is only available on WinNT, not
 * Win9x. There is an equivalent mechanism on Win9x with
 * PAGE_NOACCESS.
 *
 * Currently, does not work under Win64.
 */

static uintnat win32_alt_stack[0x80];

static void caml_reset_stack (void *faulting_address)
{
  OSVERSIONINFO osi;
  SYSTEM_INFO si;
  DWORD page_size;
  MEMORY_BASIC_INFORMATION mbi;
  DWORD oldprot;

  /* get the os version (Win9x or WinNT ?) */
  osi.dwOSVersionInfoSize = sizeof osi;
  if (! GetVersionEx (&osi))
    goto failed;

  /* get the system's page size. */
  GetSystemInfo (&si);
  page_size = si.dwPageSize;

  /* get some information on the page the fault occurred */
  if (! VirtualQuery (faulting_address, &mbi, sizeof mbi))
    goto failed;

  /* restore the PAGE_GUARD protection on this page */
  switch (osi.dwPlatformId) {
  case VER_PLATFORM_WIN32_NT:
    VirtualProtect (mbi.BaseAddress, page_size,
                    mbi.Protect | PAGE_GUARD, &oldprot);
    break;
  case VER_PLATFORM_WIN32_WINDOWS:
    VirtualProtect (mbi.BaseAddress, page_size,
                    PAGE_NOACCESS, &oldprot);
    break;
  }

 failed:
  caml_raise_stack_overflow();
}

CAMLextern int caml_is_in_code(void *);

static LONG CALLBACK
    caml_UnhandledExceptionFilter (EXCEPTION_POINTERS* exn_info)
{
  DWORD code   = exn_info->ExceptionRecord->ExceptionCode;
  CONTEXT *ctx = exn_info->ContextRecord;
  DWORD *ctx_ip = &(ctx->Eip);
  DWORD *ctx_sp = &(ctx->Esp);

  if (code == EXCEPTION_STACK_OVERFLOW && Is_in_code_area (*ctx_ip))
    {
      uintnat faulting_address;
      uintnat * alt_esp;

      /* grab the address that caused the fault */
      faulting_address = exn_info->ExceptionRecord->ExceptionInformation[1];

      /* call caml_reset_stack(faulting_address) using the alternate stack */
      alt_esp  = win32_alt_stack + sizeof(win32_alt_stack) / sizeof(uintnat);
      *--alt_esp = faulting_address;
      *ctx_sp = (uintnat) (alt_esp - 1);
      *ctx_ip = (uintnat) &caml_reset_stack;

      return EXCEPTION_CONTINUE_EXECUTION;
    }

  return EXCEPTION_CONTINUE_SEARCH;
}

void caml_win32_overflow_detection()
{
  SetUnhandledExceptionFilter (caml_UnhandledExceptionFilter);
}

#endif

/* Seeding of pseudo-random number generators */

int caml_win32_random_seed (intnat data[16])
{
  /* For better randomness, consider:
     http://msdn.microsoft.com/library/en-us/seccrypto/security/rtlgenrandom.asp
     http://blogs.msdn.com/b/michael_howard/archive/2005/01/14/353379.aspx
  */
  FILETIME t;
  LARGE_INTEGER pc;
  GetSystemTimeAsFileTime(&t);
  QueryPerformanceCounter(&pc);  /* PR#6032 */
  data[0] = t.dwLowDateTime;
  data[1] = t.dwHighDateTime;
  data[2] = GetCurrentProcessId();
  data[3] = pc.LowPart;
  data[4] = pc.HighPart;
  return 5;
}


#ifdef _MSC_VER

static void invalid_parameter_handler(const wchar_t* expression,
   const wchar_t* function,
   const wchar_t* file,
   unsigned int line,
   uintptr_t pReserved)
{
  /* no crash box */
}


void caml_install_invalid_parameter_handler()
{
  _set_invalid_parameter_handler(invalid_parameter_handler);
}

#endif


/* Recover executable name  */

int caml_executable_name(char * name, int name_len)
{
  int retcode;

  int ret = GetModuleFileName(NULL, name, name_len);
  if (0 == ret || ret >= name_len) return -1;
  return 0;
}

/* snprintf emulation */

int caml_snprintf(char * buf, size_t size, const char * format, ...)
{
  int len;
  va_list args;

  if (size > 0) {
    va_start(args, format);
    len = _vsnprintf(buf, size, format, args);
    va_end(args);
    if (len >= 0 && len < size) {
      /* [len] characters were stored in [buf],
         a null-terminator was appended. */
      return len;
    }
    /* [size] characters were stored in [buf], without null termination.
       Put a null terminator, truncating the output. */
    buf[size - 1] = 0;
  }
  /* Compute the actual length of output, excluding null terminator */
  va_start(args, format);
  len = _vscprintf(format, args);
  va_end(args);
  return len;
}
