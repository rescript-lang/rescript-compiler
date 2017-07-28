/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operating system and standard library dependencies. */

/* 0. Operating system type string. */

#define OCAML_OS_TYPE "Unix"
/* #define OCAML_OS_TYPE "Win32" */
/* #define OCAML_OS_TYPE "MacOS" */

/* 1. For the runtime system. */

#define POSIX_SIGNALS

/* Define POSIX_SIGNALS if signal handling is POSIX-compliant.
   In particular, sigaction(), sigprocmask() and the operations on
   sigset_t are provided. */

#define BSD_SIGNALS

/* Define BSD_SIGNALS if signal handlers have the BSD semantics: the handler
   remains attached to the signal when the signal is received. Leave it
   undefined if signal handlers have the System V semantics: the signal
   resets the behavior to default. */

#define HAS_SIGSETMASK

/* Define HAS_SIGSETMASK if you have sigsetmask(), as in BSD. */

#define HAS_TERMCAP

/* Define HAS_TERMCAP if you have the termcap functions to read the
   terminal database, e.g. tgetent(), tgetstr(), tgetnum(), tputs().
   Also add the required libraries (e.g. -lcurses -ltermcap) to $(CCLIBS)
   in ../Makefile.config */

#define SUPPORT_DYNAMIC_LINKING

/* Define SUPPORT_DYNAMIC_LINKING if dynamic loading of C stub code
   via dlopen() is available. */

#define HAS_C99_FLOAT_OPS

/* Define HAS_C99_FLOAT_OPS if <math.h> conforms to ISO C99.
   In particular, it should provide expm1(), log1p(), hypot(), copysign(). */

/* 2. For the Unix library. */

#define HAS_SOCKETS

/* Define HAS_SOCKETS if you have BSD sockets. */

#define HAS_SOCKLEN_T

/* Define HAS_SOCKLEN_T if the type socklen_t is defined in
   /usr/include/sys/socket.h. */

#define HAS_UNISTD

/* Define HAS_UNISTD if you have /usr/include/unistd.h. */

#define HAS_DIRENT

/* Define HAS_DIRENT if you have /usr/include/dirent.h and the result of
   readdir() is of type struct dirent *.
   Otherwise, we'll load /usr/include/sys/dir.h, and readdir() is expected to
   return a struct direct *. */

#define HAS_REWINDDIR

/* Define HAS_REWINDDIR if you have rewinddir(). */

#define HAS_LOCKF

/* Define HAS_LOCKF if the library provides the lockf() function. */

#define HAS_MKFIFO

/* Define HAS_MKFIFO if the library provides the mkfifo() function. */

#define HAS_GETCWD
#define HAS_GETWD

/* Define HAS_GETCWD if the library provides the getcwd() function. */
/* Define HAS_GETWD if the library provides the getwd() function. */

#define HAS_GETPRIORITY

/* Define HAS_GETPRIORITY if the library provides getpriority() and
   setpriority(). Otherwise, we'll use nice(). */

#define HAS_UTIME
#define HAS_UTIMES

/* Define HAS_UTIME if you have /usr/include/utime.h and the library
   provides utime(). Define HAS_UTIMES if the library provides utimes(). */

#define HAS_DUP2

/* Define HAS_DUP2 if you have dup2(). */

#define HAS_FCHMOD

/* Define HAS_FCHMOD if you have fchmod() and fchown(). */

#define HAS_TRUNCATE

/* Define HAS_TRUNCATE if you have truncate() and
   ftruncate(). */

#define HAS_SELECT

/* Define HAS_SELECT if you have select(). */

#define HAS_SYS_SELECT_H

/* Define HAS_SYS_SELECT_H if /usr/include/sys/select.h exists
   and should be included before using select(). */

#define HAS_SYMLINK

/* Define HAS_SYMLINK if you have symlink() and readlink() and lstat(). */

#define HAS_WAIT4
#define HAS_WAITPID

/* Define HAS_WAIT4 if you have wait4().
   Define HAS_WAITPID if you have waitpid(). */

#define HAS_GETGROUPS

/* Define HAS_GETGROUPS if you have getgroups(). */

#define HAS_SETGROUPS

/* Define HAS_SETGROUPS if you have setgroups(). */

#define HAS_INITGROUPS

/* Define HAS_INITGROUPS if you have initgroups(). */

#define HAS_TERMIOS

/* Define HAS_TERMIOS if you have /usr/include/termios.h and it is
   Posix-compliant. */

#define HAS_ASYNC_IO

/* Define HAS_ASYNC_IO if BSD-style asynchronous I/O are supported
   (the process can request to be sent a SIGIO signal when a descriptor
   is ready for reading). */

#define HAS_SETITIMER

/* Define HAS_SETITIMER if you have setitimer(). */

#define HAS_GETHOSTNAME

/* Define HAS_GETHOSTNAME if you have gethostname(). */

#define HAS_UNAME

/* Define HAS_UNAME if you have uname(). */

#define HAS_GETTIMEOFDAY

/* Define HAS_GETTIMEOFDAY if you have gettimeofday(). */

#define HAS_MKTIME

/* Define HAS_MKTIME if you have mktime(). */

#define HAS_SETSID

/* Define HAS_SETSID if you have setsid(). */

#define HAS_PUTENV

/* Define HAS_PUTENV if you have putenv(). */

#define HAS_LOCALE

/* Define HAS_LOCALE if you have the include file <locale.h> and the
   setlocale() function. */

#define HAS_MMAP

/* Define HAS_MMAP if you have the include file <sys/mman.h> and the
   functions mmap() and munmap(). */

#define HAS_GETHOSTBYNAME_R 6

/* Define HAS_GETHOSTBYNAME_R if gethostbyname_r() is available.
   The value of this symbol is the number of arguments of
   gethostbyname_r(): either 5 or 6 depending on prototype.
   (5 is the Solaris version, 6 is the Linux version). */

#define HAS_GETHOSTBYADDR_R 8

/* Define HAS_GETHOSTBYADDR_R if gethostbyname_r() is available.
   The value of this symbol is the number of arguments of
   gethostbyaddr_r(): either 7 or 8 depending on prototype.
   (7 is the Solaris version, 8 is the Linux version). */

#define HAS_NICE

/* Define HAS_NICE if you have nice(). */
