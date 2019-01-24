/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operating system - specific stuff */

#ifndef CAML_OSDEPS_H
#define CAML_OSDEPS_H

#ifdef _WIN32
extern unsigned short caml_win32_major;
extern unsigned short caml_win32_minor;
extern unsigned short caml_win32_build;
extern unsigned short caml_win32_revision;
#endif

#ifdef CAML_INTERNALS

#include "misc.h"
#include "memory.h"

/* Read at most [n] bytes from file descriptor [fd] into buffer [buf].
   [flags] indicates whether [fd] is a socket
   (bit [CHANNEL_FLAG_FROM_SOCKET] is set in this case, see [io.h]).
   (This distinction matters for Win32, but not for Unix.)
   Return number of bytes read.
   In case of error, raises [Sys_error] or [Sys_blocked_io]. */
extern int caml_read_fd(int fd, int flags, void * buf, int n);

/* Write at most [n] bytes from buffer [buf] onto file descriptor [fd].
   [flags] indicates whether [fd] is a socket
   (bit [CHANNEL_FLAG_FROM_SOCKET] is set in this case, see [io.h]).
   (This distinction matters for Win32, but not for Unix.)
   Return number of bytes written.
   In case of error, raises [Sys_error] or [Sys_blocked_io]. */
extern int caml_write_fd(int fd, int flags, void * buf, int n);

/* Decompose the given path into a list of directories, and add them
   to the given table. */
extern char_os * caml_decompose_path(struct ext_table * tbl, char_os * path);

/* Search the given file in the given list of directories.
   If not found, return a copy of [name]. */
extern char_os * caml_search_in_path(struct ext_table * path, const char_os * name);

/* Same, but search an executable name in the system path for executables. */
CAMLextern char_os * caml_search_exe_in_path(const char_os * name);

/* Same, but search a shared library in the given path. */
extern char_os * caml_search_dll_in_path(struct ext_table * path, const char_os * name);

/* Open a shared library and return a handle on it.
   If [for_execution] is true, perform full symbol resolution and
   execute initialization code so that functions from the shared library
   can be called.  If [for_execution] is false, functions from this
   shared library will not be called, but just checked for presence,
   so symbol resolution can be skipped.
   If [global] is true, symbols from the shared library can be used
   to resolve for other libraries to be opened later on.
   Return [NULL] on error. */
extern void * caml_dlopen(char_os * libname, int for_execution, int global);

/* Close a shared library handle */
extern void caml_dlclose(void * handle);

/* Look up the given symbol in the given shared library.
   Return [NULL] if not found, or symbol value if found. */
extern void * caml_dlsym(void * handle, const char * name);

extern void * caml_globalsym(const char * name);

/* Return an error message describing the most recent dynlink failure. */
extern char * caml_dlerror(void);

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */
extern int caml_read_directory(char_os * dirname, struct ext_table * contents);

/* Recover executable name if possible (/proc/sef/exe under Linux,
   GetModuleFileName under Windows).  Return NULL on error,
   string allocated with [caml_stat_alloc] on success. */
extern char_os * caml_executable_name(void);

/* Secure version of [getenv]: returns NULL if the process has special
   privileges (setuid bit, setgid bit, capabilities).
*/
extern char_os *caml_secure_getenv(char_os const *var);

#ifdef _WIN32

extern int caml_win32_rename(const wchar_t *, const wchar_t *);

extern void caml_probe_win32_version(void);
extern void caml_setup_win32_terminal(void);
extern void caml_restore_win32_terminal(void);

/* Windows Unicode support */

extern int win_multi_byte_to_wide_char(const char* s, int slen, wchar_t *out, int outlen);
extern int win_wide_char_to_multi_byte(const wchar_t* s, int slen, char *out, int outlen);

/* [caml_stat_strdup_to_utf16(s)] returns a NULL-terminated copy of [s],
   re-encoded in UTF-16.  The encoding of [s] is assumed to be UTF-8 if
   [caml_windows_unicode_runtime_enabled] is non-zero **and** [s] is valid
   UTF-8, or the current Windows code page otherwise.

   The returned string is allocated with [caml_stat_alloc], so it should be free
   using [caml_stat_free].
*/
extern wchar_t* caml_stat_strdup_to_utf16(const char *s);

/* [caml_stat_strdup_of_utf16(s)] returns a NULL-terminated copy of [s],
   re-encoded in UTF-8 if [caml_windows_unicode_runtime_enabled] is non-zero or
   the current Windows code page otherwise.

   The returned string is allocated with [caml_stat_alloc], so it should be free
   using [caml_stat_free].
*/
extern char* caml_stat_strdup_of_utf16(const wchar_t *s);

/* [caml_copy_string_of_utf16(s)] returns an OCaml string containing a copy of
   [s] re-encoded in UTF-8 if [caml_windows_unicode_runtime_enabled] is non-zero
   or in the current code page otherwise.
*/
extern value caml_copy_string_of_utf16(const wchar_t *s);

#endif /* _WIN32 */

#endif /* CAML_INTERNALS */

#endif /* CAML_OSDEPS_H */
