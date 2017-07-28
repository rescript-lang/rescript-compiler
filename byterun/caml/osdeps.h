/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operating system - specific stuff */

#ifndef CAML_OSDEPS_H
#define CAML_OSDEPS_H

#include "misc.h"

/* Decompose the given path into a list of directories, and add them
   to the given table.  Return the block to be freed later. */
extern char * caml_decompose_path(struct ext_table * tbl, char * path);

/* Search the given file in the given list of directories.
   If not found, return a copy of [name].  Result is allocated with
   [caml_stat_alloc]. */
extern char * caml_search_in_path(struct ext_table * path, char * name);

/* Same, but search an executable name in the system path for executables. */
CAMLextern char * caml_search_exe_in_path(char * name);

/* Same, but search a shared library in the given path. */
extern char * caml_search_dll_in_path(struct ext_table * path, char * name);

/* Open a shared library and return a handle on it.
   If [for_execution] is true, perform full symbol resolution and
   execute initialization code so that functions from the shared library
   can be called.  If [for_execution] is false, functions from this
   shared library will not be called, but just checked for presence,
   so symbol resolution can be skipped.
   If [global] is true, symbols from the shared library can be used
   to resolve for other libraries to be opened later on.
   Return [NULL] on error. */
extern void * caml_dlopen(char * libname, int for_execution, int global);

/* Close a shared library handle */
extern void caml_dlclose(void * handle);

/* Look up the given symbol in the given shared library.
   Return [NULL] if not found, or symbol value if found. */
extern void * caml_dlsym(void * handle, char * name);

extern void * caml_globalsym(char * name);

/* Return an error message describing the most recent dynlink failure. */
extern char * caml_dlerror(void);

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */
extern int caml_read_directory(char * dirname, struct ext_table * contents);

/* Recover executable name if possible (/proc/sef/exe under Linux,
   GetModuleFileName under Windows). */
extern int caml_executable_name(char * name, int name_len);

#endif /* CAML_OSDEPS_H */
