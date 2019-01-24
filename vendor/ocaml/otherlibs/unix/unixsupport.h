/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_UNIXSUPPORT_H
#define CAML_UNIXSUPPORT_H

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define Nothing ((value) 0)

extern value unix_error_of_code (int errcode);
extern int code_of_unix_error (value error);

CAMLnoreturn_start
extern void unix_error (int errcode, const char * cmdname, value arg)
CAMLnoreturn_end;

CAMLnoreturn_start
extern void uerror (const char * cmdname, value arg)
CAMLnoreturn_end;

extern void caml_unix_check_path(value path, const char * cmdname);

#define UNIX_BUFFER_SIZE 65536

#define DIR_Val(v) *((DIR **) &Field(v, 0))

extern char ** cstringvect(value arg, char * cmdname);
extern void cstringvect_free(char **);

extern int unix_cloexec_default;
extern int unix_cloexec_p(value cloexec);
extern void unix_set_cloexec(int fd, char * cmdname, value arg);
extern void unix_clear_cloexec(int fd, char * cmdname, value arg);

#ifdef __cplusplus
}
#endif

#define EXECV_CAST

#endif /* CAML_UNIXSUPPORT_H */
