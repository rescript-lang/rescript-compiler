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

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

CAMLprim value unix_execve(value path, value args, value env)
{
  char_os ** argv;
  char_os ** envp;
  char_os * wpath;
  caml_unix_check_path(path, "execve");
  argv = cstringvect(args, "execve");
  envp = cstringvect(env, "execve");
  wpath = caml_stat_strdup_to_os(String_val(path));
  (void) execve_os(wpath, EXECV_CAST argv, EXECV_CAST envp);
  caml_stat_free(wpath);
  cstringvect_free(argv);
  cstringvect_free(envp);
  uerror("execve", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}
