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

#define _GNU_SOURCE  /* helps to find execvpe() */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#define CAML_INTERNALS
#include <caml/osdeps.h>
#include "unixsupport.h"
#include "errno.h"

CAMLprim value unix_execvp(value path, value args)
{
  char_os ** argv;
  char_os * wpath;
  caml_unix_check_path(path, "execvp");
  argv = cstringvect(args, "execvp");
  wpath = caml_stat_strdup_to_os(String_val(path));
  (void) execvp_os((const char_os *)wpath, EXECV_CAST argv);
  caml_stat_free(wpath);
  cstringvect_free(argv);
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                    /* from smart compilers */
}

#ifdef HAS_EXECVPE

CAMLprim value unix_execvpe(value path, value args, value env)
{
  char_os ** argv;
  char_os ** envp;
  char_os * wpath;
  caml_unix_check_path(path, "execvpe");
  argv = cstringvect(args, "execvpe");
  envp = cstringvect(env, "execvpe");
  wpath = caml_stat_strdup_to_os(String_val(path));
  (void) execvpe_os((const char_os *)wpath, EXECV_CAST argv, EXECV_CAST envp);
  caml_stat_free(wpath);
  cstringvect_free(argv);
  cstringvect_free(envp);
  uerror("execvpe", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                    /* from smart compilers */
}

#else

CAMLprim value unix_execvpe(value path, value args, value env)
{
  unix_error(ENOSYS, "execvpe", path);
  return Val_unit;
}

#endif

