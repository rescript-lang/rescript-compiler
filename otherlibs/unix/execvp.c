/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "unixsupport.h"

extern char ** cstringvect();
#ifndef _WIN32
extern char ** environ;
#endif

CAMLprim value unix_execvp(value path, value args)
{
  char ** argv;
  argv = cstringvect(args);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

CAMLprim value unix_execvpe(value path, value args, value env)
{
  char ** argv;
  char ** saved_environ;
  argv = cstringvect(args);
  saved_environ = environ;
  environ = cstringvect(env);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  stat_free((char *) environ);
  environ = saved_environ;
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}
