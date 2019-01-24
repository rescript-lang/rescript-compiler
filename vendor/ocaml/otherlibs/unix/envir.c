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

#include <caml/config.h>

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAS_GETAUXVAL
#include <sys/auxv.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>

extern char ** environ;

CAMLprim value unix_environment_unsafe(value unit)
{
  if (environ != NULL) {
    return caml_copy_string_array((const char**)environ);
  } else {
    return Atom(0);
  }
}

static char **secure_environ(void)
{
#ifdef HAS_GETAUXVAL
  if (!getauxval(AT_SECURE))
    return environ;
  else
   return NULL;
#elif defined(HAS_ISSETUGID)
  if (!issetugid ())
    return environ;
  else
    return NULL;
#else
  if (geteuid () == getuid () && getegid () == getgid ())
    return environ;
  else
    return NULL;
#endif
}

CAMLprim value unix_environment(value unit)
{
  char **e = secure_environ();
  if (e != NULL) {
    return caml_copy_string_array((const char**)e);
  } else {
    return Atom(0);
  }
}
