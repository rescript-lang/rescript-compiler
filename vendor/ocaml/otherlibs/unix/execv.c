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

CAMLprim value unix_execv(value path, value args)
{
  char ** argv;
  argv = cstringvect(args);
  (void) execv(String_val(path), argv);
  stat_free((char *) argv);
  uerror("execv", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}
