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

/* Main program -- in C */

#include <stdlib.h>
#include <stdio.h>
#include <caml/callback.h>

extern int fib(int n);
extern char * format_result(int n);

#ifdef _WIN32
int wmain(int argc, wchar_t ** argv)
#else
int main(int argc, char ** argv)
#endif
{
  printf("Initializing OCaml code...\n");

  /* Initializing the runtime twice, to check that it's possible to
     make nested calls to caml_startup/caml_shutdown. */
#ifdef NO_BYTECODE_FILE
  caml_startup(argv);
  caml_startup(argv);
#else
  caml_main(argv);
  caml_main(argv);
#endif

  printf("Back in C code...\n");
  printf("Computing fib(20)...\n");
  printf("%s\n", format_result(fib(20)));

  caml_shutdown();
  caml_shutdown();

  return 0;
}
