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
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/osdeps.h>

#include <Windows.h>
#include <stdlib.h>

CAMLprim value unix_environment(value unit)
{
  /* Win32 doesn't have a notion of setuid bit, so accessing environ is safe. */
  if (_wenviron != NULL) {
    return caml_alloc_array((void *)caml_copy_string_of_utf16, (const char**)_wenviron);
  } else {
    return Atom(0);
  }
}
