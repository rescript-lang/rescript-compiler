/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*                         Alain Frisch, LexiFi                           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

#if !defined(OPENSTEP) && (defined(_WIN32) && !defined(__CYGWIN__))
#  if defined(_MSC_VER) || defined(__MINGW32__)
#    define _DLLAPI __declspec(dllexport)
#  else
#    define _DLLAPI extern
#  endif
#  if defined(__MINGW32__) || defined(UNDER_CE)
#    define _CALLPROC
#  else
#    define _CALLPROC __stdcall
#  endif
#elif defined(__GNUC__) && (__GNUC__ * 100 + __GNUC_MINOR__) >= 303
#  define _DLLAPI __attribute__((visibility("default")))
#  define _CALLPROC
#endif /* WIN32 && !CYGWIN */

_DLLAPI void _CALLPROC start_caml_engine() {
  wchar_t * argv[2];
  argv[0] = L"--";
  argv[1] = NULL;
  caml_startup(argv);
}
