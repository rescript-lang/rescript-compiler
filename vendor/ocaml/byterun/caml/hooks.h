/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                    Fabrice Le Fessant, INRIA de Paris                  */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_HOOKS_H
#define CAML_HOOKS_H

#include "misc.h"
#include "memory.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CAML_INTERNALS

#ifdef NATIVE_CODE

/* executed just before calling the entry point of a dynamically
   loaded native code module. */
CAMLextern void (*caml_natdynlink_hook)(void* handle, const char* unit);

#endif /* NATIVE_CODE */

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_HOOKS_H */
