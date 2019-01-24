/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2011 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Auxiliary functions for custom hash functions */

#ifndef CAML_HASH_H
#define CAML_HASH_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern uint32_t caml_hash_mix_uint32(uint32_t h, uint32_t d);
CAMLextern uint32_t caml_hash_mix_intnat(uint32_t h, intnat d);
CAMLextern uint32_t caml_hash_mix_int64(uint32_t h, int64_t d);
CAMLextern uint32_t caml_hash_mix_double(uint32_t h, double d);
CAMLextern uint32_t caml_hash_mix_float(uint32_t h, float d);
CAMLextern uint32_t caml_hash_mix_string(uint32_t h, value s);

#ifdef __cplusplus
}
#endif


#endif /* CAML_HASH_H */
