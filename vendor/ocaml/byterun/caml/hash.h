/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2011 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Auxiliary functions for custom hash functions */

#ifndef CAML_HASH_H
#define CAML_HASH_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern uint32 caml_hash_mix_uint32(uint32 h, uint32 d);
CAMLextern uint32 caml_hash_mix_intnat(uint32 h, intnat d);
CAMLextern uint32 caml_hash_mix_int64(uint32 h, int64 d);
CAMLextern uint32 caml_hash_mix_double(uint32 h, double d);
CAMLextern uint32 caml_hash_mix_float(uint32 h, float d);
CAMLextern uint32 caml_hash_mix_string(uint32 h, value s);

#ifdef __cplusplus
}
#endif

#endif /* CAML_HASH_H */
