/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*                  Jeremie Dimino, Jane Street Europe                    */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef __TEST_COMMON_H
#define __TEST_COMMON_H

/* Where the OCaml side stores the arguments and result for a test
   case. The C function will read the result it is supposed to return
   from this buffer.

   Argument [n] is stored at [n * 8] and the result is stored at
   [arity * 8].
*/
extern char *ocaml_buffer;

/* Where the C function stores the arguments it receive for a test
   case. The OCaml side will store the result from the C function in
   this buffer. At the of a test case, both these buffers must be
   equal. */
extern char *c_buffer;

#define get_intnat(n) *(intnat*)(ocaml_buffer+((n)*8))
#define get_int32(n) *(int32_t*)(ocaml_buffer+((n)*8))
#define get_int64(n) *(int64_t*)(ocaml_buffer+((n)*8))
#define get_double(n) *(double*)(ocaml_buffer+((n)*8))

#define set_intnat(n, x) *(intnat*)(c_buffer+((n)*8)) = (x)
#define set_int32(n, x) *(int32_t*)(c_buffer+((n)*8)) = (x)
#define set_int64(n, x) *(int64_t*)(c_buffer+((n)*8)) = (x)
#define set_double(n, x) *(double*)(c_buffer+((n)*8)) = (x)

#endif /* __TEST_COMMON_H */
