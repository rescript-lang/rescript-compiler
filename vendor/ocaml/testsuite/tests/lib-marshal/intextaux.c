/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/intext.h>

value marshal_to_block(value vbuf, value vlen, value v, value vflags)
{
  return Val_long(output_value_to_block(v, vflags,
                                        (char *) vbuf, Long_val(vlen)));
}

value marshal_from_block(value vbuf, value vlen)
{
  return input_value_from_block((char *) vbuf, Long_val(vlen));
}
