/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

extern int error_table[];

CAMLprim value unix_error_message(value err)
{
  int errnum;
  wchar_t buffer[512];

  errnum = Is_block(err) ? Int_val(Field(err, 0)) : error_table[Int_val(err)];
  if (errnum > 0)
    return caml_copy_string(strerror(errnum));
  if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL,
                    -errnum,
                    0,
                    buffer,
                    sizeof(buffer)/sizeof(wchar_t),
                    NULL))
    return caml_copy_string_of_utf16(buffer);
  swprintf(buffer, sizeof(buffer)/sizeof(wchar_t), L"unknown error #%d", errnum);
  return caml_copy_string_of_utf16(buffer);
}
