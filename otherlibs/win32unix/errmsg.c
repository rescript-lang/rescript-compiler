/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"

extern int error_table[];

CAMLprim value unix_error_message(value err)
{
  int errnum;
  char buffer[512];

  errnum = Is_block(err) ? Int_val(Field(err, 0)) : error_table[Int_val(err)];
  if (errnum > 0)
    return copy_string(strerror(errnum));
  if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL,
                    -errnum,
                    0,
                    buffer,
                    sizeof(buffer),
                    NULL))
    return copy_string(buffer);
  sprintf(buffer, "unknown error #%d", errnum);
  return copy_string(buffer);
}
