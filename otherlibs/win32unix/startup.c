/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include "winworker.h"
#include "windbug.h"

value val_process_id;

CAMLprim value win_startup(unit)
     value unit;
{
  WSADATA wsaData;
  int i;
  HANDLE h;

  (void) WSAStartup(MAKEWORD(2, 0), &wsaData);
  DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(),
                  GetCurrentProcess(), &h, 0, TRUE,
                  DUPLICATE_SAME_ACCESS);
  val_process_id = Val_int(h);

  worker_init();

  return Val_unit;
}

CAMLprim value win_cleanup(unit)
     value unit;
{
  worker_cleanup();

  (void) WSACleanup();

  return Val_unit;
}
