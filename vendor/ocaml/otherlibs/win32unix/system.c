/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
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
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "unixsupport.h"
#include <process.h>
#include <stdio.h>

CAMLprim value win_system(cmd)
     value cmd;
{
  int ret;
  value st;
  wchar_t *buf;

  caml_unix_check_path(cmd, "system");
  buf = caml_stat_strdup_to_utf16 (String_val (cmd));
  caml_enter_blocking_section();
  _flushall();
  ret = _wsystem(buf);
  caml_leave_blocking_section();
  caml_stat_free(buf);
  if (ret == -1) uerror("system", Nothing);
  st = caml_alloc_small(1, 0); /* Tag 0: Exited */
  Field(st, 0) = Val_int(ret);
  return st;
}
