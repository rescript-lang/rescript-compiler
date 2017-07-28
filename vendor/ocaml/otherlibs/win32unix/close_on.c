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

#include <caml/mlvalues.h>
#include "unixsupport.h"
#include <windows.h>

int win_set_inherit(value fd, BOOL inherit)
{
  /* According to the MSDN, SetHandleInformation may not work
     for console handles on WinNT4 and earlier versions. */
  if (! SetHandleInformation(Handle_val(fd),
                             HANDLE_FLAG_INHERIT,
                             inherit ? HANDLE_FLAG_INHERIT : 0)) {
    win32_maperr(GetLastError());
    return -1;
  }
  return 0;
}

CAMLprim value win_set_close_on_exec(value fd)
{
  if (win_set_inherit(fd, FALSE) == -1) uerror("set_close_on_exec", Nothing);
  return Val_unit;
}

CAMLprim value win_clear_close_on_exec(value fd)
{
  if (win_set_inherit(fd, TRUE) == -1) uerror("clear_close_on_exec", Nothing);
  return Val_unit;
}
