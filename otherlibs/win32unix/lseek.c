/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

static DWORD seek_command_table[] = {
  FILE_BEGIN, FILE_CURRENT, FILE_END
};

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

static __int64 caml_set_file_pointer(HANDLE h, __int64 dist, DWORD mode)
{
  LARGE_INTEGER i;
  DWORD err;

  i.QuadPart = dist;
  i.LowPart = SetFilePointer(h, i.LowPart, &i.HighPart, mode);
  if (i.LowPart == INVALID_SET_FILE_POINTER) {
    err = GetLastError();
    if (err != NO_ERROR) { win32_maperr(err); uerror("lseek", Nothing); }
  }
  return i.QuadPart;
}

CAMLprim value unix_lseek(value fd, value ofs, value cmd)
{
  __int64 ret;

  ret = caml_set_file_pointer(Handle_val(fd), Long_val(ofs),
                              seek_command_table[Int_val(cmd)]);
  if (ret > Max_long) {
    win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
    uerror("lseek", Nothing);
  }
  return Val_long(ret);
}

CAMLprim value unix_lseek_64(value fd, value ofs, value cmd)
{
  __int64 ret;

  ret = caml_set_file_pointer(Handle_val(fd), Int64_val(ofs),
                              seek_command_table[Int_val(cmd)]);
  return copy_int64(ret);
}
