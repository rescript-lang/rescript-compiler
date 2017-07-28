/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "unixsupport.h"
#include "cst2constr.h"
#define _INTEGRAL_MAX_BITS 64
#include <sys/types.h>
#include <sys/stat.h>

#ifndef S_IFLNK
#define S_IFLNK 0
#endif
#ifndef S_IFIFO
#define S_IFIFO 0
#endif
#ifndef S_IFSOCK
#define S_IFSOCK 0
#endif
#ifndef S_IFBLK
#define S_IFBLK 0
#endif

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

static value stat_aux(int use_64, struct _stati64 *buf)
{
  CAMLparam0 ();
  CAMLlocal1 (v);

  v = caml_alloc (12, 0);
  Store_field (v, 0, Val_int (buf->st_dev));
  Store_field (v, 1, Val_int (buf->st_ino));
  Store_field (v, 2, cst_to_constr (buf->st_mode & S_IFMT, file_kind_table,
                                    sizeof(file_kind_table) / sizeof(int), 0));
  Store_field (v, 3, Val_int(buf->st_mode & 07777));
  Store_field (v, 4, Val_int (buf->st_nlink));
  Store_field (v, 5, Val_int (buf->st_uid));
  Store_field (v, 6, Val_int (buf->st_gid));
  Store_field (v, 7, Val_int (buf->st_rdev));
  Store_field (v, 8,
               use_64 ? copy_int64(buf->st_size) : Val_int (buf->st_size));
  Store_field (v, 9, copy_double((double) buf->st_atime));
  Store_field (v, 10, copy_double((double) buf->st_mtime));
  Store_field (v, 11, copy_double((double) buf->st_ctime));
  CAMLreturn (v);
}

CAMLprim value unix_stat(value path)
{
  int ret;
  struct _stati64 buf;

  ret = _stati64(String_val(path), &buf);
  if (ret == -1) uerror("stat", path);
  if (buf.st_size > Max_long) {
    win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
    uerror("stat", path);
  }
  return stat_aux(0, &buf);
}

CAMLprim value unix_stat_64(value path)
{
  int ret;
  struct _stati64 buf;
  ret = _stati64(String_val(path), &buf);
  if (ret == -1) uerror("stat", path);
  return stat_aux(1, &buf);
}

CAMLprim value unix_fstat(value handle)
{
  int ret;
  struct _stati64 buf;

  ret = _fstati64(win_CRT_fd_of_filedescr(handle), &buf);
  if (ret == -1) uerror("fstat", Nothing);
  if (buf.st_size > Max_long) {
    win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
    uerror("fstat", Nothing);
  }
  return stat_aux(0, &buf);
}

CAMLprim value unix_fstat_64(value handle)
{
  int ret;
  struct _stati64 buf;

  ret = _fstati64(win_CRT_fd_of_filedescr(handle), &buf);
  if (ret == -1) uerror("fstat", Nothing);
  return stat_aux(1, &buf);
}
