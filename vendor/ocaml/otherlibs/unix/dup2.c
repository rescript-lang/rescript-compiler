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
#include "unixsupport.h"

#ifdef HAS_DUP2

CAMLprim value unix_dup2(value fd1, value fd2)
{
  if (dup2(Int_val(fd1), Int_val(fd2)) == -1) uerror("dup2", Nothing);
  return Val_unit;
}

#else

static int do_dup2(int fd1, int fd2)
{
  int fd;
  int res;

  fd = dup(fd1);
  if (fd == -1) return -1;
  if (fd == fd2) return 0;
  res = do_dup2(fd1, fd2);
  close(fd);
  return res;
}

CAMLprim value unix_dup2(value fd1, value fd2)
{
  close(Int_val(fd2));
  if (do_dup2(Int_val(fd1), Int_val(fd2)) == -1) uerror("dup2", Nothing);
  return Val_unit;
}

#endif
