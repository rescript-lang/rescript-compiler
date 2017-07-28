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

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include "unixsupport.h"
#include <errno.h>
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

#ifdef HAS_REWINDDIR

CAMLprim value unix_rewinddir(value vd)
{
  DIR * d = DIR_Val(vd);
  if (d == (DIR *) NULL) unix_error(EBADF, "rewinddir", Nothing);
  rewinddir(d);
  return Val_unit;
}

#else

CAMLprim value unix_rewinddir(value d)
{ invalid_argument("rewinddir not implemented"); }

#endif
