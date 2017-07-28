/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Contributed by Stephane Glondu <steph@glondu.net>                  */
/*                                                                     */
/*  Copyright 2009 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifdef HAS_INITGROUPS

#include <sys/types.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <limits.h>
#include <grp.h>
#include "unixsupport.h"

CAMLprim value unix_initgroups(value user, value group)
{
  if (initgroups(String_val(user), Int_val(group)) == -1) {
    uerror("initgroups", Nothing);
  }
  return Val_unit;
}

#else

CAMLprim value unix_initgroups(value user, value group)
{ invalid_argument("initgroups not implemented"); }

#endif
