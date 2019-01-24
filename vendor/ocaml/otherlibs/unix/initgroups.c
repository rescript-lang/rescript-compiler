/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Stephane Glondu <steph@glondu.net>                    */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifdef HAS_INITGROUPS

#include <sys/types.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <errno.h>
#include <limits.h>
#include <grp.h>
#include "unixsupport.h"

CAMLprim value unix_initgroups(value user, value group)
{
  if (! caml_string_is_c_safe(user))
    unix_error(EINVAL, "initgroups", user);
  if (initgroups(String_val(user), Int_val(group)) == -1) {
    uerror("initgroups", Nothing);
  }
  return Val_unit;
}

#else

CAMLprim value unix_initgroups(value user, value group)
{ caml_invalid_argument("initgroups not implemented"); }

#endif
