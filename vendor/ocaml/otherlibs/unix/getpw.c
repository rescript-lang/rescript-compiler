/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "unixsupport.h"
#include <pwd.h>

static value alloc_passwd_entry(struct passwd *entry)
{
  value res;
  value name = Val_unit, passwd = Val_unit, gecos = Val_unit;
  value dir = Val_unit, shell = Val_unit;

  Begin_roots5 (name, passwd, gecos, dir, shell);
    name = caml_copy_string(entry->pw_name);
    passwd = caml_copy_string(entry->pw_passwd);
#if !defined(__BEOS__) && !defined(__ANDROID__)
    gecos = caml_copy_string(entry->pw_gecos);
#else
    gecos = caml_copy_string("");
#endif
    dir = caml_copy_string(entry->pw_dir);
    shell = caml_copy_string(entry->pw_shell);
    res = caml_alloc_small(7, 0);
    Field(res,0) = name;
    Field(res,1) = passwd;
    Field(res,2) = Val_int(entry->pw_uid);
    Field(res,3) = Val_int(entry->pw_gid);
    Field(res,4) = gecos;
    Field(res,5) = dir;
    Field(res,6) = shell;
  End_roots();
  return res;
}

CAMLprim value unix_getpwnam(value name)
{
  struct passwd * entry;
  if (! caml_string_is_c_safe(name)) caml_raise_not_found();
  entry = getpwnam(String_val(name));
  if (entry == (struct passwd *) NULL) caml_raise_not_found();
  return alloc_passwd_entry(entry);
}

CAMLprim value unix_getpwuid(value uid)
{
  struct passwd * entry;
  entry = getpwuid(Int_val(uid));
  if (entry == (struct passwd *) NULL) caml_raise_not_found();
  return alloc_passwd_entry(entry);
}
