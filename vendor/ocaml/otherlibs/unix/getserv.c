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
#include <caml/fail.h>
#include <caml/memory.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/types.h>

#ifndef _WIN32
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif

static value alloc_service_entry(struct servent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit, proto = Val_unit;

  Begin_roots3 (name, aliases, proto);
    name = caml_copy_string(entry->s_name);
    aliases = caml_copy_string_array((const char**)entry->s_aliases);
    proto = caml_copy_string(entry->s_proto);
    res = caml_alloc_small(4, 0);
    Field(res,0) = name;
    Field(res,1) = aliases;
    Field(res,2) = Val_int(ntohs(entry->s_port));
    Field(res,3) = proto;
  End_roots();
  return res;
}

CAMLprim value unix_getservbyname(value name, value proto)
{
  struct servent * entry;
  if (! (caml_string_is_c_safe(name) && caml_string_is_c_safe(proto)))
    caml_raise_not_found();
  entry = getservbyname(String_val(name), String_val(proto));
  if (entry == (struct servent *) NULL) caml_raise_not_found();
  return alloc_service_entry(entry);
}

CAMLprim value unix_getservbyport(value port, value proto)
{
  struct servent * entry;
  if (! caml_string_is_c_safe(proto)) caml_raise_not_found();
  entry = getservbyport(htons(Int_val(port)), String_val(proto));
  if (entry == (struct servent *) NULL) caml_raise_not_found();
  return alloc_service_entry(entry);
}

#else

CAMLprim value unix_getservbyport(value port, value proto)
{ caml_invalid_argument("getservbyport not implemented"); }

CAMLprim value unix_getservbyname(value name, value proto)
{ caml_invalid_argument("getservbyname not implemented"); }

#endif
