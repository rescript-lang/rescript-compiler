/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/* Contributed by Stephane Glondu <steph@glondu.net>                   */
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
#include <caml/memory.h>

#ifdef HAS_SETGROUPS

#include <sys/types.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <limits.h>
#include <grp.h>
#include "unixsupport.h"

CAMLprim value unix_setgroups(value groups)
{
  gid_t * gidset;
  mlsize_t size, i;
  int n;

  size = Wosize_val(groups);
  gidset = (gid_t *) caml_stat_alloc(size * sizeof(gid_t));
  for (i = 0; i < size; i++) gidset[i] = Int_val(Field(groups, i));

  n = setgroups(size, gidset);

  stat_free(gidset);
  if (n == -1) uerror("setgroups", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_setgroups(value groups)
{ invalid_argument("setgroups not implemented"); }

#endif
