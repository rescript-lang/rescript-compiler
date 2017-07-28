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
#include <errno.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

CAMLprim value unix_nice(value incr)
{
  int ret;
  errno = 0;
#ifdef HAS_NICE
  ret = nice(Int_val(incr));
#else
  ret = 0;
#endif
  if (ret == -1 && errno != 0) uerror("nice", Nothing);
  return Val_int(ret);
}
