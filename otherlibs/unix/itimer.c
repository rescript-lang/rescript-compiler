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
#include <caml/fail.h>
#include <caml/memory.h>
#include "unixsupport.h"

#ifdef HAS_SETITIMER

#include <math.h>
#include <sys/time.h>

static void unix_set_timeval(struct timeval * tv, double d)
{
  double integr, frac;
  frac = modf(d, &integr);
  /* Round time up so that if d is small but not 0, we end up with
     a non-0 timeval. */
  tv->tv_sec = integr;
  tv->tv_usec = ceil(1e6 * frac);
  if (tv->tv_usec >= 1000000) { tv->tv_sec++; tv->tv_usec = 0; }
}

static value unix_convert_itimer(struct itimerval *tp)
{
#define Get_timeval(tv) (double) tv.tv_sec + (double) tv.tv_usec / 1e6
  value res = alloc_small(Double_wosize * 2, Double_array_tag);
  Store_double_field(res, 0, Get_timeval(tp->it_interval));
  Store_double_field(res, 1, Get_timeval(tp->it_value));
  return res;
#undef Get_timeval
}

static int itimers[3] = { ITIMER_REAL, ITIMER_VIRTUAL, ITIMER_PROF };

CAMLprim value unix_setitimer(value which, value newval)
{
  struct itimerval new, old;
  unix_set_timeval(&new.it_interval, Double_field(newval, 0));
  unix_set_timeval(&new.it_value, Double_field(newval, 1));
  if (setitimer(itimers[Int_val(which)], &new, &old) == -1)
    uerror("setitimer", Nothing);
  return unix_convert_itimer(&old);
}

CAMLprim value unix_getitimer(value which)
{
  struct itimerval val;
  if (getitimer(itimers[Int_val(which)], &val) == -1)
    uerror("getitimer", Nothing);
  return unix_convert_itimer(&val);
}

#else

CAMLprim value unix_setitimer(value which, value newval)
{ invalid_argument("setitimer not implemented"); }
CAMLprim value unix_getitimer(value which)
{ invalid_argument("getitimer not implemented"); }

#endif
