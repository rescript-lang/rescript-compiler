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
#include <caml/signals.h>
#include "unixsupport.h"

#include <errno.h>
#include <time.h>
#ifdef HAS_SELECT
#include <sys/types.h>
#include <sys/time.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif
#endif

CAMLprim value unix_sleep(value duration)
{
  double d = Double_val(duration);
  if (d < 0.0) return Val_unit;
#if defined(HAS_NANOSLEEP)
  {
    struct timespec t;
    int ret;
    caml_enter_blocking_section();
    t.tv_sec = (time_t) d;
    t.tv_nsec = (d - t.tv_sec) * 1e9;
    do {
      ret = nanosleep(&t, &t);
    } while (ret == -1 && errno == EINTR);
    caml_leave_blocking_section();
    if (ret == -1) uerror("sleep", Nothing);
  }
#elif defined(HAS_SELECT)
  {
    struct timeval t;
    int ret;
    caml_enter_blocking_section();
    t.tv_sec = (time_t) d;
    t.tv_usec = (d - t.tv_sec) * 1e6;
    do {
      ret = select(0, NULL, NULL, NULL, &t);
    } while (ret == -1 && errno == EINTR);
    caml_leave_blocking_section();
    if (ret == -1) uerror("sleep", Nothing);
  }
#else
  /* Fallback implementation, resolution 1 second only.
     We cannot reliably iterate until sleep() returns 0, because the
     remaining time returned by sleep() is generally rounded up. */
  {
    caml_enter_blocking_section();
    sleep ((unsigned int) d);
    caml_leave_blocking_section();
  }
#endif
  return Val_unit;
}
