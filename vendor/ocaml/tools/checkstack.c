/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

#define MINSTACKBYTES (384 * 1024 * sizeof (long))

int main(int argc, char ** argv)
{
  struct rlimit limit;
  int rc;

  rc = getrlimit (RLIMIT_STACK, &limit);
  if (rc != 0) exit (0);
  if (limit.rlim_cur < MINSTACKBYTES){
    fprintf (stderr,
             "\nThe current stack size limit is too low (%luk)\n"
             "You must increase it with one of the following commands:\n"
             "Under sh, bash, zsh:  ulimit -s %lu\n"
             "Under csh, tcsh:      limit stacksize %lu\n\n",
             (unsigned long) (limit.rlim_cur / 1024),
             (unsigned long) (MINSTACKBYTES / 1024),
             (unsigned long) (MINSTACKBYTES / 1024));
    exit (3);
  }
  exit (0);
}
