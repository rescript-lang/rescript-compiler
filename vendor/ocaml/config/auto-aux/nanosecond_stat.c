/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 Jeremie Dimino, Jane Street Group, LLC              */
/*                                                                     */
/*  Copyright 2015 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "../../otherlibs/unix/nanosecond_stat.h"

int main() {
  struct stat *buf;
  double a, m, c;
  a = (double)NSEC(buf, a);
  m = (double)NSEC(buf, m);
  c = (double)NSEC(buf, c);
  return 0;
}
