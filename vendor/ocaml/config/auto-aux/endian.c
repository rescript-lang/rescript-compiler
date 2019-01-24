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

#include <string.h>
#include "m.h"

#ifndef ARCH_SIXTYFOUR
long intval = 0x41424344L;
char * bigendian = "ABCD";
char * littleendian = "DCBA";
#else
long intval = 0x4142434445464748L;
char * bigendian = "ABCDEFGH";
char * littleendian = "HGFEDCBA";
#endif

int main(void)
{
  long n[2];
  char * p;

  n[0] = intval;
  n[1] = 0;
  p = (char *) n;
  if (strcmp(p, bigendian) == 0)
    return 0;
  if (strcmp(p, littleendian) == 0)
    return 1;
  return 2;
}
