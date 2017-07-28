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

#include <stdio.h>
#include <string.h>

/* Check for the availability of "long long" type as per ISO C9X */

/* Meaning of return code:
     0   long long OK, printf with %ll
     1   long long OK, printf with %q
     2   long long OK, no printf
     3   long long not suitable */

int main(int argc, char **argv)
{
  long long l;
  unsigned long long u;
  char buffer[64];

  if (sizeof(long long) != 8) return 3;
  l = 123456789123456789LL;
  buffer[0] = '\0';
  sprintf(buffer, "%lld", l);
  if (strcmp(buffer, "123456789123456789") == 0) return 0;
  /* the MacOS X library uses qd to format long longs */
  buffer[0] = '\0';
  sprintf (buffer, "%qd", l);
  if (strcmp (buffer, "123456789123456789") == 0) return 1;
  return 2;
}
