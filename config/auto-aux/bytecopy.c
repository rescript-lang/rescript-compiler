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

char buffer[27];

#ifdef reverse
#define cpy(s1,s2,n) copy(s2,s1,n)
#else
#define cpy copy
#endif

int main(int argc, char ** argv)
{
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  if (strcmp(buffer, "abcdefghijklmnopqrstuvwxyz") != 0) exit(1);
  cpy(buffer, buffer+3, 26-3);
  if (strcmp(buffer, "abcabcdefghijklmnopqrstuvw") != 0) exit(1);
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  cpy(buffer+3, buffer, 26-3);
  if (strcmp(buffer, "defghijklmnopqrstuvwxyzxyz") != 0) exit(1);
  exit(0);
}
