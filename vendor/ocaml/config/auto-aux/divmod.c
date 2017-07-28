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

/* Test semantics of division and modulus for negative arguments */

long div4[] =
{ -4,-3,-3,-3,-3,-2,-2,-2,-2,-1,-1,-1,-1,0,0,0,
  0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4 };

long divm4[] =
{ 4,3,3,3,3,2,2,2,2,1,1,1,1,0,0,0,
  0,0,0,0,-1,-1,-1,-1,-2,-2,-2,-2,-3,-3,-3,-3,-4 };

long mod4[] =
{ 0,-3,-2,-1,0,-3,-2,-1,0,-3,-2,-1,0,-3,-2,-1,
  0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0 };

long modm4[] =
{ 0,-3,-2,-1,0,-3,-2,-1,0,-3,-2,-1,0,-3,-2,-1,
  0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0 };

long q1 = 4;
long q2 = -4;

int main()
{
  int i;
  for (i = -16; i <= 16; i++) {
    if (i / q1 != div4[i+16]) return 1;
    if (i / q2 != divm4[i+16]) return 1;
    if (i % q1 != mod4[i+16]) return 1;
    if (i % q2 != modm4[i+16]) return 1;
  }
  return 0;
}
