/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Swap byte-order in 16, 32, and 64-bit integers or floats */

#ifndef CAML_REVERSE_H
#define CAML_REVERSE_H

#define Reverse_16(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a;                                                                  \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _q[0] = _p[1];                                                            \
  _q[1] = _a;                                                               \
}

#define Reverse_32(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a, _b;                                                              \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _b = _p[1];                                                               \
  _q[0] = _p[3];                                                            \
  _q[1] = _p[2];                                                            \
  _q[3] = _a;                                                               \
  _q[2] = _b;                                                               \
}

#define Reverse_64(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a, _b;                                                              \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _b = _p[1];                                                               \
  _q[0] = _p[7];                                                            \
  _q[1] = _p[6];                                                            \
  _q[7] = _a;                                                               \
  _q[6] = _b;                                                               \
  _a = _p[2];                                                               \
  _b = _p[3];                                                               \
  _q[2] = _p[5];                                                            \
  _q[3] = _p[4];                                                            \
  _q[5] = _a;                                                               \
  _q[4] = _b;                                                               \
}

#define Perm_index(perm,i) ((perm >> (i * 4)) & 0xF)

#define Permute_64(dst,perm_dst,src,perm_src) {                             \
  char * _p;                                                                \
  char _a, _b, _c, _d, _e, _f, _g, _h;                                      \
  _p = (char *) (src);                                                      \
  _a = _p[Perm_index(perm_src, 0)];                                         \
  _b = _p[Perm_index(perm_src, 1)];                                         \
  _c = _p[Perm_index(perm_src, 2)];                                         \
  _d = _p[Perm_index(perm_src, 3)];                                         \
  _e = _p[Perm_index(perm_src, 4)];                                         \
  _f = _p[Perm_index(perm_src, 5)];                                         \
  _g = _p[Perm_index(perm_src, 6)];                                         \
  _h = _p[Perm_index(perm_src, 7)];                                         \
  _p = (char *) (dst);                                                      \
  _p[Perm_index(perm_dst, 0)] = _a;                                         \
  _p[Perm_index(perm_dst, 1)] = _b;                                         \
  _p[Perm_index(perm_dst, 2)] = _c;                                         \
  _p[Perm_index(perm_dst, 3)] = _d;                                         \
  _p[Perm_index(perm_dst, 4)] = _e;                                         \
  _p[Perm_index(perm_dst, 5)] = _f;                                         \
  _p[Perm_index(perm_dst, 6)] = _g;                                         \
  _p[Perm_index(perm_dst, 7)] = _h;                                         \
}

#endif /* CAML_REVERSE_H */
