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

/* Machine configuration, Intel x86 processors, Win32,
   Visual C++ or Mingw compiler */

#ifdef _WIN64
#define ARCH_SIXTYFOUR
#else
#undef ARCH_SIXTYFOUR
#endif
#undef ARCH_BIG_ENDIAN
#undef ARCH_ALIGN_DOUBLE

#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#ifdef _WIN64
#define SIZEOF_PTR 8
#else
#define SIZEOF_PTR 4
#endif
#define SIZEOF_SHORT 2

#ifdef __MINGW32__
#define ARCH_INT64_TYPE long long
#define ARCH_UINT64_TYPE unsigned long long
#else
#define ARCH_INT64_TYPE __int64
#define ARCH_UINT64_TYPE unsigned __int64
#endif
#define ARCH_INT64_PRINTF_FORMAT "I64"
#if _MSC_VER >= 1800
#define ARCH_SIZET_PRINTF_FORMAT "z"
#else
#define ARCH_SIZET_PRINTF_FORMAT "I"
#endif

#if defined(_MSC_VER) && !defined(__cplusplus)
#define inline __inline
#endif

#undef NONSTANDARD_DIV_MOD

#define PROFINFO_WIDTH 0

/* Microsoft introduced the LL integer literal suffix in Visual C++ .NET 2003 */
#if defined(_MSC_VER) && _MSC_VER < 1400
#define INT64_LITERAL(s) s ## i64
#else
#define INT64_LITERAL(s) s ## LL
#endif

#define FLAT_FLOAT_ARRAY
