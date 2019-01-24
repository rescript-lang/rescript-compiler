/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Sylvain Le Gall for Lexifi                            */
/*                                                                        */
/*   Copyright 2008 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifdef DEBUG

#include <stdio.h>
#include <windows.h>

/* According to MSDN, MSVC supports the gcc ## operator (to deal with empty
   argument lists)
 */
#define DEBUG_PRINT(fmt, ...) \
  do \
  { \
    if (debug_test()) \
    { \
      fprintf(stderr, "DBUG (pid:%ld, tid: %ld): ", GetCurrentProcessId(), \
              GetCurrentThreadId()); \
      fprintf(stderr, fmt, ##__VA_ARGS__); \
      fprintf(stderr, "\n"); \
      fflush(stderr); \
    }; \
  } while(0)

/* Test if we are in dbug mode */
int  debug_test    (void);

#elif defined(_MSC_VER) && _MSC_VER < 1300

#define DEBUG_PRINT(fmt)

/* __pragma wasn't added until Visual C++ .NET 2002, so simply disable the
   warning entirely
 */

#pragma warning (disable:4002)

#elif defined(_MSC_VER) && _MSC_VER <= 1400

/* Not all versions of the Visual Studio 2005 C Compiler (Version 14) support
   variadic macros, hence the test for this branch being <= 1400 rather than
   < 1400.
   This convoluted pair of macros allow DEBUG_PRINT to remain while temporarily
   suppressing the warning displayed for a macro called with too many
   parameters.
 */
#define DEBUG_PRINT_S(fmt) __pragma(warning(pop))
#define DEBUG_PRINT \
  __pragma(warning(push)) \
  __pragma(warning(disable:4002)) \
  DEBUG_PRINT_S

#else

/* Visual Studio supports variadic macros in all versions from 2008 (CL 15). */
#define DEBUG_PRINT(fmt, ...)

#endif
