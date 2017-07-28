/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Contributed by Sylvain Le Gall for Lexifi                          */
/*                                                                     */
/*  Copyright 2008 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

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

#else
#define DEBUG_PRINT(fmt, ...)
#endif
