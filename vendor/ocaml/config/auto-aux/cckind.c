/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Determine vendor and version of C compiler */

/* This file is to be preprocessed and its output examined. */
/* It is not C source code to be executed.  */
/* This helps with cross-compilation. */

#if defined(__INTEL_COMPILER)
icc __INTEL_COMPILER
#elif defined(__clang_major__) && defined(__clang_minor__)
clang __clang_major__ __clang_minor__
#elif defined(__GNUC__) && defined(__GNUC_MINOR__)
gcc __GNUC__ __GNUC_MINOR__
#elif defined(__xlc__) && (__xlC__)
xlc __xlC__ __xlC_ver__
#else
unknown
#endif
