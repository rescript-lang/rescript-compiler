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

#include "windbug.h"

int debug_test (void)
{
  static int debug_init = 0;
  static int debug = 0;

#ifdef DEBUG
  if (!debug_init)
  {
    debug = (getenv("OCAMLDEBUG") != NULL);
    debug_init = 1;
  };
#endif

  return debug;
}
