/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include "unixsupport.h"

static int shutdown_command_table[] = {
  0, 1, 2
};

CAMLprim value unix_shutdown(sock, cmd)
     value sock, cmd;
{
  if (shutdown(Socket_val(sock),
               shutdown_command_table[Int_val(cmd)]) == -1) {
    win32_maperr(WSAGetLastError());
    uerror("shutdown", Nothing);
  }
  return Val_unit;
}
