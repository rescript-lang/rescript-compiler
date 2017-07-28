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

#include "libgraph.h"

value caml_gr_sound(value vfreq, value vdur)
{
  XKeyboardControl kbdcontrol;

  caml_gr_check_open();
  kbdcontrol.bell_pitch = Int_val(vfreq);
  kbdcontrol.bell_duration = Int_val(vdur);
  XChangeKeyboardControl(caml_gr_display, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XBell(caml_gr_display, 0);
  kbdcontrol.bell_pitch = -1;   /* restore default value */
  kbdcontrol.bell_duration = -1; /* restore default value */
  XChangeKeyboardControl(caml_gr_display, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XFlush(caml_gr_display);
  return Val_unit;
}
