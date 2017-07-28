/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*              Jun Furuse, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include "libgraph.h"

value caml_gr_open_subwindow(value vx, value vy, value width, value height)
{
  Window win;

  int h = Int_val(height);
  int w = Int_val(width);
  int x = Int_val(vx);
  int y = Int_val(vy);

  caml_gr_check_open();
  win = XCreateSimpleWindow(caml_gr_display, caml_gr_window.win,
                            x, Wcvt(y + h), w, h,
                            0, caml_gr_black, caml_gr_background);
  XMapWindow(caml_gr_display, win);
  XFlush(caml_gr_display);
  return (caml_gr_id_of_window (win));
}

value caml_gr_close_subwindow(value wid)
{
  Window win;

  caml_gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XDestroyWindow(caml_gr_display, win);
  XFlush(caml_gr_display);
  return Val_unit;
}
