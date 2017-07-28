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
#include <caml/alloc.h>

value caml_gr_plot(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  caml_gr_check_open();
  if(caml_gr_remember_modeflag)
    XDrawPoint(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc, x,
               Bcvt(y));
  if(caml_gr_display_modeflag) {
    XDrawPoint(caml_gr_display, caml_gr_window.win, caml_gr_window.gc, x,
               Wcvt(y));
    XFlush(caml_gr_display);
  }
  return Val_unit;
}

value caml_gr_moveto(value vx, value vy)
{
  caml_gr_x = Int_val(vx);
  caml_gr_y = Int_val(vy);
  return Val_unit;
}

value caml_gr_current_x(void)
{
  return Val_int(caml_gr_x);
}

value caml_gr_current_y(void)
{
  return Val_int(caml_gr_y);
}

value caml_gr_lineto(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  caml_gr_check_open();
  if(caml_gr_remember_modeflag)
    XDrawLine(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
              caml_gr_x, Bcvt(caml_gr_y), x, Bcvt(y));
  if(caml_gr_display_modeflag) {
    XDrawLine(caml_gr_display, caml_gr_window.win, caml_gr_window.gc,
          caml_gr_x, Wcvt(caml_gr_y), x, Wcvt(y));
    XFlush(caml_gr_display);
  }
  caml_gr_x = x;
  caml_gr_y = y;
  return Val_unit;
}

value caml_gr_draw_rect(value vx, value vy, value vw, value vh)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int w = Int_val(vw);
  int h = Int_val(vh);

  caml_gr_check_open();
  if(caml_gr_remember_modeflag)
    XDrawRectangle(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
                   x, Bcvt(y) - h, w, h);
  if(caml_gr_display_modeflag) {
    XDrawRectangle(caml_gr_display, caml_gr_window.win, caml_gr_window.gc,
                   x, Wcvt(y) - h, w, h);
    XFlush(caml_gr_display);
  }
  return Val_unit;
}

value caml_gr_draw_arc_nat(value vx, value vy, value vrx, value vry, value va1,
                           value va2)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int rx = Int_val(vrx);
  int ry = Int_val(vry);
  int a1 = Int_val(va1);
  int a2 = Int_val(va2);

  caml_gr_check_open();
  if(caml_gr_remember_modeflag)
    XDrawArc(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
             x - rx, Bcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  if(caml_gr_display_modeflag) {
    XDrawArc(caml_gr_display, caml_gr_window.win, caml_gr_window.gc,
         x - rx, Wcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
    XFlush(caml_gr_display);
  }
  return Val_unit;
}

value caml_gr_draw_arc(value *argv, int argc)
{
  return caml_gr_draw_arc_nat(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}

value caml_gr_set_line_width(value vwidth)
{
  int width = Int_val(vwidth);

  caml_gr_check_open();
  XSetLineAttributes(caml_gr_display, caml_gr_window.gc,
                     width, LineSolid, CapRound, JoinRound);
  XSetLineAttributes(caml_gr_display, caml_gr_bstore.gc,
                     width, LineSolid, CapRound, JoinRound);
  return Val_unit;
}
