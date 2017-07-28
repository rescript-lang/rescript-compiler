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
#include <caml/memory.h>

value caml_gr_fill_rect(value vx, value vy, value vw, value vh)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int w = Int_val(vw);
  int h = Int_val(vh);

  caml_gr_check_open();
  if(caml_gr_remember_modeflag)
    XFillRectangle(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
                   x, Bcvt(y) - h, w + 1, h + 1);
  if(caml_gr_display_modeflag) {
    XFillRectangle(caml_gr_display, caml_gr_window.win, caml_gr_window.gc,
           x, Wcvt(y) - h, w + 1, h + 1);
    XFlush(caml_gr_display);
  }
  return Val_unit;
}

value caml_gr_fill_poly(value array)
{
  XPoint * points;
  int npoints, i;

  caml_gr_check_open();
  npoints = Wosize_val(array);
  points = (XPoint *) caml_stat_alloc(npoints * sizeof(XPoint));
  for (i = 0; i < npoints; i++) {
    points[i].x = Int_val(Field(Field(array, i), 0));
    points[i].y = Bcvt(Int_val(Field(Field(array, i), 1)));
  }
  if(caml_gr_remember_modeflag)
    XFillPolygon(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc, points,
                 npoints, Complex, CoordModeOrigin);
  if(caml_gr_display_modeflag) {
    for (i = 0; i < npoints; i++)
      points[i].y = BtoW(points[i].y);
    XFillPolygon(caml_gr_display, caml_gr_window.win, caml_gr_window.gc, points,
         npoints, Complex, CoordModeOrigin);
    XFlush(caml_gr_display);
  }
  stat_free((char *) points);
  return Val_unit;
}

value caml_gr_fill_arc_nat(value vx, value vy, value vrx, value vry, value va1,
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
    XFillArc(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
             x - rx, Bcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  if(caml_gr_display_modeflag) {
    XFillArc(caml_gr_display, caml_gr_window.win, caml_gr_window.gc,
         x - rx, Wcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
    XFlush(caml_gr_display);
  }
  return Val_unit;
}

value caml_gr_fill_arc(value *argv, int argc)
{
  return caml_gr_fill_arc_nat(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}
