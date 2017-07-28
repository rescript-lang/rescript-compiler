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

value caml_gr_point_color(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XImage * im;
  int rgb;

  caml_gr_check_open();
  im = XGetImage(caml_gr_display, caml_gr_bstore.win, x, Bcvt(y), 1, 1, (-1),
                 ZPixmap);
  rgb = caml_gr_rgb_pixel(XGetPixel(im, 0, 0));
  XDestroyImage(im);
  return Val_int(rgb);
}
