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
#include "image.h"
#include <caml/alloc.h>
#include <caml/custom.h>

static void caml_gr_free_image(value im)
{
  XFreePixmap(caml_gr_display, Data_im(im));
  if (Mask_im(im) != None) XFreePixmap(caml_gr_display, Mask_im(im));
}

static struct custom_operations image_ops = {
  "_image",
  caml_gr_free_image,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

#define Max_image_mem 2000000

value caml_gr_new_image(int w, int h)
{
  value res = alloc_custom(&image_ops, sizeof(struct grimage),
                           w * h, Max_image_mem);
  Width_im(res) = w;
  Height_im(res) = h;
  Data_im(res) = XCreatePixmap(caml_gr_display, caml_gr_window.win, w, h,
                               XDefaultDepth(caml_gr_display, caml_gr_screen));
  Mask_im(res) = None;
  return res;
}

value caml_gr_create_image(value vw, value vh)
{
  caml_gr_check_open();
  return caml_gr_new_image(Int_val(vw), Int_val(vh));
}

value caml_gr_blit_image(value im, value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  caml_gr_check_open();
  XCopyArea(caml_gr_display, caml_gr_bstore.win, Data_im(im), caml_gr_bstore.gc,
            x, Bcvt(y) + 1 - Height_im(im),
            Width_im(im), Height_im(im),
            0, 0);
  return Val_unit;
}

value caml_gr_draw_image(value im, value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int wy = Wcvt(y) + 1 - Height_im(im);
  int by = Bcvt(y) + 1 - Height_im(im);

  caml_gr_check_open();
  if (Mask_im(im) != None) {
    if(caml_gr_remember_modeflag) {
      XSetClipOrigin(caml_gr_display, caml_gr_bstore.gc, x, by);
      XSetClipMask(caml_gr_display, caml_gr_bstore.gc, Mask_im(im));
    }
    if(caml_gr_display_modeflag) {
      XSetClipOrigin(caml_gr_display, caml_gr_window.gc, x, wy);
      XSetClipMask(caml_gr_display, caml_gr_window.gc, Mask_im(im));
    }
  }
  if(caml_gr_remember_modeflag)
    XCopyArea(caml_gr_display, Data_im(im), caml_gr_bstore.win,
              caml_gr_bstore.gc,
              0, 0,
              Width_im(im), Height_im(im),
              x, by);
  if(caml_gr_display_modeflag)
    XCopyArea(caml_gr_display, Data_im(im), caml_gr_window.win,
              caml_gr_window.gc,
          0, 0,
          Width_im(im), Height_im(im),
          x, wy);
  if (Mask_im(im) != None) {
    if(caml_gr_remember_modeflag)
      XSetClipMask(caml_gr_display, caml_gr_bstore.gc, None);
    if(caml_gr_display_modeflag)
      XSetClipMask(caml_gr_display, caml_gr_window.gc, None);
  }
  if(caml_gr_display_modeflag)
    XFlush(caml_gr_display);
  return Val_unit;
}
