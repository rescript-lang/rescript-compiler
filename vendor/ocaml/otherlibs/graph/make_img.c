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
#include <caml/memory.h>

value caml_gr_make_image(value m)
{
  int width, height;
  value im;
  Bool has_transp;
  XImage * idata, * imask;
  char * bdata, * bmask;
  int i, j, rgb;
  value line;
  GC gc;

  caml_gr_check_open();
  height = Wosize_val(m);
  if (height == 0) return caml_gr_new_image(0, 0);
  width = Wosize_val(Field(m, 0));
  for (i = 1; i < height; i++)
    if (Wosize_val(Field(m, i)) != width)
      caml_gr_fail("make_image: lines of different lengths", NULL);

  /* Build an XImage for the data part of the image */
  idata =
    XCreateImage(caml_gr_display,
                 DefaultVisual(caml_gr_display, caml_gr_screen),
                 XDefaultDepth(caml_gr_display, caml_gr_screen),
                 ZPixmap, 0, NULL, width, height,
                 BitmapPad(caml_gr_display), 0);

  bdata = (char *) caml_stat_alloc(height * idata->bytes_per_line);
  idata->data = bdata;
  has_transp = False;

  for (i = 0; i < height; i++) {
    line = Field(m, i);
    for (j = 0; j < width; j++) {
      rgb = Int_val(Field(line, j));
      if (rgb == Transparent) { has_transp = True; rgb = 0; }
      XPutPixel(idata, j, i, caml_gr_pixel_rgb(rgb));
    }
  }

  /* If the matrix contains transparent points,
     build an XImage for the mask part of the image */
  if (has_transp) {
    imask =
      XCreateImage(caml_gr_display,
                   DefaultVisual(caml_gr_display, caml_gr_screen),
                   1, ZPixmap, 0, NULL, width, height,
                   BitmapPad(caml_gr_display), 0);
    bmask = (char *) caml_stat_alloc(height * imask->bytes_per_line);
    imask->data = bmask;

    for (i = 0; i < height; i++) {
      line = Field(m, i);
      for (j = 0; j < width; j++) {
        rgb = Int_val(Field(line, j));
        XPutPixel(imask, j, i, rgb != Transparent);
      }
    }
  } else {
    imask = NULL;
  }

  /* Allocate the image and store the XImages into the Pixmaps */
  im = caml_gr_new_image(width, height);
  gc = XCreateGC(caml_gr_display, Data_im(im), 0, NULL);
  XPutImage(caml_gr_display, Data_im(im), gc, idata, 0, 0, 0, 0, width, height);
  XDestroyImage(idata);
  XFreeGC(caml_gr_display, gc);
  if (has_transp) {
    Mask_im(im) = XCreatePixmap(caml_gr_display, caml_gr_window.win, width,
                                height, 1);
    gc = XCreateGC(caml_gr_display, Mask_im(im), 0, NULL);
    XPutImage(caml_gr_display, Mask_im(im), gc, imask, 0, 0, 0, 0, width,
              height);
    XDestroyImage(imask);
    XFreeGC(caml_gr_display, gc);
  }
  XFlush(caml_gr_display);
  return im;
}
