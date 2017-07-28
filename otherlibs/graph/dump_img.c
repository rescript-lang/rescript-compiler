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
#include <caml/memory.h>

value caml_gr_dump_image(value image)
{
  int width, height, i, j;
  XImage * idata, * imask;
  value m = Val_unit;

  Begin_roots2(image, m);
    caml_gr_check_open();
    width = Width_im(image);
    height = Height_im(image);
    m = alloc(height, 0);
    for (i = 0; i < height; i++) {
      value v = alloc(width, 0);
      modify(&Field(m, i), v);
    }

    idata =
      XGetImage(caml_gr_display, Data_im(image), 0, 0, width, height, (-1),
                ZPixmap);
    for (i = 0; i < height; i++)
      for (j = 0; j < width; j++)
        Field(Field(m, i), j) =
          Val_int(caml_gr_rgb_pixel(XGetPixel(idata, j, i)));
    XDestroyImage(idata);

    if (Mask_im(image) != None) {
      imask =
        XGetImage(caml_gr_display, Mask_im(image), 0, 0, width, height, 1,
                  ZPixmap);
      for (i = 0; i < height; i++)
        for (j = 0; j < width; j++)
          if (XGetPixel(imask, j, i) == 0)
            Field(Field(m, i), j) = Val_int(Transparent);
      XDestroyImage(imask);
    }
  End_roots();
  return m;
}
