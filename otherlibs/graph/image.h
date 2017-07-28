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

struct grimage {
  int width, height;            /* Dimensions of the image */
  Pixmap data;                  /* Pixels */
  Pixmap mask;                  /* Mask for transparent points, or None */
};

#define Width_im(i) (((struct grimage *)Data_custom_val(i))->width)
#define Height_im(i) (((struct grimage *)Data_custom_val(i))->height)
#define Data_im(i) (((struct grimage *)Data_custom_val(i))->data)
#define Mask_im(i) (((struct grimage *)Data_custom_val(i))->mask)

#define Transparent (-1)

value caml_gr_new_image(int w, int h);
