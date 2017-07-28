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

XFontStruct * caml_gr_font = NULL;

static void caml_gr_get_font(char *fontname)
{
  XFontStruct * font = XLoadQueryFont(caml_gr_display, fontname);
  if (font == NULL) caml_gr_fail("cannot find font %s", fontname);
  if (caml_gr_font != NULL) XFreeFont(caml_gr_display, caml_gr_font);
  caml_gr_font = font;
  XSetFont(caml_gr_display, caml_gr_window.gc, caml_gr_font->fid);
  XSetFont(caml_gr_display, caml_gr_bstore.gc, caml_gr_font->fid);
}

value caml_gr_set_font(value fontname)
{
  caml_gr_check_open();
  caml_gr_get_font(String_val(fontname));
  return Val_unit;
}

value caml_gr_set_text_size (value sz)
{
  return Val_unit;
}

static void caml_gr_draw_text(char *txt, int len)
{
  if (caml_gr_font == NULL) caml_gr_get_font(DEFAULT_FONT);
  if (caml_gr_remember_modeflag)
    XDrawString(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
                caml_gr_x, Bcvt(caml_gr_y) - caml_gr_font->descent + 1, txt,
                len);
  if (caml_gr_display_modeflag) {
    XDrawString(caml_gr_display, caml_gr_window.win, caml_gr_window.gc,
                caml_gr_x, Wcvt(caml_gr_y) - caml_gr_font->descent + 1, txt,
                len);
    XFlush(caml_gr_display);
  }
  caml_gr_x += XTextWidth(caml_gr_font, txt, len);
}

value caml_gr_draw_char(value chr)
{
  char str[1];
  caml_gr_check_open();
  str[0] = Int_val(chr);
  caml_gr_draw_text(str, 1);
  return Val_unit;
}

value caml_gr_draw_string(value str)
{
  caml_gr_check_open();
  caml_gr_draw_text(String_val(str), string_length(str));
  return Val_unit;
}

value caml_gr_text_size(value str)
{
  int width;
  value res;
  caml_gr_check_open();
  if (caml_gr_font == NULL) caml_gr_get_font(DEFAULT_FONT);
  width = XTextWidth(caml_gr_font, String_val(str), string_length(str));
  res = alloc_small(2, 0);
  Field(res, 0) = Val_int(width);
  Field(res, 1) = Val_int(caml_gr_font->ascent + caml_gr_font->descent);
  return res;
}
