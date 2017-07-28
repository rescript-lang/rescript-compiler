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
#include <X11/Xatom.h>

/* Cache to speed up the translation rgb -> pixel value. */

struct color_cache_entry {
  int rgb;                      /* RGB value with format 0xRRGGBB */
  unsigned long pixel;          /* Pixel value */
};

#define Color_cache_size 512
static struct color_cache_entry color_cache[Color_cache_size];
#define Empty (-1)
#define Hash_rgb(r,g,b) \
  ((((r) & 0xE0) << 1) + (((g) & 0xE0) >> 2) + (((b) & 0xE0) >> 5))
#define Color_cache_slack 16

static int num_overflows = 0;

/* rgb -> pixel conversion *without* display connection */

Bool caml_gr_direct_rgb = False;
int caml_gr_red_l, caml_gr_red_r;
int caml_gr_green_l, caml_gr_green_r;
int caml_gr_blue_l, caml_gr_blue_r;
unsigned long caml_gr_red_mask, caml_gr_green_mask, caml_gr_blue_mask;

/* rgb -> pixel table */
unsigned long caml_gr_red_vals[256];
unsigned long caml_gr_green_vals[256];
unsigned long caml_gr_blue_vals[256];

void caml_gr_get_shifts( unsigned long mask, int *lsl, int *lsr )
{
  int l = 0;
  int r = 0;
  int bit = 1;
  if ( mask == 0 ){ *lsl = -1; *lsr = -1; return; }

  for( l = 0; l < 32; l++ ){
    if( bit & mask ){ break; }
    bit = bit << 1;
  }
  for( r = l; r < 32; r++ ){
    if( ! (bit & mask) ){ break; }
    bit = bit << 1;
  }
  /* fix r */
  if ( r == 32 ) { r = 31; }
  *lsl = l;
  *lsr = 16 - (r - l);
}

void caml_gr_init_direct_rgb_to_pixel(void)
{
  Visual *visual;
  int i;

  visual = DefaultVisual(caml_gr_display,caml_gr_screen);

  if ( visual->class == TrueColor || visual->class == DirectColor ){

    caml_gr_red_mask = visual->red_mask;
    caml_gr_green_mask = visual->green_mask;
    caml_gr_blue_mask = visual->blue_mask;

#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "visual %lx %lx %lx\n",
            caml_gr_red_mask,
            caml_gr_green_mask,
            caml_gr_blue_mask);
#endif

    caml_gr_get_shifts(caml_gr_red_mask, &caml_gr_red_l, &caml_gr_red_r);
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "red %d %d\n", caml_gr_red_l, caml_gr_red_r);
#endif
    for(i=0; i<256; i++){
      caml_gr_red_vals[i] = (((i << 8) + i) >> caml_gr_red_r) << caml_gr_red_l;
    }

    caml_gr_get_shifts(caml_gr_green_mask, &caml_gr_green_l, &caml_gr_green_r);
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "green %d %d\n", caml_gr_green_l, caml_gr_green_r);
#endif
    for(i=0; i<256; i++){
      caml_gr_green_vals[i] =
        (((i << 8) + i) >> caml_gr_green_r) << caml_gr_green_l;
    }

    caml_gr_get_shifts(caml_gr_blue_mask, &caml_gr_blue_l, &caml_gr_blue_r);
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "blue %d %d\n", caml_gr_blue_l, caml_gr_blue_r);
#endif
    for(i=0; i<256; i++){
      caml_gr_blue_vals[i] =
        (((i << 8) + i) >> caml_gr_blue_r) << caml_gr_blue_l;
    }

    if( caml_gr_red_l < 0 || caml_gr_red_r < 0 ||
        caml_gr_green_l < 0 || caml_gr_green_r < 0 ||
        caml_gr_blue_l < 0 || caml_gr_blue_r < 0 ){
#ifdef QUICKCOLORDEBUG
      fprintf(stderr, "Damn, boost failed\n");
#endif
      caml_gr_direct_rgb = False;
    } else {
#ifdef QUICKCOLORDEBUG
      fprintf(stderr, "Boost ok\n");
#endif
      caml_gr_direct_rgb = True;
    }
  } else {
    /* we cannot use direct_rgb_to_pixel */
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "No boost!\n");
#endif
    caml_gr_direct_rgb = False;
  }
}

void caml_gr_init_color_cache(void)
{
  int i;
  for (i = 0; i < Color_cache_size; i++) color_cache[i].rgb = Empty;
  i = Hash_rgb(0, 0, 0);
  color_cache[i].rgb = 0;
  color_cache[i].pixel = caml_gr_black;
  i = Hash_rgb(0xFF, 0xFF, 0xFF);
  color_cache[i].rgb = 0xFFFFFF;
  color_cache[i].pixel = caml_gr_white;
}

unsigned long caml_gr_pixel_rgb(int rgb)
{
  unsigned int r, g, b;
  int h, i;
  XColor color;

  r = (rgb >> 16) & 0xFF;
  g = (rgb >> 8) & 0xFF;
  b = rgb & 0xFF;

  if (caml_gr_direct_rgb){
    return caml_gr_red_vals[r] | caml_gr_green_vals[g] | caml_gr_blue_vals[b];
  }

  h = Hash_rgb(r, g, b);
  i = h;
  while(1) {
    if (color_cache[i].rgb == Empty) break;
    if (color_cache[i].rgb == rgb) return color_cache[i].pixel;
    i = (i + 1) & (Color_cache_size - 1);
    if (i == h) {
        /* Cache is full.  Instead of inserting at slot h, which causes
           thrashing if many colors hash to the same value,
           insert at h + n where n is pseudo-random and
           smaller than Color_cache_slack */
        int slack = num_overflows++ & (Color_cache_slack - 1);
        i = (i + slack) & (Color_cache_size - 1);
        break;
    }
  }
  color.red = r * 0x101;
  color.green = g * 0x101;
  color.blue = b * 0x101;
  XAllocColor(caml_gr_display, caml_gr_colormap, &color);
  color_cache[i].rgb = rgb;
  color_cache[i].pixel = color.pixel;
  return color.pixel;
}

int caml_gr_rgb_pixel(long unsigned int pixel)
{
  register int r,g,b;

  XColor color;
  int i;

  if (caml_gr_direct_rgb) {
    r = (((pixel & caml_gr_red_mask) >> caml_gr_red_l) << 8)
        >> (16 - caml_gr_red_r);
    g = (((pixel & caml_gr_green_mask) >> caml_gr_green_l) << 8)
        >> (16 - caml_gr_green_r);
    b = (((pixel & caml_gr_blue_mask) >> caml_gr_blue_l) << 8)
        >> (16 - caml_gr_blue_r);
    return (r << 16) + (g << 8) + b;
  }

  if (pixel == caml_gr_black) return 0;
  if (pixel == caml_gr_white) return 0xFFFFFF;

  /* Probably faster to do a linear search than to query the X server. */
  for (i = 0; i < Color_cache_size; i++) {
    if (color_cache[i].rgb != Empty && color_cache[i].pixel == pixel)
      return color_cache[i].rgb;
  }
  color.pixel = pixel;
  XQueryColor(caml_gr_display, caml_gr_colormap, &color);
  return
    ((color.red >> 8) << 16) + ((color.green >> 8) << 8) + (color.blue >> 8);
}

value caml_gr_set_color(value vrgb)
{
  int xcolor;
  caml_gr_check_open();
  caml_gr_color = Int_val(vrgb);
  if (caml_gr_color >= 0 ){
    xcolor = caml_gr_pixel_rgb(Int_val(vrgb));
    XSetForeground(caml_gr_display, caml_gr_window.gc, xcolor);
    XSetForeground(caml_gr_display, caml_gr_bstore.gc, xcolor);
  } else {
    XSetForeground(caml_gr_display, caml_gr_window.gc, caml_gr_background);
    XSetForeground(caml_gr_display, caml_gr_bstore.gc, caml_gr_background);
  }
  return Val_unit;
}
