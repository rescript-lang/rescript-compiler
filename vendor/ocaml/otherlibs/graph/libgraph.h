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

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <caml/mlvalues.h>

struct canvas {
  int w, h;                     /* Dimensions of the drawable */
  Drawable win;                 /* The drawable itself */
  GC gc;                        /* The associated graphics context */
};

extern Display * caml_gr_display;     /* The display connection */
extern int caml_gr_screen;            /* The screen number */
extern Colormap caml_gr_colormap;     /* The color map */
extern struct canvas caml_gr_window;  /* The graphics window */
extern struct canvas caml_gr_bstore;  /* The pixmap used for backing store */
extern int caml_gr_white, caml_gr_black;    /* Black and white pixels for X */
extern int caml_gr_background;        /* Background color for X
                                     (used for CAML color -1) */
extern Bool caml_gr_display_modeflag;     /* Display-mode flag */
extern Bool caml_gr_remember_modeflag;    /* Remember-mode flag */
extern int caml_gr_x, caml_gr_y;      /* Coordinates of the current point */
extern int caml_gr_color;        /* Current *CAML* drawing color (can be -1) */
extern XFontStruct * caml_gr_font;    /* Current font */
extern long caml_gr_selected_events;  /* Events we are interested in */
extern Bool caml_gr_ignore_sigio;     /* Whether to consume events on sigio */

extern Bool caml_gr_direct_rgb;
extern int caml_gr_byte_order;
extern int caml_gr_bitmap_unit;
extern int caml_gr_bits_per_pixel;

#define Wcvt(y) (caml_gr_window.h - 1 - (y))
#define Bcvt(y) (caml_gr_bstore.h - 1 - (y))
#define WtoB(y) ((y) + caml_gr_bstore.h - caml_gr_window.h)
#define BtoW(y) ((y) + caml_gr_window.h - caml_gr_bstore.h)
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#define DEFAULT_SCREEN_WIDTH 600
#define DEFAULT_SCREEN_HEIGHT 450
#define BORDER_WIDTH 2
#define DEFAULT_WINDOW_NAME "OCaml graphics"
#define DEFAULT_SELECTED_EVENTS \
            (ExposureMask | KeyPressMask | StructureNotifyMask)
#define DEFAULT_FONT "fixed"
#define SIZE_QUEUE 256

/* To handle events asynchronously */
#ifdef HAS_ASYNC_IO
#define USE_ASYNC_IO
#define EVENT_SIGNAL SIGIO
#else
#ifdef HAS_SETITIMER
#define USE_INTERVAL_TIMER
#define EVENT_SIGNAL SIGALRM
#else
#define USE_ALARM
#define EVENT_SIGNAL SIGALRM
#endif
#endif

extern void caml_gr_fail(char *fmt, char *arg);
extern void caml_gr_check_open(void);
extern unsigned long caml_gr_pixel_rgb(int rgb);
extern int caml_gr_rgb_pixel(long unsigned int pixel);
extern void caml_gr_handle_event(XEvent *e);
extern void caml_gr_init_color_cache(void);
extern void caml_gr_init_direct_rgb_to_pixel(void);
extern value caml_gr_id_of_window( Window w );
