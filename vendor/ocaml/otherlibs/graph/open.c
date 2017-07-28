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

#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include "libgraph.h"
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_SETITIMER
#include <sys/time.h>
#endif

Display * caml_gr_display = NULL;
int caml_gr_screen;
Colormap caml_gr_colormap;
int caml_gr_white, caml_gr_black, caml_gr_background;
struct canvas caml_gr_window;
struct canvas caml_gr_bstore;
Bool caml_gr_display_modeflag;
Bool caml_gr_remember_modeflag;
int caml_gr_x, caml_gr_y;
int caml_gr_color;
extern XFontStruct * caml_gr_font;
long caml_gr_selected_events;
Bool caml_gr_ignore_sigio = False;
static Bool caml_gr_initialized = False;
static char * window_name = NULL;

static int caml_gr_error_handler(Display *display, XErrorEvent *error);
static int caml_gr_ioerror_handler(Display *display);
value caml_gr_clear_graph(void);

value caml_gr_open_graph(value arg)
{
  char display_name[256], geometry_spec[64];
  char * p, * q;
  XSizeHints hints;
  int ret;
  XEvent event;
  int x, y, w, h;
  XWindowAttributes attributes;

  if (caml_gr_initialized) {
    caml_gr_clear_graph();
  } else {

    /* Parse the argument */
    for (p = String_val(arg), q = display_name; *p != 0 && *p != ' '; p++)
      if (q < display_name + sizeof(display_name) - 1) *q++ = *p;
    *q = 0;
    while (*p == ' ') p++;
    for (q = geometry_spec; *p != 0; p++)
      if (q < geometry_spec + sizeof(geometry_spec) - 1) *q++ = *p;
    *q = 0;

    /* Open the display */
    if (caml_gr_display == NULL) {
      caml_gr_display = XOpenDisplay(display_name);
      if (caml_gr_display == NULL)
        caml_gr_fail("Cannot open display %s", XDisplayName(display_name));
      caml_gr_screen = DefaultScreen(caml_gr_display);
      caml_gr_black = BlackPixel(caml_gr_display, caml_gr_screen);
      caml_gr_white = WhitePixel(caml_gr_display, caml_gr_screen);
      caml_gr_background = caml_gr_white;
      caml_gr_colormap = DefaultColormap(caml_gr_display, caml_gr_screen);
    }

    /* Set up the error handlers */
    XSetErrorHandler(caml_gr_error_handler);
    XSetIOErrorHandler(caml_gr_ioerror_handler);

    /* Parse the geometry specification */
    hints.x = 0;
    hints.y = 0;
    hints.width = DEFAULT_SCREEN_WIDTH;
    hints.height = DEFAULT_SCREEN_HEIGHT;
    hints.flags = PPosition | PSize;
    hints.win_gravity = 0;

    ret = XWMGeometry(caml_gr_display, caml_gr_screen, geometry_spec, "",
                      BORDER_WIDTH,
                      &hints, &x, &y, &w, &h, &hints.win_gravity);
    if (ret & (XValue | YValue)) {
      hints.x = x; hints.y = y; hints.flags |= USPosition;
    }
    if (ret & (WidthValue | HeightValue)) {
      hints.width = w; hints.height = h; hints.flags |= USSize;
    }

    /* Initial drawing color is black */
    caml_gr_color = 0; /* CAML COLOR */

    /* Create the on-screen window */
    caml_gr_window.w = hints.width;
    caml_gr_window.h = hints.height;
    caml_gr_window.win =
      XCreateSimpleWindow(caml_gr_display, DefaultRootWindow(caml_gr_display),
                          hints.x, hints.y, hints.width, hints.height,
                          BORDER_WIDTH, caml_gr_black, caml_gr_background);
    p = window_name;
    if (p == NULL) p = DEFAULT_WINDOW_NAME;
    /* What not use XSetWMProperties? */
    XSetStandardProperties(caml_gr_display, caml_gr_window.win, p, p,
                           None, NULL, 0, &hints);
    caml_gr_window.gc = XCreateGC(caml_gr_display, caml_gr_window.win, 0, NULL);
    XSetBackground(caml_gr_display, caml_gr_window.gc, caml_gr_background);
    XSetForeground(caml_gr_display, caml_gr_window.gc, caml_gr_black);

    /* Require exposure, resize and keyboard events */
    caml_gr_selected_events = DEFAULT_SELECTED_EVENTS;
    XSelectInput(caml_gr_display, caml_gr_window.win, caml_gr_selected_events);

    /* Map the window on the screen and wait for the first Expose event */
    XMapWindow(caml_gr_display, caml_gr_window.win);
    do { XNextEvent(caml_gr_display, &event); } while (event.type != Expose);

    /* Get the actual window dimensions */
    XGetWindowAttributes(caml_gr_display, caml_gr_window.win, &attributes);
    caml_gr_window.w = attributes.width;
    caml_gr_window.h = attributes.height;

    /* Create the pixmap used for backing store */
    caml_gr_bstore.w = caml_gr_window.w;
    caml_gr_bstore.h = caml_gr_window.h;
    caml_gr_bstore.win =
      XCreatePixmap(caml_gr_display, caml_gr_window.win, caml_gr_bstore.w,
                    caml_gr_bstore.h,
                    XDefaultDepth(caml_gr_display, caml_gr_screen));
    caml_gr_bstore.gc = XCreateGC(caml_gr_display, caml_gr_bstore.win, 0, NULL);
    XSetBackground(caml_gr_display, caml_gr_bstore.gc, caml_gr_background);

    /* Clear the pixmap */
    XSetForeground(caml_gr_display, caml_gr_bstore.gc, caml_gr_background);
    XFillRectangle(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
                   0, 0, caml_gr_bstore.w, caml_gr_bstore.h);
    XSetForeground(caml_gr_display, caml_gr_bstore.gc, caml_gr_black);

    /* Set the display and remember modes on */
    caml_gr_display_modeflag = True ;
    caml_gr_remember_modeflag = True ;

    /* The global data structures are now correctly initialized.
       In particular, caml_gr_sigio_handler can now handle events safely. */
    caml_gr_initialized = True;

    /* If possible, request that system calls be restarted after
       the EVENT_SIGNAL signal. */
#ifdef POSIX_SIGNALS
#ifdef SA_RESTART
    { struct sigaction action;
      sigaction(EVENT_SIGNAL, NULL, &action);
      action.sa_flags |= SA_RESTART;
      sigaction(EVENT_SIGNAL, &action, NULL);
    }
#endif
#endif

#ifdef USE_ASYNC_IO
    /* If BSD-style asynchronous I/O are supported:
       arrange for I/O on the connection to trigger the SIGIO signal */
    ret = fcntl(ConnectionNumber(caml_gr_display), F_GETFL, 0);
    fcntl(ConnectionNumber(caml_gr_display), F_SETFL, ret | FASYNC);
    fcntl(ConnectionNumber(caml_gr_display), F_SETOWN, getpid());
#endif
  }
#ifdef USE_INTERVAL_TIMER
  /* If BSD-style interval timers are provided, use the real-time timer
     to poll events. */
  { struct itimerval it;
    it.it_interval.tv_sec = 0;
    it.it_interval.tv_usec = 250000;
    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 250000;
    setitimer(ITIMER_REAL, &it, NULL);
  }
#endif
#ifdef USE_ALARM
  /* The poor man's solution: use alarm to poll events. */
  alarm(1);
#endif
  /* Position the current point at origin */
  caml_gr_x = 0;
  caml_gr_y = 0;
  /* Reset the color cache */
  caml_gr_init_color_cache();
  caml_gr_init_direct_rgb_to_pixel();
  return Val_unit;
}

value caml_gr_close_graph(void)
{
  if (caml_gr_initialized) {
#ifdef USE_INTERVAL_TIMER
    struct itimerval it;
    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 0;
    setitimer(ITIMER_REAL, &it, NULL);
#endif
    caml_gr_initialized = False;
    if (caml_gr_font != NULL) {
      XFreeFont(caml_gr_display, caml_gr_font); caml_gr_font = NULL;
    }
    XFreeGC(caml_gr_display, caml_gr_window.gc);
    XDestroyWindow(caml_gr_display, caml_gr_window.win);
    XFreeGC(caml_gr_display, caml_gr_bstore.gc);
    XFreePixmap(caml_gr_display, caml_gr_bstore.win);
    XFlush(caml_gr_display);
    XCloseDisplay (caml_gr_display);
    caml_gr_display = NULL;
  }
  return Val_unit;
}

value caml_gr_id_of_window(Window win)
{
  char tmp[256];

  sprintf(tmp, "%lu", (unsigned long)win);
  return copy_string( tmp );
}

value caml_gr_window_id(void)
{
  caml_gr_check_open();
  return caml_gr_id_of_window(caml_gr_window.win);
}

value caml_gr_set_window_title(value n)
{
  if (window_name != NULL) stat_free(window_name);
  window_name = caml_strdup(String_val(n));
  if (caml_gr_initialized) {
    XStoreName(caml_gr_display, caml_gr_window.win, window_name);
    XSetIconName(caml_gr_display, caml_gr_window.win, window_name);
    XFlush(caml_gr_display);
  }
  return Val_unit;
}

value caml_gr_resize_window (value vx, value vy)
{
  caml_gr_check_open ();

  caml_gr_window.w = Int_val (vx);
  caml_gr_window.h = Int_val (vy);
  XResizeWindow (caml_gr_display, caml_gr_window.win, caml_gr_window.w,
                 caml_gr_window.h);

  XFreeGC(caml_gr_display, caml_gr_bstore.gc);
  XFreePixmap(caml_gr_display, caml_gr_bstore.win);

  caml_gr_bstore.w = caml_gr_window.w;
  caml_gr_bstore.h = caml_gr_window.h;
  caml_gr_bstore.win =
    XCreatePixmap(caml_gr_display, caml_gr_window.win, caml_gr_bstore.w,
                  caml_gr_bstore.h,
                  XDefaultDepth(caml_gr_display, caml_gr_screen));
  caml_gr_bstore.gc = XCreateGC(caml_gr_display, caml_gr_bstore.win, 0, NULL);
  XSetBackground(caml_gr_display, caml_gr_bstore.gc, caml_gr_background);

  caml_gr_clear_graph ();
  return Val_unit;
}

value caml_gr_clear_graph(void)
{
  caml_gr_check_open();
  if(caml_gr_remember_modeflag) {
    XSetForeground(caml_gr_display, caml_gr_bstore.gc, caml_gr_white);
    XFillRectangle(caml_gr_display, caml_gr_bstore.win, caml_gr_bstore.gc,
                   0, 0, caml_gr_bstore.w, caml_gr_bstore.h);
    XSetForeground(caml_gr_display, caml_gr_bstore.gc, caml_gr_color);
  }
  if(caml_gr_display_modeflag) {
    XSetForeground(caml_gr_display, caml_gr_window.gc, caml_gr_white);
    XFillRectangle(caml_gr_display, caml_gr_window.win, caml_gr_window.gc,
                   0, 0, caml_gr_window.w, caml_gr_window.h);
    XSetForeground(caml_gr_display, caml_gr_window.gc, caml_gr_color);
    XFlush(caml_gr_display);
  }
  caml_gr_init_color_cache();
  caml_gr_init_direct_rgb_to_pixel();
  return Val_unit;
}

value caml_gr_size_x(void)
{
  caml_gr_check_open();
  return Val_int(caml_gr_window.w);
}

value caml_gr_size_y(void)
{
  caml_gr_check_open();
  return Val_int(caml_gr_window.h);
}

value caml_gr_synchronize(void)
{
  caml_gr_check_open();
  XCopyArea(caml_gr_display, caml_gr_bstore.win, caml_gr_window.win,
            caml_gr_window.gc,
            0, caml_gr_bstore.h - caml_gr_window.h,
            caml_gr_window.w, caml_gr_window.h,
            0, 0);
  XFlush(caml_gr_display);
  return Val_unit ;
}

value caml_gr_display_mode(value flag)
{
  caml_gr_display_modeflag = Bool_val (flag);
  return Val_unit ;
}

value caml_gr_remember_mode(value flag)
{
  caml_gr_remember_modeflag = Bool_val(flag);
  return Val_unit ;
}

/* The caml_gr_sigio_handler is called via the signal machinery in the bytecode
   interpreter. The signal system ensures that this function will be
   called either between two bytecode instructions, or during a blocking
   primitive. In either case, not in the middle of an Xlib call. */

value caml_gr_sigio_signal(value unit)
{
  return Val_int(EVENT_SIGNAL);
}

value caml_gr_sigio_handler(void)
{
  XEvent grevent;

  if (caml_gr_initialized && !caml_gr_ignore_sigio) {
    while (XCheckMaskEvent(caml_gr_display, -1 /*all events*/, &grevent)) {
      caml_gr_handle_event(&grevent);
    }
  }
#ifdef USE_ALARM
  alarm(1);
#endif
  return Val_unit;
}

/* Processing of graphic errors */

static value * graphic_failure_exn = NULL;

void caml_gr_fail(char *fmt, char *arg)
{
  char buffer[1024];

  if (graphic_failure_exn == NULL) {
    graphic_failure_exn = caml_named_value("Graphics.Graphic_failure");
    if (graphic_failure_exn == NULL)
      invalid_argument("Exception Graphics.Graphic_failure not initialized,"
                       " must link graphics.cma");
  }
  sprintf(buffer, fmt, arg);
  raise_with_string(*graphic_failure_exn, buffer);
}

void caml_gr_check_open(void)
{
  if (!caml_gr_initialized) caml_gr_fail("graphic screen not opened", NULL);
}

static int caml_gr_error_handler(Display *display, XErrorEvent *error)
{
  char errmsg[512];
  XGetErrorText(error->display, error->error_code, errmsg, sizeof(errmsg));
  caml_gr_fail("Xlib error: %s", errmsg);
  return 0;
}

static int caml_gr_ioerror_handler(Display *display)
{
  caml_gr_fail("fatal I/O error", NULL);
  return 0;
}
