/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Jacob Navia, after Xavier Leroy                          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <windows.h>
#include <windowsx.h>

struct canvas {
  int w, h;                     /* Dimensions of the drawable */
  HWND win;                     /* The drawable itself */
  HDC gc;                        /* The associated graphics context */
};

extern HWND grdisplay;     /* The display connection */
extern COLORREF grbackground;
extern BOOL grdisplay_mode;     /* Display-mode flag */
extern BOOL grremember_mode;    /* Remember-mode flag */
extern int grx, gry;            /* Coordinates of the current point */
extern int grcolor;             /* Current *CAML* drawing color (can be -1) */
extern HFONT * grfont;          /* Current font */

extern BOOL direct_rgb;
extern int byte_order;
extern int bitmap_unit;
extern int bits_per_pixel;

#define Wcvt(y) (grwindow.height - 1 - (y))
#define Bcvt(y) (grwindow.height - 1 - (y))
#define WtoB(y) ((y) + WindowRect.bottom - grwindow.h)

#define DEFAULT_SCREEN_WIDTH 1024
#define DEFAULT_SCREEN_HEIGHT 768
#define BORDER_WIDTH 2
#define WINDOW_NAME "OCaml graphics"
#define ICON_NAME "OCaml graphics"
#define SIZE_QUEUE 256

void gr_fail(char *fmt, char *arg);
void gr_check_open(void);
CAMLprim value caml_gr_set_color(value vcolor);

// Windows specific definitions
extern RECT WindowRect;
extern int grCurrentColor;

typedef struct tagWindow {
        HDC gc;
        HDC gcBitmap;
        HWND hwnd;
        HBRUSH CurrentBrush;
        HPEN CurrentPen;
        DWORD CurrentColor;
        int width;
        int height;
        int grx;
        int gry;
        HBITMAP hBitmap;
        HFONT CurrentFont;
        int CurrentFontSize;
        HDC tempDC; // For image operations;
} GR_WINDOW;

extern GR_WINDOW grwindow;
HFONT CreationFont(char *name);
extern void caml_gr_init_event_queue(void);
extern void caml_gr_handle_event(UINT msg, WPARAM wParam, LPARAM lParam);
