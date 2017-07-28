/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Developed by Jacob Navia, based on code by J-M Geffroy and X Leroy */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <fcntl.h>
#include <signal.h>
#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "libgraph.h"
#include "caml/callback.h"
#include <windows.h>

static value gr_reset(void);
static long tid;
static HANDLE threadHandle;
HWND grdisplay = NULL;
int grscreen;
COLORREF grwhite, grblack;
COLORREF grbackground;
int grCurrentColor;
struct canvas grbstore;
BOOL grdisplay_mode;
BOOL grremember_mode;
int grx, gry;
int grcolor;
extern HFONT * grfont;
MSG msg;

static char *szOcamlWindowClass = "OcamlWindowClass";
static BOOL gr_initialized = 0;
CAMLprim value caml_gr_clear_graph(value unit);
HANDLE hInst;

HFONT CreationFont(char *name)
{
   LOGFONT CurrentFont;
   memset(&CurrentFont, 0, sizeof(LOGFONT));
   CurrentFont.lfCharSet = ANSI_CHARSET;
   CurrentFont.lfWeight = FW_NORMAL;
   CurrentFont.lfHeight = grwindow.CurrentFontSize;
   CurrentFont.lfPitchAndFamily = (BYTE) (FIXED_PITCH | FF_MODERN);
   strncpy(CurrentFont.lfFaceName, name, sizeof(CurrentFont.lfFaceName));
   CurrentFont.lfFaceName[sizeof(CurrentFont.lfFaceName) - 1] = 0;
   return (CreateFontIndirect(&CurrentFont));
}

void SetCoordinates(HWND hwnd)
{
        RECT rc;

        GetClientRect(hwnd,&rc);
        grwindow.width = rc.right;
        grwindow.height = rc.bottom;
        gr_reset();
}

void ResetForClose(HWND hwnd)
{
        DeleteDC(grwindow.tempDC);
        DeleteDC(grwindow.gcBitmap);
        DeleteObject(grwindow.hBitmap);
        memset(&grwindow,0,sizeof(grwindow));
        gr_initialized = 0;
}



static LRESULT CALLBACK GraphicsWndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam)
{
        PAINTSTRUCT ps;
        HDC hdc;

        switch (msg) {
                // Create the MDI client invisible window
        case WM_CREATE:
                break;
        case WM_PAINT:
                hdc = BeginPaint(hwnd,&ps);
                BitBlt(hdc,0,0,grwindow.width,grwindow.height,
                        grwindow.gcBitmap,0,0,SRCCOPY);
                EndPaint(hwnd,&ps);
                break;
                // Move the child windows
        case WM_SIZE:
                // Position the MDI client window between the tool and status bars
                if (wParam != SIZE_MINIMIZED) {
                        SetCoordinates(hwnd);
                }

                return 0;
                // End application
        case WM_DESTROY:
                ResetForClose(hwnd);
                gr_check_open();
                break;
        }
        caml_gr_handle_event(msg, wParam, lParam);
        return DefWindowProc(hwnd, msg, wParam, lParam);
}

int DoRegisterClass(void)
{
        WNDCLASS wc;

        memset(&wc,0,sizeof(WNDCLASS));
        wc.style = CS_HREDRAW|CS_VREDRAW|CS_OWNDC ;
        wc.lpfnWndProc = (WNDPROC)GraphicsWndProc;
        wc.hInstance = hInst;
        wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
        wc.lpszClassName = szOcamlWindowClass;
        wc.lpszMenuName = 0;
        wc.hCursor = LoadCursor(NULL,IDC_ARROW);
        wc.hIcon = 0;
        return RegisterClass(&wc);
}

static value gr_reset(void)
{
        RECT rc;
        int screenx,screeny;

        screenx = GetSystemMetrics(SM_CXSCREEN);
        screeny = GetSystemMetrics(SM_CYSCREEN);
        GetClientRect(grwindow.hwnd,&rc);
        grwindow.gc = GetDC(grwindow.hwnd);
        grwindow.width = rc.right;
        grwindow.height = rc.bottom;
        if (grwindow.gcBitmap == (HDC)0) {
                grwindow.hBitmap = CreateCompatibleBitmap(grwindow.gc,screenx,screeny);
                grwindow.gcBitmap = CreateCompatibleDC(grwindow.gc);
                grwindow.tempDC = CreateCompatibleDC(grwindow.gc);
                SelectObject(grwindow.gcBitmap,grwindow.hBitmap);
                SetMapMode(grwindow.gcBitmap,MM_TEXT);
                MoveToEx(grwindow.gcBitmap,0,grwindow.height-1,0);
                BitBlt(grwindow.gcBitmap,0,0,screenx,screeny,
                        grwindow.gcBitmap,0,0,WHITENESS);
                grwindow.CurrentFontSize = 15;
                grwindow.CurrentFont = CreationFont("Courier");
        }
        grwindow.CurrentColor = GetSysColor(COLOR_WINDOWTEXT);
        grwindow.grx = 0;
        grwindow.gry = 0;
        grwindow.CurrentPen = SelectObject(grwindow.gc,GetStockObject(WHITE_PEN));
        SelectObject(grwindow.gc,grwindow.CurrentPen);
        SelectObject(grwindow.gcBitmap,grwindow.CurrentPen);
        grwindow.CurrentBrush = SelectObject(grwindow.gc,GetStockObject(WHITE_BRUSH));
        SelectObject(grwindow.gc,grwindow.CurrentBrush);
        SelectObject(grwindow.gcBitmap,grwindow.CurrentBrush);
        caml_gr_set_color(Val_long(0));
        SelectObject(grwindow.gc,grwindow.CurrentFont);
        SelectObject(grwindow.gcBitmap,grwindow.CurrentFont);
        grdisplay_mode = grremember_mode = 1;
        MoveToEx(grwindow.gc,0,grwindow.height-1,0);
        MoveToEx(grwindow.gcBitmap,0,grwindow.height-1,0);
        SetTextAlign(grwindow.gcBitmap,TA_BOTTOM);
        SetTextAlign(grwindow.gc,TA_BOTTOM);
        return Val_unit;
}

void SuspendGraphicThread(void)
{
        SuspendThread(threadHandle);
}

void ResumeGraphicThread(void)
{
        ResumeThread(threadHandle);
}

/* For handshake between the event handling thread and the main thread */
static char * open_graph_errmsg;
static HANDLE open_graph_event;

static DWORD WINAPI gr_open_graph_internal(value arg)
{
  RECT rc;
  int ret;
  int event;
  int x, y, w, h;
  int screenx,screeny;
  int attributes;
  static int registered;
  MSG msg;

  gr_initialized = TRUE;
  hInst = GetModuleHandle(NULL);
  x = y = w = h = CW_USEDEFAULT;
  sscanf(String_val(arg), "%dx%d+%d+%d", &w, &h, &x, &y);

  /* Open the display */
  if (grwindow.hwnd == NULL || !IsWindow(grwindow.hwnd)) {
    if (!registered) {
      registered = DoRegisterClass();
      if (!registered) {
        open_graph_errmsg = "Cannot register the window class";
        SetEvent(open_graph_event);
        return 1;
      }
    }
    grwindow.hwnd = CreateWindow(szOcamlWindowClass,
                                 WINDOW_NAME,
                                 WS_OVERLAPPEDWINDOW,
                                 x,y,
                                 w,h,
                                 NULL,0,hInst,NULL);
    if (grwindow.hwnd == NULL) {
      open_graph_errmsg = "Cannot create window";
      SetEvent(open_graph_event);
      return 1;
    }
#if 0
    if (x != CW_USEDEFAULT) {
      rc.left = 0;
      rc.top = 0;
      rc.right = w;
      rc.bottom = h;
      AdjustWindowRect(&rc,GetWindowLong(grwindow.hwnd,GWL_STYLE),0);
      MoveWindow(grwindow.hwnd,x,y,rc.right-rc.left,rc.bottom-rc.top,1);
    }
#endif
  }
  gr_reset();
  ShowWindow(grwindow.hwnd,SW_SHOWNORMAL);

  /* Position the current point at origin */
  grwindow.grx = 0;
  grwindow.gry = 0;

  caml_gr_init_event_queue();

  /* The global data structures are now correctly initialized.
     Restart the OCaml main thread. */
  open_graph_errmsg = NULL;
  SetEvent(open_graph_event);

  /* Enter the message handling loop */
  while (GetMessage(&msg,NULL,0,0)) {
    TranslateMessage(&msg);  // Translates virtual key codes
    DispatchMessage(&msg);   // Dispatches message to window
    if (!IsWindow(grwindow.hwnd))
      break;
  }
  return 0;
}

CAMLprim value caml_gr_open_graph(value arg)
{
  long tid;
  if (gr_initialized) return Val_unit;
  open_graph_event = CreateEvent(NULL, FALSE, FALSE, NULL);
  threadHandle =
    CreateThread(NULL,0,
                 (LPTHREAD_START_ROUTINE)gr_open_graph_internal,(void **)arg,
                 0,
                 &tid);
  WaitForSingleObject(open_graph_event, INFINITE);
  CloseHandle(open_graph_event);
  if (open_graph_errmsg != NULL) gr_fail("%s", open_graph_errmsg);
  return Val_unit;
}

CAMLprim value caml_gr_close_graph(value unit)
{
        if (gr_initialized) {
                PostMessage(grwindow.hwnd, WM_CLOSE, 0, 0);
                WaitForSingleObject(threadHandle, INFINITE);
        }
        return Val_unit;
}

CAMLprim value caml_gr_clear_graph(value unit)
{
        gr_check_open();
        if(grremember_mode) {
                BitBlt(grwindow.gcBitmap,0,0,grwindow.width,grwindow.height,
                        grwindow.gcBitmap,0,0,WHITENESS);
        }
        if(grdisplay_mode) {
                BitBlt(grwindow.gc,0,0,grwindow.width,grwindow.height,
                        grwindow.gc,0,0,WHITENESS);
        }
        return Val_unit;
}

CAMLprim value caml_gr_size_x(value unit)
{
        gr_check_open();
        return Val_int(grwindow.width);
}

CAMLprim value caml_gr_size_y(value unit)
{
        gr_check_open();
        return Val_int(grwindow.height);
}

CAMLprim value caml_gr_resize_window (value vx, value vy)
{
  gr_check_open ();

  /* FIXME TODO implement this function... */

  return Val_unit;
}

CAMLprim value caml_gr_synchronize(value unit)
{
        gr_check_open();
        BitBlt(grwindow.gc,0,0,grwindow.width,grwindow.height,
                grwindow.gcBitmap,0,0,SRCCOPY);
        return Val_unit ;
}

CAMLprim value caml_gr_display_mode(value flag)
{
        grdisplay_mode =  (Int_val(flag)) ? 1 : 0;
        return Val_unit ;
}

CAMLprim value caml_gr_remember_mode(value flag)
{
        grremember_mode = (Int_val(flag)) ? 1 : 0;
        return Val_unit ;
}

CAMLprim value caml_gr_sigio_signal(value unit)
{
        return Val_unit;
}

CAMLprim value caml_gr_sigio_handler(value unit)
{
        return Val_unit;
}


/* Processing of graphic errors */

static value * graphic_failure_exn = NULL;
void gr_fail(char *fmt, char *arg)
{
  char buffer[1024];

  if (graphic_failure_exn == NULL) {
    graphic_failure_exn = caml_named_value("Graphics.Graphic_failure");
    if (graphic_failure_exn == NULL)
      invalid_argument("Exception Graphics.Graphic_failure not initialized, must link graphics.cma");
  }
  sprintf(buffer, fmt, arg);
  raise_with_string(*graphic_failure_exn, buffer);
}

void gr_check_open(void)
{
  if (!gr_initialized) gr_fail("graphic screen not opened", NULL);
}
