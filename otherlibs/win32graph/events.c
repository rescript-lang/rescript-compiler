/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2004 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "libgraph.h"
#include <windows.h>

enum {
  EVENT_BUTTON_DOWN = 1,
  EVENT_BUTTON_UP = 2,
  EVENT_KEY_PRESSED = 4,
  EVENT_MOUSE_MOTION = 8
};

struct event_data {
  short mouse_x, mouse_y;
  unsigned char kind;
  unsigned char button;
  unsigned char key;
};

static struct event_data caml_gr_queue[SIZE_QUEUE];
static unsigned int caml_gr_head = 0;       /* position of next read */
static unsigned int caml_gr_tail = 0;       /* position of next write */

static int caml_gr_event_mask = EVENT_KEY_PRESSED;
static int last_button = 0;
static LPARAM last_pos = 0;

HANDLE caml_gr_queue_semaphore = NULL;
CRITICAL_SECTION caml_gr_queue_mutex;

void caml_gr_init_event_queue(void)
{
  if (caml_gr_queue_semaphore == NULL) {
    caml_gr_queue_semaphore = CreateSemaphore(NULL, 0, SIZE_QUEUE, NULL);
    InitializeCriticalSection(&caml_gr_queue_mutex);
  }
}

#define QueueIsEmpty (caml_gr_tail == caml_gr_head)

static void caml_gr_enqueue_event(int kind, LPARAM mouse_xy,
                                  int button, int key)
{
  struct event_data * ev;

  if ((caml_gr_event_mask & kind) == 0) return;
  EnterCriticalSection(&caml_gr_queue_mutex);
  ev = &(caml_gr_queue[caml_gr_tail]);
  ev->kind = kind;
  ev->mouse_x = GET_X_LPARAM(mouse_xy);
  ev->mouse_y = GET_Y_LPARAM(mouse_xy);
  ev->button = (button != 0);
  ev->key = key;
  caml_gr_tail = (caml_gr_tail + 1) % SIZE_QUEUE;
  /* If queue was full, it now appears empty;
     drop oldest entry from queue. */
  if (QueueIsEmpty) {
    caml_gr_head = (caml_gr_head + 1) % SIZE_QUEUE;
  } else {
    /* One more event in queue */
    ReleaseSemaphore(caml_gr_queue_semaphore, 1, NULL);
  }
  LeaveCriticalSection(&caml_gr_queue_mutex);
}

void caml_gr_handle_event(UINT msg, WPARAM wParam, LPARAM lParam)
{
  switch (msg) {
  case WM_LBUTTONDOWN:
  case WM_RBUTTONDOWN:
  case WM_MBUTTONDOWN:
    last_button = 1;
    last_pos = lParam;
    caml_gr_enqueue_event(EVENT_BUTTON_DOWN, lParam, 1, 0);
    break;

  case WM_LBUTTONUP:
  case WM_RBUTTONUP:
  case WM_MBUTTONUP:
    last_button = 0;
    last_pos = lParam;
    caml_gr_enqueue_event(EVENT_BUTTON_UP, lParam, 0, 0);
    break;

  case WM_CHAR:
    caml_gr_enqueue_event(EVENT_KEY_PRESSED, last_pos, last_button, wParam);
    break;

  case WM_MOUSEMOVE:
    last_pos = lParam;
    caml_gr_enqueue_event(EVENT_MOUSE_MOTION, lParam, last_button, 0);
    break;
  }
}

static value caml_gr_wait_allocate_result(int mouse_x, int mouse_y,
                                          int button,
                                          int keypressed, int key)
{
  value res = alloc_small(5, 0);
  Field(res, 0) = Val_int(mouse_x);
  Field(res, 1) = Val_int(grwindow.height - 1 - mouse_y);
  Field(res, 2) = Val_bool(button);
  Field(res, 3) = Val_bool(keypressed);
  Field(res, 4) = Val_int(key & 0xFF);
  return res;
}

static value caml_gr_wait_event_poll(void)
{
  int key, keypressed, i;

  /* Look inside event queue for pending KeyPress events */
  EnterCriticalSection(&caml_gr_queue_mutex);
  key = 0;
  keypressed = 0;
  for (i = caml_gr_head; i != caml_gr_tail; i = (i + 1) % SIZE_QUEUE) {
    if (caml_gr_queue[i].kind == EVENT_KEY_PRESSED) {
      keypressed = 1;
      key = caml_gr_queue[i].key;
      break;
    }
  }
  LeaveCriticalSection(&caml_gr_queue_mutex);
  /* Use global vars for mouse position and buttons */
  return caml_gr_wait_allocate_result(GET_X_LPARAM(last_pos),
                                      GET_Y_LPARAM(last_pos),
                                      last_button,
                                      keypressed, key);
}

static value caml_gr_wait_event_blocking(int mask)
{
  struct event_data ev;

  /* Increase the selected events if needed */
  caml_gr_event_mask |= mask;
  /* Pop events from queue until one matches */
  do {
    /* Wait for event queue to be non-empty */
    WaitForSingleObject(caml_gr_queue_semaphore, INFINITE);
    /* Pop oldest event in queue */
    EnterCriticalSection(&caml_gr_queue_mutex);
    ev = caml_gr_queue[caml_gr_head];
    /* Queue should never be empty at this point, but just in case... */
    if (QueueIsEmpty) {
      ev.kind = 0;
    } else {
      caml_gr_head = (caml_gr_head + 1) % SIZE_QUEUE;
    }
    LeaveCriticalSection(&caml_gr_queue_mutex);
    /* Check if it matches */
  } while ((ev.kind & mask) == 0);
  return caml_gr_wait_allocate_result(ev.mouse_x, ev.mouse_y, ev.button,
                                      ev.kind == EVENT_KEY_PRESSED,
                                      ev.key);
}

CAMLprim value caml_gr_wait_event(value eventlist) /* ML */
{
  int mask, poll;

  gr_check_open();
  mask = 0;
  poll = 0;
  while (eventlist != Val_int(0)) {
    switch (Int_val(Field(eventlist, 0))) {
    case 0:                     /* Button_down */
      mask |= EVENT_BUTTON_DOWN; break;
    case 1:                     /* Button_up */
      mask |= EVENT_BUTTON_UP; break;
    case 2:                     /* Key_pressed */
      mask |= EVENT_KEY_PRESSED; break;
    case 3:                     /* Mouse_motion */
      mask |= EVENT_MOUSE_MOTION; break;
    case 4:                     /* Poll */
      poll = 1; break;
    }
    eventlist = Field(eventlist, 1);
  }
  if (poll)
    return caml_gr_wait_event_poll();
  else
    return caml_gr_wait_event_blocking(mask);
}
