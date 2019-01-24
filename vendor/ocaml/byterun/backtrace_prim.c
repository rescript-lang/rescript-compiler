/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Stack backtrace for uncaught exceptions */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/io.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/exec.h"
#include "caml/fix_code.h"
#include "caml/memory.h"
#include "caml/startup.h"
#include "caml/stacks.h"
#include "caml/sys.h"
#include "caml/backtrace.h"
#include "caml/fail.h"
#include "caml/backtrace_prim.h"

/* The table of debug information fragments */
struct ext_table caml_debug_info;

CAMLexport char_os * caml_cds_file = NULL;

/* Location of fields in the Instruct.debug_event record */
enum {
  EV_POS = 0,
  EV_MODULE = 1,
  EV_LOC = 2,
  EV_KIND = 3
};

/* Location of fields in the Location.t record. */
enum {
  LOC_START = 0,
  LOC_END = 1,
  LOC_GHOST = 2
};

/* Location of fields in the Lexing.position record. */
enum {
  POS_FNAME = 0,
  POS_LNUM = 1,
  POS_BOL = 2,
  POS_CNUM = 3
};

/* Runtime representation of the debug information, optimized
   for quick lookup */
struct ev_info {
  code_t ev_pc;
  char *ev_filename;
  int ev_lnum;
  int ev_startchr;
  int ev_endchr;
};

struct debug_info {
  code_t start;
  code_t end;
  mlsize_t num_events;
  struct ev_info *events;
  int already_read;
};

static struct debug_info *find_debug_info(code_t pc)
{
  int i;
  for (i = 0; i < caml_debug_info.size; i++) {
    struct debug_info *di = caml_debug_info.contents[i];
    if (pc >= di->start && pc < di->end)
      return di;
  }
  return NULL;
}

static int cmp_ev_info(const void *a, const void *b)
{
  code_t pc_a = ((const struct ev_info*)a)->ev_pc;
  code_t pc_b = ((const struct ev_info*)b)->ev_pc;
  if (pc_a > pc_b) return 1;
  if (pc_a < pc_b) return -1;
  return 0;
}

static struct ev_info *process_debug_events(code_t code_start, value events_heap,
                                     mlsize_t *num_events)
{
  CAMLparam1(events_heap);
  CAMLlocal3(l, ev, ev_start);
  mlsize_t i, j;
  struct ev_info *events;

  /* Compute the size of the required event buffer. */
  *num_events = 0;
  for (i = 0; i < caml_array_length(events_heap); i++)
    for (l = Field(events_heap, i); l != Val_int(0); l = Field(l, 1))
      (*num_events)++;

  if (*num_events == 0)
      CAMLreturnT(struct ev_info *, NULL);

  events = caml_stat_alloc_noexc(*num_events * sizeof(struct ev_info));
  if(events == NULL)
    caml_fatal_error ("caml_add_debug_info: out of memory");

  j = 0;
  for (i = 0; i < caml_array_length(events_heap); i++) {
    for (l = Field(events_heap, i); l != Val_int(0); l = Field(l, 1)) {
      ev = Field(l, 0);

      events[j].ev_pc = (code_t)((char*)code_start
                                 + Long_val(Field(ev, EV_POS)));

      ev_start = Field(Field(ev, EV_LOC), LOC_START);

      {
        uintnat fnsz = caml_string_length(Field(ev_start, POS_FNAME)) + 1;
        events[j].ev_filename = (char*)caml_stat_alloc_noexc(fnsz);
        if(events[j].ev_filename == NULL)
          caml_fatal_error ("caml_add_debug_info: out of memory");
        memcpy(events[j].ev_filename,
            String_val(Field(ev_start, POS_FNAME)),
            fnsz);
      }

      events[j].ev_lnum = Int_val(Field(ev_start, POS_LNUM));
      events[j].ev_startchr =
        Int_val(Field(ev_start, POS_CNUM))
        - Int_val(Field(ev_start, POS_BOL));
      events[j].ev_endchr =
        Int_val(Field(Field(Field(ev, EV_LOC), LOC_END), POS_CNUM))
        - Int_val(Field(ev_start, POS_BOL));

      j++;
    }
  }

  CAMLassert(j == *num_events);

  qsort(events, *num_events, sizeof(struct ev_info), cmp_ev_info);

  CAMLreturnT(struct ev_info *, events);
}

/* Processes a (Instruct.debug_event list array) into a form suitable
   for quick lookup and registers it for the (code_start,code_size) pc range. */
CAMLprim value caml_add_debug_info(code_t code_start, value code_size,
                                   value events_heap)
{
  CAMLparam1(events_heap);
  struct debug_info *debug_info;

  /* build the OCaml-side debug_info value */
  debug_info = caml_stat_alloc(sizeof(struct debug_info));

  debug_info->start = code_start;
  debug_info->end = (code_t)((char*) code_start + Long_val(code_size));
  if (events_heap == Val_unit) {
    debug_info->events = NULL;
    debug_info->num_events = 0;
    debug_info->already_read = 0;
  } else {
    debug_info->events =
      process_debug_events(code_start, events_heap, &debug_info->num_events);
    debug_info->already_read = 1;
  }

  caml_ext_table_add(&caml_debug_info, debug_info);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_remove_debug_info(code_t start)
{
  CAMLparam0();
  CAMLlocal2(dis, prev);

  int i;
  for (i = 0; i < caml_debug_info.size; i++) {
    struct debug_info *di = caml_debug_info.contents[i];
    if (di->start == start) {
      /* note that caml_ext_table_remove calls caml_stat_free on the
         removed resource, bracketing the caml_stat_alloc call in
         caml_add_debug_info. */
      caml_ext_table_remove(&caml_debug_info, di);
      break;
    }
  }

  CAMLreturn(Val_unit);
}

int caml_alloc_backtrace_buffer(void){
  CAMLassert(caml_backtrace_pos == 0);
  caml_backtrace_buffer =
    caml_stat_alloc_noexc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
  if (caml_backtrace_buffer == NULL) return -1;
  return 0;
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void caml_stash_backtrace(value exn, code_t pc, value * sp, int reraise)
{
  if (pc != NULL) pc = pc - 1;
  if (exn != caml_backtrace_last_exn || !reraise) {
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = exn;
  }

  if (caml_backtrace_buffer == NULL && caml_alloc_backtrace_buffer() == -1)
    return;

  if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
  /* testing the code region is needed: PR#1554 */
  if (find_debug_info(pc) != NULL)
    caml_backtrace_buffer[caml_backtrace_pos++] = pc;

  /* Traverse the stack and put all values pointing into bytecode
     into the backtrace buffer. */
  for (/*nothing*/; sp < caml_trapsp; sp++) {
    code_t p = (code_t) *sp;
    if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
    if (find_debug_info(p) != NULL)
      caml_backtrace_buffer[caml_backtrace_pos++] = p;
  }
}

/* returns the next frame pointer (or NULL if none is available);
   updates *sp to point to the following one, and *trsp to the next
   trap frame, which we will skip when we reach it  */

code_t caml_next_frame_pointer(value ** sp, value ** trsp)
{
  while (*sp < caml_stack_high) {
    code_t *p = (code_t*) (*sp)++;
    if(&Trap_pc(*trsp) == p) {
      *trsp = Trap_link(*trsp);
      continue;
    }

    if (find_debug_info(*p) != NULL)
      return *p;
  }
  return NULL;
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

CAMLprim value caml_get_current_callstack(value max_frames_value)
{
  CAMLparam1(max_frames_value);
  CAMLlocal1(trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_size;

  /* first compute the size of the trace */
  {
    value * sp = caml_extern_sp;
    value * trsp = caml_trapsp;

    for (trace_size = 0; trace_size < max_frames; trace_size++) {
      code_t p = caml_next_frame_pointer(&sp, &trsp);
      if (p == NULL) break;
    }
  }

  trace = caml_alloc(trace_size, 0);

  /* then collect the trace */
  {
    value * sp = caml_extern_sp;
    value * trsp = caml_trapsp;
    uintnat trace_pos;

    for (trace_pos = 0; trace_pos < trace_size; trace_pos++) {
      code_t p = caml_next_frame_pointer(&sp, &trsp);
      CAMLassert(p != NULL);
      Field(trace, trace_pos) = Val_backtrace_slot(p);
    }
  }

  CAMLreturn(trace);
}

/* Read the debugging info contained in the current bytecode executable. */

#ifndef O_BINARY
#define O_BINARY 0
#endif

static void read_main_debug_info(struct debug_info *di)
{
  CAMLparam0();
  CAMLlocal3(events, evl, l);
  char_os *exec_name;
  int fd, num_events, orig, i;
  struct channel *chan;
  struct exec_trailer trail;

  CAMLassert(di->already_read == 0);
  di->already_read = 1;

  if (caml_cds_file != NULL) {
    exec_name = caml_cds_file;
  } else {
    exec_name = caml_exe_name;
  }

  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0){
    caml_fatal_error ("executable program file not found");
    CAMLreturn0;
  }

  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "DBUG") != -1) {
    chan = caml_open_descriptor_in(fd);

    num_events = caml_getword(chan);
    events = caml_alloc(num_events, 0);

    for (i = 0; i < num_events; i++) {
      orig = caml_getword(chan);
      evl = caml_input_val(chan);
      caml_input_val(chan); /* Skip the list of absolute directory names */
      /* Relocate events in event list */
      for (l = evl; l != Val_int(0); l = Field(l, 1)) {
        value ev = Field(l, 0);
        Field(ev, EV_POS) = Val_long(Long_val(Field(ev, EV_POS)) + orig);
      }
      /* Record event list */
      Store_field(events, i, evl);
    }

    caml_close_channel(chan);

    di->events = process_debug_events(caml_start_code, events, &di->num_events);
  }

  CAMLreturn0;
}

CAMLexport void caml_init_debug_info(void)
{
  caml_ext_table_init(&caml_debug_info, 1);
  caml_add_debug_info(caml_start_code, Val_long(caml_code_size), Val_unit);
}

int caml_debug_info_available(void)
{
  return (caml_debug_info.size != 0);
}

/* Search the event index for the given PC.  Return -1 if not found. */

static struct ev_info *event_for_location(code_t pc)
{
  uintnat low, high;
  struct debug_info *di = find_debug_info(pc);

  if (di == NULL)
    return NULL;

  if (!di->already_read)
    read_main_debug_info(di);

  if (di->num_events == 0)
    return NULL;

  low = 0;
  high = di->num_events;
  while (low+1 < high) {
    uintnat m = (low+high)/2;
    if(pc < di->events[m].ev_pc) high = m;
    else low = m;
  }
  if (di->events[low].ev_pc == pc)
    return &di->events[low];
  /* ocamlc sometimes moves an event past a following PUSH instruction;
     allow mismatch by 1 instruction. */
  if (di->events[low].ev_pc == pc + 1)
    return &di->events[low];
  if (low+1 < di->num_events && di->events[low+1].ev_pc == pc + 1)
    return &di->events[low+1];

  return NULL;
}

/* Extract location information for the given PC */

void caml_debuginfo_location(debuginfo dbg,
                             /*out*/ struct caml_loc_info * li)
{
  code_t pc = dbg;
  struct ev_info *event = event_for_location(pc);
  li->loc_is_raise =
    caml_is_instruction(*pc, RAISE) ||
    caml_is_instruction(*pc, RERAISE);
  if (event == NULL) {
    li->loc_valid = 0;
    return;
  }
  li->loc_valid = 1;
  li->loc_is_inlined = 0;
  li->loc_filename = event->ev_filename;
  li->loc_lnum = event->ev_lnum;
  li->loc_startchr = event->ev_startchr;
  li->loc_endchr = event->ev_endchr;
}

debuginfo caml_debuginfo_extract(backtrace_slot slot)
{
  return (debuginfo)slot;
}

debuginfo caml_debuginfo_next(debuginfo dbg)
{
  /* No inlining in bytecode */
  return NULL;
}
