/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2006 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "stack.h"

int caml_backtrace_active = 0;
int caml_backtrace_pos = 0;
code_t * caml_backtrace_buffer = NULL;
value caml_backtrace_last_exn = Val_unit;
#define BACKTRACE_BUFFER_SIZE 1024

/* In order to prevent the GC from walking through the debug information
   (which have no headers), we transform frame_descr pointers into
   31/63 bits ocaml integers by shifting them by 1 to the right. We do
   not lose information as descr pointers are aligned.

   In particular, we do not need to use [caml_initialize] when setting
   an array element with such a value.
*/
#define Val_Descrptr(descr) Val_long((uintnat)descr>>1)
#define Descrptr_Val(v) ((frame_descr *) (Long_val(v)<<1))

/* Start or stop the backtrace machinery */

CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;
    if (flag) {
      caml_register_global_root(&caml_backtrace_last_exn);
    } else {
      caml_remove_global_root(&caml_backtrace_last_exn);
    }
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(caml_backtrace_active);
}

/* returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */

frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp)
{
  frame_descr * d;
  uintnat h;

  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  while (1) {
    h = Hash_retaddr(*pc);
    while (1) {
      d = caml_frame_descriptors[h];
      if (d == 0) return NULL; /* can happen if some code compiled without -g */
      if (d->retaddr == *pc) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, update sp/pc and return the frame descriptor */
#ifndef Stack_grows_upwards
      *sp += (d->frame_size & 0xFFFC);
#else
      *sp -= (d->frame_size & 0xFFFC);
#endif
      *pc = Saved_return_address(*sp);
#ifdef Mask_already_scanned
      *pc = Mask_already_scanned(*pc);
#endif
      return d;
    } else {
      /* Special frame marking the top of a stack chunk for an ML callback.
         Skip C portion of stack and continue with next ML stack chunk. */
      struct caml_context * next_context = Callback_link(*sp);
      *sp = next_context->bottom_of_stack;
      *pc = next_context->last_retaddr;
      /* A null sp means no more ML stack chunks; stop here. */
      if (*sp == NULL) return NULL;
    }
  }
}

/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented. */

void caml_stash_backtrace(value exn, uintnat pc, char * sp, char * trapsp)
{
  if (exn != caml_backtrace_last_exn) {
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = exn;
  }
  if (caml_backtrace_buffer == NULL) {
    Assert(caml_backtrace_pos == 0);
    caml_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (caml_backtrace_buffer == NULL) return;
  }

  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) return;
    /* store its descriptor in the backtrace buffer */
    if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
    caml_backtrace_buffer[caml_backtrace_pos++] = (code_t) descr;

    /* Stop when we reach the current exception handler */
#ifndef Stack_grows_upwards
    if (sp > trapsp) return;
#else
    if (sp < trapsp) return;
#endif
  }
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

CAMLprim value caml_get_current_callstack(value max_frames_value) {
  CAMLparam1(max_frames_value);
  CAMLlocal1(trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_size;

  /* first compute the size of the trace */
  {
    uintnat pc = caml_last_return_address;
    /* note that [caml_bottom_of_stack] always points to the most recent
     * frame, independently of the [Stack_grows_upwards] setting */
    char * sp = caml_bottom_of_stack;
    char * limitsp = caml_top_of_stack;

    trace_size = 0;
    while (1) {
      frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
      if (descr == NULL) break;
      if (trace_size >= max_frames) break;
      ++trace_size;

#ifndef Stack_grows_upwards
      if (sp > limitsp) break;
#else
      if (sp < limitsp) break;
#endif
    }
  }

  trace = caml_alloc((mlsize_t) trace_size, 0);

  /* then collect the trace */
  {
    uintnat pc = caml_last_return_address;
    char * sp = caml_bottom_of_stack;
    intnat trace_pos;

    for (trace_pos = 0; trace_pos < trace_size; trace_pos++) {
      frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
      Assert(descr != NULL);
      Field(trace, trace_pos) = Val_Descrptr(descr);
    }
  }

  CAMLreturn(trace);
}

/* Extract location information for the given frame descriptor */

CAMLexport void extract_location_info(frame_descr * d,
                                  /*out*/ struct caml_loc_info * li)
{
  uintnat infoptr;
  uint32 info1, info2;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if ((d->frame_size & 1) == 0) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    return;
  }
  /* Recover debugging info */
  infoptr = ((uintnat) d +
             sizeof(char *) + sizeof(short) + sizeof(short) +
             sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
            & -sizeof(frame_descr *);
  info1 = ((uint32 *)infoptr)[0];
  info2 = ((uint32 *)infoptr)[1];
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn kk
                          44       36         26                       2  0
                       (32+12)    (32+4)
     k ( 2 bits): 0 if it's a call, 1 if it's a raise
     n (24 bits): offset (in 4-byte words) of file name relative to infoptr
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 3) != 0;
  li->loc_filename = (char *) infoptr + (info1 & 0x3FFFFFC);
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

/* Print location information -- same behavior as in Printexc

   note that the test for compiler-inserted raises is slightly redundant:
     (!li->loc_valid && li->loc_is_raise)
   extract_location_info above guarantees that when li->loc_valid is
   0, then li->loc_is_raise is always 1, so the latter test is
   useless. We kept it to keep code identical to the byterun/
   implementation. */

static void print_location(struct caml_loc_info * li, int index)
{
  char * info;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid && li->loc_is_raise) return;

  if (li->loc_is_raise) {
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    if (index == 0)
      info = "Raised by primitive operation at";
    else
      info = "Called from";
  }
  if (! li->loc_valid) {
    fprintf(stderr, "%s unknown location\n", info);
  } else {
    fprintf (stderr, "%s file \"%s\", line %d, characters %d-%d\n",
             info, li->loc_filename, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }
}

/* Print a backtrace */

void caml_print_exception_backtrace(void)
{
  int i;
  struct caml_loc_info li;

  for (i = 0; i < caml_backtrace_pos; i++) {
    extract_location_info((frame_descr *) (caml_backtrace_buffer[i]), &li);
    print_location(&li, i);
  }
}

/* Convert the raw backtrace to a data structure usable from OCaml */

CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot) {
  CAMLparam1(backtrace_slot);
  CAMLlocal2(p, fname);
  struct caml_loc_info li;

  extract_location_info(Descrptr_Val(backtrace_slot), &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(5, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
  } else {
    p = caml_alloc_small(1, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
  }

  CAMLreturn(p);
}

/* Get a copy of the latest backtrace */

CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  const int tag = 0;

  /* Beware: the allocations below may cause finalizers to be run, and another
     backtrace---possibly of a different length---to be stashed (for example
     if the finalizer raises then catches an exception).  We choose to ignore
     any such finalizer backtraces and return the original one. */

  if (caml_backtrace_buffer == NULL || caml_backtrace_pos == 0) {
    res = caml_alloc(0, tag);
  }
  else {
    code_t saved_caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE];
    int saved_caml_backtrace_pos;
    intnat i;

    saved_caml_backtrace_pos = caml_backtrace_pos;

    if (saved_caml_backtrace_pos > BACKTRACE_BUFFER_SIZE) {
      saved_caml_backtrace_pos = BACKTRACE_BUFFER_SIZE;
    }

    memcpy(saved_caml_backtrace_buffer, caml_backtrace_buffer,
           saved_caml_backtrace_pos * sizeof(code_t));

    res = caml_alloc(saved_caml_backtrace_pos, tag);
    for (i = 0; i < saved_caml_backtrace_pos; i++) {
      /* [Val_Descrptr] always returns an immediate. */
      Field(res, i) = Val_Descrptr(saved_caml_backtrace_buffer[i]);
    }
  }

  CAMLreturn(res);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.
*/

CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal3(arr, res, backtrace);
  intnat i;

  backtrace = caml_get_exception_raw_backtrace(Val_unit);

  arr = caml_alloc(Wosize_val(backtrace), 0);
  for (i = 0; i < Wosize_val(backtrace); i++) {
    Store_field(arr, i, caml_convert_raw_backtrace_slot(Field(backtrace, i)));
  }

  res = caml_alloc_small(1, 0); Field(res, 0) = arr; /* Some */
  CAMLreturn(res);
}
