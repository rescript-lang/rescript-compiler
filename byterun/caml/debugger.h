/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Interface with the debugger */

#ifndef CAML_DEBUGGER_H
#define CAML_DEBUGGER_H

#include "misc.h"
#include "mlvalues.h"

CAMLextern int caml_debugger_in_use;
CAMLextern int caml_debugger_fork_mode; /* non-zero for parent */
extern uintnat caml_event_count;

enum event_kind {
  EVENT_COUNT, BREAKPOINT, PROGRAM_START, PROGRAM_EXIT,
  TRAP_BARRIER, UNCAUGHT_EXC
};

void caml_debugger_init (void);
void caml_debugger (enum event_kind event);
void caml_debugger_cleanup_fork (void);

/* Communication protocol */

/* Requests from the debugger to the runtime system */

enum debugger_request {
  REQ_SET_EVENT = 'e',          /* uint32 pos */
  /* Set an event on the instruction at position pos */
  REQ_SET_BREAKPOINT = 'B',     /* uint32 pos, (char k) */
  /* Set a breakpoint at position pos */
  /* In profiling mode, the breakpoint kind is set to k */
  REQ_RESET_INSTR = 'i',        /* uint32 pos */
  /* Clear an event or breapoint at position pos, restores initial instr. */
  REQ_CHECKPOINT = 'c',         /* no args */
  /* Checkpoint the runtime system by forking a child process.
     Reply is pid of child process or -1 if checkpoint failed. */
  REQ_GO = 'g',                 /* uint32 n */
  /* Run the program for n events.
     Reply is one of debugger_reply described below. */
  REQ_STOP = 's',               /* no args */
  /* Terminate the runtime system */
  REQ_WAIT = 'w',               /* no args */
  /* Reap one dead child (a discarded checkpoint). */
  REQ_INITIAL_FRAME = '0',      /* no args */
  /* Set current frame to bottom frame (the one currently executing).
     Reply is stack offset and current pc. */
  REQ_GET_FRAME = 'f',          /* no args */
  /* Return current frame location (stack offset + current pc). */
  REQ_SET_FRAME = 'S',          /* uint32 stack_offset */
  /* Set current frame to given stack offset. No reply. */
  REQ_UP_FRAME = 'U',           /* uint32 n */
  /* Move one frame up. Argument n is size of current frame (in words).
     Reply is stack offset and current pc, or -1 if top of stack reached. */
  REQ_SET_TRAP_BARRIER = 'b',   /* uint32 offset */
  /* Set the trap barrier at the given offset. */
  REQ_GET_LOCAL = 'L',          /* uint32 slot_number */
  /* Return the local variable at the given slot in the current frame.
     Reply is one value. */
  REQ_GET_ENVIRONMENT = 'E',    /* uint32 slot_number */
  /* Return the local variable at the given slot in the heap environment
     of the current frame. Reply is one value. */
  REQ_GET_GLOBAL = 'G',         /* uint32 global_number */
  /* Return the specified global variable. Reply is one value. */
  REQ_GET_ACCU = 'A',           /* no args */
  /* Return the current contents of the accumulator. Reply is one value. */
  REQ_GET_HEADER = 'H',         /* mlvalue v */
  /* As REQ_GET_OBJ, but sends only the header. */
  REQ_GET_FIELD = 'F',          /* mlvalue v, uint32 fieldnum */
  /* As REQ_GET_OBJ, but sends only one field. */
  REQ_MARSHAL_OBJ = 'M',        /* mlvalue v */
  /* Send a copy of the data structure rooted at v, using the same
     format as [caml_output_value]. */
  REQ_GET_CLOSURE_CODE = 'C',   /* mlvalue v */
  /* Send the code address of the given closure.
     Reply is one uint32. */
  REQ_SET_FORK_MODE = 'K'       /* uint32 m */
  /* Set whether to follow the child (m=0) or the parent on fork. */
};

/* Replies to a REQ_GO request. All replies are followed by three uint32:
   - the value of the event counter
   - the position of the stack
   - the current pc. */

enum debugger_reply {
  REP_EVENT = 'e',
  /* Event counter reached 0. */
  REP_BREAKPOINT = 'b',
  /* Breakpoint hit. */
  REP_EXITED = 'x',
  /* Program exited by calling exit or reaching the end of the source. */
  REP_TRAP = 's',
  /* Trap barrier crossed. */
  REP_UNCAUGHT_EXC = 'u'
  /* Program exited due to a stray exception. */
};

#endif /* CAML_DEBUGGER_H */
