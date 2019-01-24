/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Mark Shinwell and Leo White, Jane Street Europe             */
/*                                                                        */
/*   Copyright 2013--2016, Jane Street Group, LLC                         */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/alloc.h"
#include "caml/backtrace_prim.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/sys.h"
#include "caml/spacetime.h"

#ifdef WITH_SPACETIME

/* We force "noinline" in certain places to be sure we know how many
   frames there will be on the stack. */
#define NOINLINE __attribute__((noinline))

#ifdef HAS_LIBUNWIND
#define UNW_LOCAL_ONLY
#include "libunwind.h"
#endif

static int automatic_snapshots = 0;
static double snapshot_interval = 0.0;
static double next_snapshot_time = 0.0;
static struct channel *snapshot_channel;
static int pid_when_snapshot_channel_opened;

extern value caml_spacetime_debug(value);

static char* start_of_free_node_block;
static char* end_of_free_node_block;

typedef struct per_thread {
  value* trie_node_root;
  value* finaliser_trie_node_root;
  struct per_thread* next;
} per_thread;

/* List of tries corresponding to threads that have been created. */
/* CR-soon mshinwell: just include the main trie in this list. */
static per_thread* per_threads = NULL;
static int num_per_threads = 0;

/* [caml_spacetime_shapes] is defined in the startup file. */
extern uint64_t* caml_spacetime_shapes;

uint64_t** caml_spacetime_static_shape_tables = NULL;
shape_table* caml_spacetime_dynamic_shape_tables = NULL;

static uintnat caml_spacetime_profinfo = (uintnat) 0;

value caml_spacetime_trie_root = Val_unit;
value* caml_spacetime_trie_node_ptr = &caml_spacetime_trie_root;

static value caml_spacetime_finaliser_trie_root_main_thread = Val_unit;
value* caml_spacetime_finaliser_trie_root
  = &caml_spacetime_finaliser_trie_root_main_thread;

/* CR-someday mshinwell: think about thread safety of the manipulation of
   this list for multicore */
allocation_point* caml_all_allocation_points = NULL;

static const uintnat chunk_size = 1024 * 1024;

static void reinitialise_free_node_block(void)
{
  size_t index;

  start_of_free_node_block = (char*) caml_stat_alloc_noexc(chunk_size);
  end_of_free_node_block = start_of_free_node_block + chunk_size;

  for (index = 0; index < chunk_size / sizeof(value); index++) {
    ((value*) start_of_free_node_block)[index] = Val_unit;
  }
}

#ifndef O_BINARY
#define O_BINARY 0
#endif

#if defined (_WIN32) || defined (_WIN64)
extern value val_process_id;
#endif

enum {
  FEATURE_CALL_COUNTS = 1,
} features;

static uint16_t version_number = 0;
static uint32_t magic_number_base = 0xace00ace;

static void caml_spacetime_write_magic_number_internal(struct channel* chan)
{
  value magic_number;
  uint16_t features = 0;

#ifdef ENABLE_CALL_COUNTS
  features |= FEATURE_CALL_COUNTS;
#endif

  magic_number =
    Val_long(((uint64_t) magic_number_base)
             | (((uint64_t) version_number) << 32)
             | (((uint64_t) features) << 48));

  Lock(chan);
  caml_output_val(chan, magic_number, Val_long(0));
  Unlock(chan);
}

CAMLprim value caml_spacetime_write_magic_number(value v_channel)
{
  caml_spacetime_write_magic_number_internal(Channel(v_channel));
  return Val_unit;
}

static char* automatic_snapshot_dir;

static void open_snapshot_channel(void)
{
  int fd;
  char filename[8192];
  int pid;
#if defined (_WIN32) || defined (_WIN64)
  pid = Int_val(val_process_id);
#else
  pid = getpid();
#endif
  snprintf(filename, 8192, "%s/spacetime-%d", automatic_snapshot_dir, pid);
  filename[8191] = '\0';
  fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0666);
  if (fd == -1) {
    automatic_snapshots = 0;
  }
  else {
    snapshot_channel = caml_open_descriptor_out(fd);
    snapshot_channel->flags |= CHANNEL_FLAG_BLOCKING_WRITE;
    pid_when_snapshot_channel_opened = pid;
    caml_spacetime_write_magic_number_internal(snapshot_channel);
  }
}

static void maybe_reopen_snapshot_channel(void)
{
  /* This function should be used before writing to the automatic snapshot
     channel.  It detects whether we have forked since the channel was opened.
     If so, we close the old channel (ignoring any errors just in case the
     old fd has been closed, e.g. in a double-fork situation where the middle
     process has a loop to manually close all fds and no Spacetime snapshot
     was written during that time) and then open a new one. */

  int pid;
#if defined (_WIN32) || defined (_WIN64)
  pid = Int_val(val_process_id);
#else
  pid = getpid();
#endif

  if (pid != pid_when_snapshot_channel_opened) {
    caml_close_channel(snapshot_channel);
    open_snapshot_channel();
  }
}

extern void caml_spacetime_automatic_save(void);

void caml_spacetime_initialize(void)
{
  /* Note that this is called very early (even prior to GC initialisation). */

  char *ap_interval;

  reinitialise_free_node_block();

  caml_spacetime_static_shape_tables = &caml_spacetime_shapes;

  ap_interval = caml_secure_getenv ("OCAML_SPACETIME_INTERVAL");
  if (ap_interval != NULL) {
    unsigned int interval = 0;
    sscanf(ap_interval, "%u", &interval);
    if (interval != 0) {
      double time;
      char cwd[4096];
      char* user_specified_automatic_snapshot_dir;
      int dir_ok = 1;

      user_specified_automatic_snapshot_dir =
        caml_secure_getenv("OCAML_SPACETIME_SNAPSHOT_DIR");

      if (user_specified_automatic_snapshot_dir == NULL) {
#if defined(HAS_GETCWD)
        if (getcwd(cwd, sizeof(cwd)) == NULL) {
          dir_ok = 0;
        }
#else
        dir_ok = 0;
#endif
        if (dir_ok) {
          automatic_snapshot_dir = strdup(cwd);
        }
      }
      else {
        automatic_snapshot_dir =
          strdup(user_specified_automatic_snapshot_dir);
      }

      if (dir_ok) {
        automatic_snapshots = 1;
        open_snapshot_channel();
        if (automatic_snapshots) {
#ifdef SIGINT
          /* Catch interrupt so that the profile can be completed.
             We do this by marking the signal as handled without
             specifying an actual handler. This causes the signal
             to be handled by a call to exit. */
          caml_set_signal_action(SIGINT, 2);
#endif
          snapshot_interval = interval / 1e3;
          time = caml_sys_time_unboxed(Val_unit);
          next_snapshot_time = time + snapshot_interval;
          atexit(&caml_spacetime_automatic_save);
        }
      }
    }
  }
}

void caml_spacetime_register_shapes(void* dynlinked_table)
{
  shape_table* table;
  table = (shape_table*) caml_stat_alloc_noexc(sizeof(shape_table));
  if (table == NULL) {
    fprintf(stderr, "Out of memory whilst registering shape table");
    abort();
  }
  table->table = (uint64_t*) dynlinked_table;
  table->next = caml_spacetime_dynamic_shape_tables;
  caml_spacetime_dynamic_shape_tables = table;
}

CAMLprim value caml_spacetime_trie_is_initialized (value v_unit)
{
  return (caml_spacetime_trie_root == Val_unit) ? Val_false : Val_true;
}

CAMLprim value caml_spacetime_get_trie_root (value v_unit)
{
  return caml_spacetime_trie_root;
}

void caml_spacetime_register_thread(
  value* trie_node_root, value* finaliser_trie_node_root)
{
  per_thread* thr;

  thr = (per_thread*) caml_stat_alloc_noexc(sizeof(per_thread));
  if (thr == NULL) {
    fprintf(stderr, "Out of memory while registering thread for profiling\n");
    abort();
  }
  thr->next = per_threads;
  per_threads = thr;

  thr->trie_node_root = trie_node_root;
  thr->finaliser_trie_node_root = finaliser_trie_node_root;

  /* CR-soon mshinwell: record thread ID (and for the main thread too) */

  num_per_threads++;
}

static void caml_spacetime_save_event_internal (value v_time_opt,
                                                struct channel* chan,
                                                value v_event_name)
{
  value v_time;
  double time_override = 0.0;
  int use_time_override = 0;

  if (Is_block(v_time_opt)) {
    time_override = Double_field(Field(v_time_opt, 0), 0);
    use_time_override = 1;
  }
  v_time = caml_spacetime_timestamp(time_override, use_time_override);

  Lock(chan);
  caml_output_val(chan, Val_long(2), Val_long(0));
  caml_output_val(chan, v_event_name, Val_long(0));
  caml_extern_allow_out_of_heap = 1;
  caml_output_val(chan, v_time, Val_long(0));
  caml_extern_allow_out_of_heap = 0;
  Unlock(chan);

  caml_stat_free(Hp_val(v_time));
}

CAMLprim value caml_spacetime_save_event (value v_time_opt,
                                          value v_channel,
                                          value v_event_name)
{
  struct channel* chan = Channel(v_channel);

  caml_spacetime_save_event_internal(v_time_opt, chan, v_event_name);

  return Val_unit;
}


void save_trie (struct channel *chan, double time_override,
                int use_time_override)
{
  value v_time, v_frames, v_shapes;
  /* CR-someday mshinwell: The commented-out changes here are for multicore,
     where we think we should have one trie per domain. */
  /* int num_marshalled = 0;
  per_thread* thr = per_threads; */

  Lock(chan);

  caml_output_val(chan, Val_long(1), Val_long(0));

  v_time = caml_spacetime_timestamp(time_override, use_time_override);
  v_frames = caml_spacetime_frame_table();
  v_shapes = caml_spacetime_shape_table();

  caml_extern_allow_out_of_heap = 1;
  caml_output_val(chan, v_time, Val_long(0));
  caml_output_val(chan, v_frames, Val_long(0));
  caml_output_val(chan, v_shapes, Val_long(0));
  caml_extern_allow_out_of_heap = 0;

  caml_output_val(chan, Val_long(1) /* Val_long(num_per_threads + 1) */,
    Val_long(0));

  /* Marshal both the main and finaliser tries, for all threads that have
     been created, to an [out_channel].  This can be done by using the
     extern.c code as usual, since the trie looks like standard OCaml values;
     but we must allow it to traverse outside the heap. */

  caml_extern_allow_out_of_heap = 1;
  caml_output_val(chan, caml_spacetime_trie_root, Val_long(0));
  caml_output_val(chan,
    caml_spacetime_finaliser_trie_root_main_thread, Val_long(0));
  /* while (thr != NULL) {
    caml_output_val(chan, *(thr->trie_node_root), Val_long(0));
    caml_output_val(chan, *(thr->finaliser_trie_node_root),
      Val_long(0));
    thr = thr->next;
    num_marshalled++;
  }
  CAMLassert(num_marshalled == num_per_threads); */
  caml_extern_allow_out_of_heap = 0;

  Unlock(chan);
}

CAMLprim value caml_spacetime_save_trie (value v_time_opt, value v_channel)
{
  struct channel* channel = Channel(v_channel);
  double time_override = 0.0;
  int use_time_override = 0;

  if (Is_block(v_time_opt)) {
    time_override = Double_field(Field(v_time_opt, 0), 0);
    use_time_override = 1;
  }

  save_trie(channel, time_override, use_time_override);

  return Val_unit;
}

c_node_type caml_spacetime_classify_c_node(c_node* node)
{
  return (node->pc & 2) ? CALL : ALLOCATION;
}

c_node* caml_spacetime_c_node_of_stored_pointer(value node_stored)
{
  CAMLassert(node_stored == Val_unit || Is_c_node(node_stored));
  return (node_stored == Val_unit) ? NULL : (c_node*) Hp_val(node_stored);
}

c_node* caml_spacetime_c_node_of_stored_pointer_not_null(
      value node_stored)
{
  CAMLassert(Is_c_node(node_stored));
  return (c_node*) Hp_val(node_stored);
}

value caml_spacetime_stored_pointer_of_c_node(c_node* c_node)
{
  value node;
  CAMLassert(c_node != NULL);
  node = Val_hp(c_node);
  CAMLassert(Is_c_node(node));
  return node;
}

#ifdef HAS_LIBUNWIND
static int pc_inside_c_node_matches(c_node* node, void* pc)
{
  return Decode_c_node_pc(node->pc) == pc;
}
#endif

static value allocate_uninitialized_ocaml_node(int size_including_header)
{
  void* node;
  uintnat size;

  CAMLassert(size_including_header >= 3);
  node = caml_stat_alloc(sizeof(uintnat) * size_including_header);

  size = size_including_header * sizeof(value);

  node = (void*) start_of_free_node_block;
  if (end_of_free_node_block - start_of_free_node_block < size) {
    reinitialise_free_node_block();
    node = (void*) start_of_free_node_block;
    CAMLassert(end_of_free_node_block - start_of_free_node_block >= size);
  }

  start_of_free_node_block += size;

  /* We don't currently rely on [uintnat] alignment, but we do need some
     alignment, so just be sure. */
  CAMLassert (((uintnat) node) % sizeof(uintnat) == 0);
  return Val_hp(node);
}

static value find_tail_node(value node, void* callee)
{
  /* Search the tail chain within [node] (which corresponds to an invocation
     of a caller of [callee]) to determine whether it contains a tail node
     corresponding to [callee].  Returns any such node, or [Val_unit] if no
     such node exists. */

  value starting_node;
  value pc;
  value found = Val_unit;

  starting_node = node;
  pc = Encode_node_pc(callee);

  do {
    CAMLassert(Is_ocaml_node(node));
    if (Node_pc(node) == pc) {
      found = node;
    }
    else {
      node = Tail_link(node);
    }
  } while (found == Val_unit && starting_node != node);

  return found;
}

CAMLprim value caml_spacetime_allocate_node(
      int size_including_header, void* pc, value* node_hole)
{
  value node;
  value caller_node = Val_unit;

  node = *node_hole;
  /* The node hole should either contain [Val_unit], indicating that this
     function was not tail called and we have not been to this point in the
     trie before; or it should contain a value encoded using
     [Encoded_tail_caller_node] that points at the node of a caller
     that tail called the current function.  (Such a value is necessary to
     be able to find the start of the caller's node, and hence its tail
     chain, so we as a tail-called callee can link ourselves in.) */
  CAMLassert(Is_tail_caller_node_encoded(node));

  if (node != Val_unit) {
    value tail_node;
    /* The callee was tail called.  Find whether there already exists a node
       for it in the tail call chain within the caller's node.  The caller's
       node must always be an OCaml node. */
    caller_node = Decode_tail_caller_node(node);
    tail_node = find_tail_node(caller_node, pc);
    if (tail_node != Val_unit) {
      /* This tail calling sequence has happened before; just fill the hole
         with the existing node and return. */
      *node_hole = tail_node;
      return 0;  /* indicates an existing node was returned */
    }
  }

  node = allocate_uninitialized_ocaml_node(size_including_header);
  Hd_val(node) =
    Make_header(size_including_header - 1, OCaml_node_tag, Caml_black);
  CAMLassert((((uintnat) pc) % 1) == 0);
  Node_pc(node) = Encode_node_pc(pc);
  /* If the callee was tail called, then the tail link field will link this
     new node into an existing tail chain.  Otherwise, it is initialized with
     the empty tail chain, i.e. the one pointing directly at [node]. */
  if (caller_node == Val_unit) {
    Tail_link(node) = node;
  }
  else {
    Tail_link(node) = Tail_link(caller_node);
    Tail_link(caller_node) = node;
  }

  /* The callee node pointers for direct tail call points are
     initialized from code emitted by the OCaml compiler.  This is done to
     avoid having to pass this function a description of which nodes are
     direct tail call points.  (We cannot just count them and put them at the
     beginning of the node because we need the indexes of elements within the
     node during instruction selection before we have found all call points.)

     All other fields have already been initialised by
     [reinitialise_free_node_block].
  */

  *node_hole = node;

  return 1;  /* indicates a new node was created */
}

static c_node* allocate_c_node(void)
{
  c_node* node;
  size_t index;

  node = (c_node*) start_of_free_node_block;
  if (end_of_free_node_block - start_of_free_node_block < sizeof(c_node)) {
    reinitialise_free_node_block();
    node = (c_node*) start_of_free_node_block;
    CAMLassert(end_of_free_node_block - start_of_free_node_block
      >= sizeof(c_node));
  }
  start_of_free_node_block += sizeof(c_node);

  CAMLassert((sizeof(c_node) % sizeof(uintnat)) == 0);

  /* CR-soon mshinwell: remove this and pad the structure properly */
  for (index = 0; index < sizeof(c_node) / sizeof(value); index++) {
    ((value*) node)[index] = Val_unit;
  }

  node->gc_header =
    Make_header(sizeof(c_node)/sizeof(uintnat) - 1, C_node_tag, Caml_black);
  node->data.call.callee_node = Val_unit;
  node->data.call.call_count = Val_long(0);
  node->next = Val_unit;

  return node;
}

/* Since a given indirect call site either always yields tail calls or
   always yields non-tail calls, the output of
   [caml_spacetime_indirect_node_hole_ptr] is uniquely determined by its
   first two arguments (the callee and the node hole).  We cache these
   to increase performance of recursive functions containing an indirect
   call (e.g. [List.map] when not inlined). */
static void* last_indirect_node_hole_ptr_callee;
static value* last_indirect_node_hole_ptr_node_hole;
static call_point* last_indirect_node_hole_ptr_result;

CAMLprim value* caml_spacetime_indirect_node_hole_ptr
      (void* callee, value* node_hole, value caller_node)
{
  /* Find the address of the node hole for an indirect call to [callee].
     If [caller_node] is not [Val_unit], it is a pointer to the caller's
     node, and indicates that this is a tail call site. */

  c_node* c_node;
  value encoded_callee;

  if (callee == last_indirect_node_hole_ptr_callee
      && node_hole == last_indirect_node_hole_ptr_node_hole) {
#ifdef ENABLE_CALL_COUNTS
    last_indirect_node_hole_ptr_result->call_count =
      Val_long (Long_val (last_indirect_node_hole_ptr_result->call_count) + 1);
#endif
    return &(last_indirect_node_hole_ptr_result->callee_node);
  }

  last_indirect_node_hole_ptr_callee = callee;
  last_indirect_node_hole_ptr_node_hole = node_hole;

  encoded_callee = Encode_c_node_pc_for_call(callee);

  while (*node_hole != Val_unit) {
    CAMLassert(((uintnat) *node_hole) % sizeof(value) == 0);

    c_node = caml_spacetime_c_node_of_stored_pointer_not_null(*node_hole);

    CAMLassert(c_node != NULL);
    CAMLassert(caml_spacetime_classify_c_node(c_node) == CALL);

    if (c_node->pc == encoded_callee) {
#ifdef ENABLE_CALL_COUNTS
      c_node->data.call.call_count =
        Val_long (Long_val(c_node->data.call.call_count) + 1);
#endif
      last_indirect_node_hole_ptr_result = &(c_node->data.call);
      return &(last_indirect_node_hole_ptr_result->callee_node);
    }
    else {
      node_hole = &c_node->next;
    }
  }

  c_node = allocate_c_node();
  c_node->pc = encoded_callee;

  if (caller_node != Val_unit) {
    /* This is a tail call site.
       Perform the initialization equivalent to that emitted by
       [Spacetime.code_for_function_prologue] for direct tail call
       sites. */
    c_node->data.call.callee_node = Encode_tail_caller_node(caller_node);
  }

  *node_hole = caml_spacetime_stored_pointer_of_c_node(c_node);

  CAMLassert(((uintnat) *node_hole) % sizeof(value) == 0);
  CAMLassert(*node_hole != Val_unit);

#ifdef ENABLE_CALL_COUNTS
  c_node->data.call.call_count =
    Val_long (Long_val(c_node->data.call.call_count) + 1);
#endif
  last_indirect_node_hole_ptr_result = &(c_node->data.call);

  return &(last_indirect_node_hole_ptr_result->callee_node);
}

/* Some notes on why caml_call_gc doesn't need a distinguished node.
   (Remember that thread switches are irrelevant here because each thread
   has its own trie.)

   caml_call_gc only invokes OCaml functions in the following circumstances:
   1. running an OCaml finaliser;
   2. executing an OCaml signal handler.
   Both of these are done on the finaliser trie.  Furthermore, both of
   these invocations start via caml_callback; the code in this file for
   handling that (caml_spacetime_c_to_ocaml) correctly copes with that by
   attaching a single "caml_start_program" node that can cope with any
   number of indirect OCaml calls from that point.

   caml_call_gc may also invoke C functions that cause allocation.  All of
   these (assuming libunwind support is present) will cause a chain of
   c_node structures to be attached to the trie, starting at the node hole
   passed to caml_call_gc from OCaml code.  These structures are extensible
   and can thus accommodate any number of C backtraces leading from
   caml_call_gc.
*/
/* CR-soon mshinwell: it might in fact be the case now that nothing called
   from caml_call_gc will do any allocation that ends up on the trie.  We
   can revisit this after the first release. */

static NOINLINE void* find_trie_node_from_libunwind(int for_allocation,
    uintnat wosize, struct ext_table** cached_frames)
{
#ifdef HAS_LIBUNWIND
  /* Given that [caml_last_return_address] is the most recent call site in
     OCaml code, and that we are now in C (or other) code called from that
     site, obtain a backtrace using libunwind and graft the most recent
     portion (everything back to but not including [caml_last_return_address])
     onto the trie.  See the important comment below regarding the fact that
     call site, and not callee, addresses are recorded during this process.

     If [for_allocation] is non-zero, the final node recorded will be for
     an allocation, and the returned pointer is to the allocation node.
     Otherwise, no node is recorded for the innermost frame, and the
     returned pointer is a pointer to the *node hole* where a node for that
     frame should be attached.

     If [for_allocation] is non-zero then [wosize] must give the size in
     words, excluding the header, of the value being allocated.

     If [cached_frames != NULL] then:
     1. If [*cached_frames] is NULL then save the captured backtrace in a
        newly-allocated table and store the pointer to that table in
        [*cached_frames];
     2. Otherwise use [*cached_frames] as the unwinding information.
     The intention is that when the context is known (e.g. a function such
     as [caml_make_vect] known to have been directly invoked from OCaml),
     we can avoid expensive calls to libunwind.
  */

  unw_cursor_t cur;
  unw_context_t ctx;
  int ret;
  int innermost_frame;
  int frame;
  static struct ext_table frames_local;
  struct ext_table* frames;
  static int ext_table_initialised = 0;
  int have_frames_already = 0;
  value* node_hole;
  c_node* node = NULL;
  int initial_table_size = 1000;
  int must_initialise_node_for_allocation = 0;

  if (!cached_frames) {
    if (!ext_table_initialised) {
      caml_ext_table_init(&frames_local, initial_table_size);
      ext_table_initialised = 1;
    }
    else {
      caml_ext_table_clear(&frames_local, 0);
    }
    frames = &frames_local;
  } else {
    if (*cached_frames) {
      frames = *cached_frames;
      have_frames_already = 1;
    }
    else {
      frames =
        (struct ext_table*) caml_stat_alloc_noexc(sizeof(struct ext_table));
      if (!frames) {
        caml_fatal_error("Not enough memory for ext_table allocation");
      }
      caml_ext_table_init(frames, initial_table_size);
      *cached_frames = frames;
    }
  }

  if (!have_frames_already) {
    /* Get the stack backtrace as far as [caml_last_return_address]. */

    ret = unw_getcontext(&ctx);
    if (ret != UNW_ESUCCESS) {
      return NULL;
    }

    ret = unw_init_local(&cur, &ctx);
    if (ret != UNW_ESUCCESS) {
      return NULL;
    }

    while ((ret = unw_step(&cur)) > 0) {
      unw_word_t ip;
      unw_get_reg(&cur, UNW_REG_IP, &ip);
      if (caml_last_return_address == (uintnat) ip) {
        break;
      }
      else {
        /* Inlined some of [caml_ext_table_add] for speed. */
        if (frames->size < frames->capacity) {
          frames->contents[frames->size++] = (void*) ip;
        } else {
          caml_ext_table_add(frames, (void*) ip);
        }
      }
    }
  }

  /* We always need to ignore the frames for:
      #0  find_trie_node_from_libunwind
      #1  caml_spacetime_c_to_ocaml
     Further, if this is not an allocation point, we should not create the
     node for the current C function that triggered us (i.e. frame #2). */
  innermost_frame = for_allocation ? 1 : 2;

  if (frames->size - 1 < innermost_frame) {
    /* Insufficiently many frames (maybe no frames) returned from
       libunwind; just don't do anything. */
    return NULL;
  }

  node_hole = caml_spacetime_trie_node_ptr;
  /* Note that if [node_hole] is filled, then it must point to a C node,
     since it is not possible for there to be a call point in an OCaml
     function that sometimes calls C and sometimes calls OCaml. */

  for (frame = frames->size - 1; frame >= innermost_frame; frame--) {
    c_node_type expected_type;
    void* pc = frames->contents[frame];
    CAMLassert (pc != (void*) caml_last_return_address);

    if (!for_allocation) {
      expected_type = CALL;
    }
    else {
      expected_type = (frame > innermost_frame ? CALL : ALLOCATION);
    }

    if (*node_hole == Val_unit) {
      node = allocate_c_node();
      /* Note: for CALL nodes, the PC is the program counter at each call
         site.  We do not store program counter addresses of the start of
         callees, unlike for OCaml nodes.  This means that some trie nodes
         will become conflated.  These can be split during post-processing by
         working out which function each call site was in. */
      node->pc = (expected_type == CALL ? Encode_c_node_pc_for_call(pc)
        : Encode_c_node_pc_for_alloc_point(pc));
      *node_hole = caml_spacetime_stored_pointer_of_c_node(node);
      if (expected_type == ALLOCATION) {
        must_initialise_node_for_allocation = 1;
      }
    }
    else {
      c_node* prev;
      int found = 0;

      node = caml_spacetime_c_node_of_stored_pointer_not_null(*node_hole);
      CAMLassert(node != NULL);
      CAMLassert(node->next == Val_unit
        || (((uintnat) (node->next)) % sizeof(value) == 0));

      prev = NULL;

      while (!found && node != NULL) {
        if (caml_spacetime_classify_c_node(node) == expected_type
            && pc_inside_c_node_matches(node, pc)) {
          found = 1;
        }
        else {
          prev = node;
          node = caml_spacetime_c_node_of_stored_pointer(node->next);
        }
      }
      if (!found) {
        CAMLassert(prev != NULL);
        node = allocate_c_node();
        node->pc = (expected_type == CALL ? Encode_c_node_pc_for_call(pc)
          : Encode_c_node_pc_for_alloc_point(pc));
        if (expected_type == ALLOCATION) {
          must_initialise_node_for_allocation = 1;
        }
        prev->next = caml_spacetime_stored_pointer_of_c_node(node);
      }
    }

    CAMLassert(node != NULL);

    CAMLassert(caml_spacetime_classify_c_node(node) == expected_type);
    CAMLassert(pc_inside_c_node_matches(node, pc));
    node_hole = &node->data.call.callee_node;
  }

  if (must_initialise_node_for_allocation) {
    caml_spacetime_profinfo++;
    if (caml_spacetime_profinfo > PROFINFO_MASK) {
      /* Profiling counter overflow. */
      caml_spacetime_profinfo = PROFINFO_MASK;
    }
    node->data.allocation.profinfo =
      Make_header_with_profinfo(
        /* "-1" because [c_node] has the GC header as its first
           element. */
        offsetof(c_node, data.allocation.count)/sizeof(value) - 1,
        Infix_tag,
        Caml_black,
        caml_spacetime_profinfo);
    node->data.allocation.count = Val_long(0);

    /* Add the new allocation point into the linked list of all allocation
       points. */
    if (caml_all_allocation_points != NULL) {
      node->data.allocation.next =
        (value) &caml_all_allocation_points->count;
    } else {
      node->data.allocation.next = Val_unit;
    }
    caml_all_allocation_points = &node->data.allocation;
  }

  if (for_allocation) {
    CAMLassert(caml_spacetime_classify_c_node(node) == ALLOCATION);
    CAMLassert(caml_spacetime_c_node_of_stored_pointer(node->next) != node);
    CAMLassert(Profinfo_hd(node->data.allocation.profinfo) > 0);
    node->data.allocation.count =
      Val_long(Long_val(node->data.allocation.count) + (1 + wosize));
  }

  CAMLassert(node->next != (value) NULL);

  return for_allocation ? (void*) node : (void*) node_hole;
#else
  return NULL;
#endif
}

void caml_spacetime_c_to_ocaml(void* ocaml_entry_point,
      void* identifying_pc_for_caml_start_program)
{
  /* Called in [caml_start_program] and [caml_callback*] when we are about
     to cross from C into OCaml.  [ocaml_entry_point] is the branch target.
     This situation is handled by ensuring the presence of a new OCaml node
     for the callback veneer; the node contains a single indirect call point
     which accumulates the [ocaml_entry_point]s.

     The layout of the node is described in the "system shape table"; see
     asmrun/amd64.S.
  */

  value node;

  /* Update the trie with the current backtrace, as far back as
     [caml_last_return_address], and leave the node hole pointer at
     the correct place for attachment of a [caml_start_program] node. */

#ifdef HAS_LIBUNWIND
  value* node_temp;
  node_temp = (value*) find_trie_node_from_libunwind(0, 0, NULL);
  if (node_temp != NULL) {
    caml_spacetime_trie_node_ptr = node_temp;
  }
#endif

  if (*caml_spacetime_trie_node_ptr == Val_unit) {
    uintnat size_including_header;

    size_including_header =
      1 /* GC header */ + Node_num_header_words + Indirect_num_fields;

    node = allocate_uninitialized_ocaml_node(size_including_header);
    Hd_val(node) =
      Make_header(size_including_header - 1, OCaml_node_tag, Caml_black);
    CAMLassert((((uintnat) identifying_pc_for_caml_start_program) % 1) == 0);
    Node_pc(node) = Encode_node_pc(identifying_pc_for_caml_start_program);
    Tail_link(node) = node;
    Indirect_pc_linked_list(node, Node_num_header_words) = Val_unit;
    *caml_spacetime_trie_node_ptr = node;
  }
  else {
    node = *caml_spacetime_trie_node_ptr;
    /* If there is a node here already, it should never be an initialized
       (but as yet unused) tail call point, since calls from OCaml into C
       are never tail calls (and no C -> C call is marked as tail). */
    CAMLassert(!Is_tail_caller_node_encoded(node));
  }

  CAMLassert(Is_ocaml_node(node));
  CAMLassert(Decode_node_pc(Node_pc(node))
    == identifying_pc_for_caml_start_program);
  CAMLassert(Tail_link(node) == node);
  CAMLassert(Wosize_val(node) == Node_num_header_words + Indirect_num_fields);

  /* Search the node to find the node hole corresponding to the indirect
     call to the OCaml function. */
  caml_spacetime_trie_node_ptr =
    caml_spacetime_indirect_node_hole_ptr(
      ocaml_entry_point,
      &Indirect_pc_linked_list(node, Node_num_header_words),
      Val_unit);
  CAMLassert(*caml_spacetime_trie_node_ptr == Val_unit
    || Is_ocaml_node(*caml_spacetime_trie_node_ptr));
}

extern void caml_garbage_collection(void);  /* signals_asm.c */
extern void caml_array_bound_error(void);  /* fail.c */

CAMLprim uintnat caml_spacetime_generate_profinfo (void* profinfo_words,
                                                   uintnat index_within_node)
{
  /* Called from code that creates a value's header inside an OCaml
     function. */

  value node;
  uintnat profinfo;

  caml_spacetime_profinfo++;
  if (caml_spacetime_profinfo > PROFINFO_MASK) {
    /* Profiling counter overflow. */
    caml_spacetime_profinfo = PROFINFO_MASK;
  }
  profinfo = caml_spacetime_profinfo;

  /* CR-someday mshinwell: we could always use the [struct allocation_point]
     overlay instead of the macros now. */

  /* [node] isn't really a node; it points into the middle of
     one---specifically to the "profinfo" word of an allocation point.
     It's done like this to avoid re-calculating the place in the node
     (which already has to be done in the OCaml-generated code run before
     this function). */
  node = (value) profinfo_words;
  CAMLassert(Alloc_point_profinfo(node, 0) == Val_unit);

  /* The profinfo value is stored shifted to reduce the number of
     instructions required on the OCaml side.  It also enables us to use
     [Infix_tag] to obtain valid value pointers into the middle of nodes,
     which is used for the linked list of all allocation points. */
  profinfo = Make_header_with_profinfo(
    index_within_node, Infix_tag, Caml_black, profinfo);

  CAMLassert(!Is_block(profinfo));
  Alloc_point_profinfo(node, 0) = profinfo;
  /* The count is set to zero by the initialisation when the node was
     created (see above). */
  CAMLassert(Alloc_point_count(node, 0) == Val_long(0));

  /* Add the new allocation point into the linked list of all allocation
     points. */
  if (caml_all_allocation_points != NULL) {
    Alloc_point_next_ptr(node, 0) = (value) &caml_all_allocation_points->count;
  }
  else {
    CAMLassert(Alloc_point_next_ptr(node, 0) == Val_unit);
  }
  caml_all_allocation_points = (allocation_point*) node;

  return profinfo;
}

uintnat caml_spacetime_my_profinfo (struct ext_table** cached_frames,
                                    uintnat wosize)
{
  /* Return the profinfo value that should be written into a value's header
     during an allocation from C.  This may necessitate extending the trie
     with information obtained from libunwind. */

  c_node* node;
  uintnat profinfo = 0;

  node = find_trie_node_from_libunwind(1, wosize, cached_frames);
  if (node != NULL) {
    profinfo = ((uintnat) (node->data.allocation.profinfo)) >> PROFINFO_SHIFT;
  }

  return profinfo;  /* N.B. not shifted by PROFINFO_SHIFT */
}

void caml_spacetime_automatic_snapshot (void)
{
  if (automatic_snapshots) {
    double start_time, end_time;
    start_time = caml_sys_time_unboxed(Val_unit);
    if (start_time >= next_snapshot_time) {
      maybe_reopen_snapshot_channel();
      caml_spacetime_save_snapshot(snapshot_channel, 0.0, 0);
      end_time = caml_sys_time_unboxed(Val_unit);
      next_snapshot_time = end_time + snapshot_interval;
    }
  }
}

CAMLprim value caml_spacetime_save_event_for_automatic_snapshots
  (value v_event_name)
{
  if (automatic_snapshots) {
    maybe_reopen_snapshot_channel();
    caml_spacetime_save_event_internal (Val_unit, snapshot_channel,
                                        v_event_name);
  }
  return Val_unit;
}

void caml_spacetime_automatic_save (void)
{
  /* Called from [atexit]. */

  if (automatic_snapshots) {
    automatic_snapshots = 0;
    maybe_reopen_snapshot_channel();
    save_trie(snapshot_channel, 0.0, 0);
    caml_flush(snapshot_channel);
    caml_close_channel(snapshot_channel);
  }
}

CAMLprim value caml_spacetime_enabled (value v_unit)
{
  return Val_true;
}

CAMLprim value caml_register_channel_for_spacetime (value v_channel)
{
  struct channel* channel = Channel(v_channel);
  channel->flags |= CHANNEL_FLAG_BLOCKING_WRITE;
  return Val_unit;
}

#else

/* Functions for when the compiler was not configured with "-spacetime". */

CAMLprim value caml_spacetime_write_magic_number(value v_channel)
{
  return Val_unit;
}

CAMLprim value caml_spacetime_enabled (value v_unit)
{
  return Val_false;
}

CAMLprim value caml_spacetime_save_event (value v_time_opt,
                                          value v_channel,
                                          value v_event_name)
{
  return Val_unit;
}

CAMLprim value caml_spacetime_save_event_for_automatic_snapshots
  (value v_event_name)
{
  return Val_unit;
}

CAMLprim value caml_spacetime_save_trie (value ignored)
{
  return Val_unit;
}

CAMLprim value caml_register_channel_for_spacetime (value v_channel)
{
  return Val_unit;
}

#endif
