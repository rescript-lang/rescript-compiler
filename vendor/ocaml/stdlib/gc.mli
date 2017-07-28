(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Memory management control and statistics; finalised values. *)

type stat =
  { minor_words : float;
    (** Number of words allocated in the minor heap since
       the program was started.  This number is accurate in
       byte-code programs, but only an approximation in programs
       compiled to native code. *)

    promoted_words : float;
    (** Number of words allocated in the minor heap that
       survived a minor collection and were moved to the major heap
       since the program was started. *)

    major_words : float;
    (** Number of words allocated in the major heap, including
       the promoted words, since the program was started. *)

    minor_collections : int;
    (** Number of minor collections since the program was started. *)

    major_collections : int;
    (** Number of major collection cycles completed since the program
        was started. *)

    heap_words : int;
    (** Total size of the major heap, in words. *)

    heap_chunks : int;
    (** Number of contiguous pieces of memory that make up the major heap. *)

    live_words : int;
    (** Number of words of live data in the major heap, including the header
       words. *)

    live_blocks : int;
    (** Number of live blocks in the major heap. *)

    free_words : int;
    (** Number of words in the free list. *)

    free_blocks : int;
    (** Number of blocks in the free list. *)

    largest_free : int;
    (** Size (in words) of the largest block in the free list. *)

    fragments : int;
    (** Number of wasted words due to fragmentation.  These are
       1-words free blocks placed between two live blocks.  They
       are not available for allocation. *)

    compactions : int;
    (** Number of heap compactions since the program was started. *)

    top_heap_words : int;
    (** Maximum size reached by the major heap, in words. *)

    stack_size: int;
    (** Current size of the stack, in words. @since 3.12.0 *)
}
(** The memory management counters are returned in a [stat] record.

   The total amount of memory allocated by the program since it was started
   is (in words) [minor_words + major_words - promoted_words].  Multiply by
   the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get
   the number of bytes.
*)

type control =
  { mutable minor_heap_size : int;
    (** The size (in words) of the minor heap.  Changing
       this parameter will trigger a minor collection.  Default: 256k. *)

    mutable major_heap_increment : int;
    (** How much to add to the major heap when increasing it. If this
        number is less than or equal to 1000, it is a percentage of
        the current heap size (i.e. setting it to 100 will double the heap
        size at each increase). If it is more than 1000, it is a fixed
        number of words that will be added to the heap. Default: 15. *)

    mutable space_overhead : int;
    (** The major GC speed is computed from this parameter.
       This is the memory that will be "wasted" because the GC does not
       immediatly collect unreachable blocks.  It is expressed as a
       percentage of the memory used for live data.
       The GC will work more (use more CPU time and collect
       blocks more eagerly) if [space_overhead] is smaller.
       Default: 80. *)

    mutable verbose : int;
    (** This value controls the GC messages on standard error output.
       It is a sum of some of the following flags, to print messages
       on the corresponding events:
       - [0x001] Start of major GC cycle.
       - [0x002] Minor collection and major GC slice.
       - [0x004] Growing and shrinking of the heap.
       - [0x008] Resizing of stacks and memory manager tables.
       - [0x010] Heap compaction.
       - [0x020] Change of GC parameters.
       - [0x040] Computation of major GC slice size.
       - [0x080] Calling of finalisation functions.
       - [0x100] Bytecode executable and shared library search at start-up.
       - [0x200] Computation of compaction-triggering condition.
       Default: 0. *)

    mutable max_overhead : int;
    (** Heap compaction is triggered when the estimated amount
       of "wasted" memory is more than [max_overhead] percent of the
       amount of live data.  If [max_overhead] is set to 0, heap
       compaction is triggered at the end of each major GC cycle
       (this setting is intended for testing purposes only).
       If [max_overhead >= 1000000], compaction is never triggered.
       If compaction is permanently disabled, it is strongly suggested
       to set [allocation_policy] to 1.
       Default: 500. *)

    mutable stack_limit : int;
    (** The maximum size of the stack (in words).  This is only
       relevant to the byte-code runtime, as the native code runtime
       uses the operating system's stack.  Default: 1024k. *)

    mutable allocation_policy : int;
    (** The policy used for allocating in the heap.  Possible
        values are 0 and 1.  0 is the next-fit policy, which is
        quite fast but can result in fragmentation.  1 is the
        first-fit policy, which can be slower in some cases but
        can be better for programs with fragmentation problems.
        Default: 0. @since 3.11.0 *)
}
(** The GC parameters are given as a [control] record.  Note that
    these parameters can also be initialised by setting the
    OCAMLRUNPARAM environment variable.  See the documentation of
    [ocamlrun]. *)

external stat : unit -> stat = "caml_gc_stat"
(** Return the current values of the memory management counters in a
   [stat] record.  This function examines every heap block to get the
   statistics. *)

external quick_stat : unit -> stat = "caml_gc_quick_stat"
(** Same as [stat] except that [live_words], [live_blocks], [free_words],
    [free_blocks], [largest_free], and [fragments] are set to 0.  This
    function is much faster than [stat] because it does not need to go
    through the heap. *)

external counters : unit -> float * float * float = "caml_gc_counters"
(** Return [(minor_words, promoted_words, major_words)].  This function
    is as fast as [quick_stat]. *)

external get : unit -> control = "caml_gc_get"
(** Return the current values of the GC parameters in a [control] record. *)

external set : control -> unit = "caml_gc_set"
(** [set r] changes the GC parameters according to the [control] record [r].
   The normal usage is: [Gc.set { (Gc.get()) with Gc.verbose = 0x00d }] *)

external minor : unit -> unit = "caml_gc_minor"
(** Trigger a minor collection. *)

external major_slice : int -> int = "caml_gc_major_slice";;
(** Do a minor collection and a slice of major collection.  The argument
    is the size of the slice, 0 to use the automatically-computed
    slice size.  In all cases, the result is the computed slice size. *)

external major : unit -> unit = "caml_gc_major"
(** Do a minor collection and finish the current major collection cycle. *)

external full_major : unit -> unit = "caml_gc_full_major"
(** Do a minor collection, finish the current major collection cycle,
   and perform a complete new cycle.  This will collect all currently
   unreachable blocks. *)

external compact : unit -> unit = "caml_gc_compaction"
(** Perform a full major collection and compact the heap.  Note that heap
   compaction is a lengthy operation. *)

val print_stat : out_channel -> unit
(** Print the current values of the memory management counters (in
   human-readable form) into the channel argument. *)

val allocated_bytes : unit -> float
(** Return the total number of bytes allocated since the program was
   started.  It is returned as a [float] to avoid overflow problems
   with [int] on 32-bit machines. *)

val finalise : ('a -> unit) -> 'a -> unit
(** [finalise f v] registers [f] as a finalisation function for [v].
   [v] must be heap-allocated.  [f] will be called with [v] as
   argument at some point between the first time [v] becomes unreachable
   and the time [v] is collected by the GC.  Several functions can
   be registered for the same value, or even several instances of the
   same function.  Each instance will be called once (or never,
   if the program terminates before [v] becomes unreachable).

   The GC will call the finalisation functions in the order of
   deallocation.  When several values become unreachable at the
   same time (i.e. during the same GC cycle), the finalisation
   functions will be called in the reverse order of the corresponding
   calls to [finalise].  If [finalise] is called in the same order
   as the values are allocated, that means each value is finalised
   before the values it depends upon.  Of course, this becomes
   false if additional dependencies are introduced by assignments.

   In the presence of multiple OCaml threads it should be assumed that
   any particular finaliser may be executed in any of the threads.

   Anything reachable from the closure of finalisation functions
   is considered reachable, so the following code will not work
   as expected:
   - [ let v = ... in Gc.finalise (fun x -> ... v ...) v ]

   Instead you should make sure that [v] is not in the closure of
   the finalisation function by writing:
   - [ let f = fun x -> ... ;; let v = ... in Gc.finalise f v ]


   The [f] function can use all features of OCaml, including
   assignments that make the value reachable again.  It can also
   loop forever (in this case, the other
   finalisation functions will not be called during the execution of f,
   unless it calls [finalise_release]).
   It can call [finalise] on [v] or other values to register other
   functions or even itself.  It can raise an exception; in this case
   the exception will interrupt whatever the program was doing when
   the function was called.


   [finalise] will raise [Invalid_argument] if [v] is not
   heap-allocated.  Some examples of values that are not
   heap-allocated are integers, constant constructors, booleans,
   the empty array, the empty list, the unit value.  The exact list
   of what is heap-allocated or not is implementation-dependent.
   Some constant values can be heap-allocated but never deallocated
   during the lifetime of the program, for example a list of integer
   constants; this is also implementation-dependent.
   You should also be aware that compiler optimisations may duplicate
   some immutable values, for example floating-point numbers when
   stored into arrays, so they can be finalised and collected while
   another copy is still in use by the program.


   The results of calling {!String.make}, {!Bytes.make}, {!Bytes.create},
   {!Array.make}, and {!Pervasives.ref} are guaranteed to be
   heap-allocated and non-constant except when the length argument is [0].
*)

val finalise_release : unit -> unit;;
(** A finalisation function may call [finalise_release] to tell the
    GC that it can launch the next finalisation function without waiting
    for the current one to return. *)

type alarm
(** An alarm is a piece of data that calls a user function at the end of
   each major GC cycle.  The following functions are provided to create
   and delete alarms. *)

val create_alarm : (unit -> unit) -> alarm
(** [create_alarm f] will arrange for [f] to be called at the end of each
   major GC cycle, starting with the current cycle or the next one.
   A value of type [alarm] is returned that you can
   use to call [delete_alarm]. *)

val delete_alarm : alarm -> unit
(** [delete_alarm a] will stop the calls to the function associated
   to [a].  Calling [delete_alarm a] again has no effect. *)
