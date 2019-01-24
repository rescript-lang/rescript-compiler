(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Access to the information recorded by the [Spacetime]
    module.  (It is intended that this module will be used by
    post-processors rather than users wishing to understand their
    programs.)
    For 64-bit targets only.
    This module may be used from any program, not just one compiled
    with a compiler configured for Spacetime. *)

module Gc_stats : sig
  type t

  val minor_words : t -> int
  val promoted_words : t -> int
  val major_words : t -> int
  val minor_collections : t -> int
  val major_collections : t -> int
  val heap_words : t -> int
  val heap_chunks : t -> int
  val compactions : t -> int
  val top_heap_words : t -> int
end

module Annotation : sig
  (** An annotation written into a value's header.  These may be looked up
      in a [Trace.t] (see below). *)
  type t

  (* CR-someday mshinwell: consider using tag and size to increase the
     available space of annotations.  Need to be careful of [Obj.truncate].
     Could also randomise the tags on records.
  *)

  val to_int : t -> int
end

module Program_counter : sig
  module OCaml : sig
    type t

    val to_int64 : t -> Int64.t
  end

  module Foreign : sig
    type t

    val to_int64 : t -> Int64.t
  end

end

module Frame_table : sig
  (* CR-someday mshinwell: move to [Gc] if dependencies permit? *)
  (** A value of type [t] corresponds to the frame table of a running
      OCaml program.  The table is indexed by program counter address
      (typically, but not always when using Spacetime, return addresses). *)
  type t

  (** Find the location, including any inlined frames, corresponding to the
      given program counter address.  Raises [Not_found] if the location
      could not be resolved. *)
  val find_exn : Program_counter.OCaml.t -> t -> Printexc.Slot.t list
end

module Function_entry_point : sig
  type t

  val to_int64 : t -> Int64.t
end

module Function_identifier : sig
  type t
  (* CR-soon mshinwell: same as [Function_entry_point] now *)
  val to_int64 : t -> Int64.t
end

module Shape_table : sig
  type t
end

module Trace : sig
  (** A value of type [t] holds the dynamic call structure of the program
      (i.e. which functions have called which other functions) together with
      information required to decode profiling annotations written into
      values' headers. *)
  type t
  type trace = t

  type node
  type ocaml_node
  type foreign_node
  type uninstrumented_node

  module OCaml : sig
    module Allocation_point : sig
      (** A value of type [t] corresponds to an allocation point in OCaml
          code. *)
      type t

      (** The program counter at (or close to) the allocation site. *)
      val program_counter : t -> Program_counter.OCaml.t

      (** The annotation written into the headers of boxed values allocated
          at the given allocation site. *)
      val annotation : t -> Annotation.t

      (** The total number of words allocated at this point. *)
      val num_words_including_headers : t -> int
    end

    module Direct_call_point : sig
      (** A value of type ['target t] corresponds to a direct (i.e. known
          at compile time) call point in OCaml code.  ['target] is the type
          of the node corresponding to the callee. *)
      type 'target t

      (** The program counter at (or close to) the call site. *)
      val call_site : _ t -> Program_counter.OCaml.t

      (** The address of the first instruction of the callee. *)
      val callee : _ t -> Function_entry_point.t

      (** The node corresponding to the callee. *)
      val callee_node : 'target t -> 'target

      (** The number of times the callee was called.  Only available if the
          compiler that recorded the Spacetime profile was configured with
          "-with-spacetime-call-counts".  [None] will be returned otherwise. *)
      val call_count : _ t -> int option
    end

    module Indirect_call_point : sig
      (** A value of type [t] corresponds to an indirect call point in OCaml
          code.  Each such value contains a list of callees to which the
          call point has branched. *)
      type t

      (** The program counter at (or close to) the call site. *)
      val call_site : t -> Program_counter.OCaml.t

      module Callee : sig
        type t

        (** The address of the first instruction of the callee. *)
        val callee : t -> Function_entry_point.t

        (** The node corresponding to the callee. *)
        val callee_node : t -> node

        (** The number of times the callee was called.  This returns [None] in
            the same circumstances as [Direct_call_point.call_count], above. *)
        val call_count : t -> int option

        (** Move to the next callee to which this call point has branched.
            [None] is returned when the end of the list is reached. *)
        val next : t -> t option
      end

      (** The list of callees to which this indirect call point has
          branched. *)
      val callees : t -> Callee.t option
    end

    module Field : sig
      (** A value of type [t] enables iteration through the contents
          ("fields") of an OCaml node. *)
      type t

      type direct_call_point =
        | To_ocaml of ocaml_node Direct_call_point.t
        | To_foreign of foreign_node Direct_call_point.t
        (* CR-soon mshinwell: once everything's finished, "uninstrumented"
           should be able to go away.  Let's try to do this after the
           first release. *)
        | To_uninstrumented of
            uninstrumented_node Direct_call_point.t

      type classification =
        | Allocation of Allocation_point.t
        | Direct_call of direct_call_point
        | Indirect_call of Indirect_call_point.t

      val classify : t -> classification
      val next : t -> t option
    end

    module Node : sig
      (** A node corresponding to an invocation of a function written in
          OCaml. *)
      type t = ocaml_node

      val compare : t -> t -> int

      (** A unique identifier for the function corresponding to this node. *)
      val function_identifier : t -> Function_identifier.t

      (** This function traverses a circular list. *)
      val next_in_tail_call_chain : t -> t

      val fields : t -> shape_table:Shape_table.t -> Field.t option
    end
  end

  module Foreign : sig
    module Allocation_point : sig
      (** A value of type [t] corresponds to an allocation point in non-OCaml
          code. *)
      type t

      val program_counter : t -> Program_counter.Foreign.t
      val annotation : t -> Annotation.t
      val num_words_including_headers : t -> int
    end

    module Call_point : sig
      (** A value of type [t] corresponds to a call point from non-OCaml
          code (to either non-OCaml code, or OCaml code via the usual
          assembly veneer).  Call counts are not available for such nodes. *)
      type t

      (** N.B. The address of the callee (of type [Function_entry_point.t]) is
          not available.  It must be recovered during post-processing. *)
      val call_site : t -> Program_counter.Foreign.t
      val callee_node : t -> node
    end

    module Field : sig
      (** A value of type [t] enables iteration through the contents ("fields")
          of a C node. *)
      type t

      type classification = private
        | Allocation of Allocation_point.t
        | Call of Call_point.t

      val classify : t -> classification
      val next : t -> t option
    end

    module Node : sig
      (** A node corresponding to an invocation of a function written in C
          (or any other language that is not OCaml). *)
      type t = foreign_node

      val compare : t -> t -> int

      val fields : t -> Field.t option

    end

  end

  module Node : sig
    (** Either an OCaml or a foreign node; or an indication that this
        is a branch of the graph corresponding to uninstrumented
        code. *)
    type t = node

    val compare : t -> t -> int

    type classification = private
      | OCaml of OCaml.Node.t
      | Foreign of Foreign.Node.t

    val classify : t -> classification

    val of_ocaml_node : OCaml.Node.t -> t
    val of_foreign_node : Foreign.Node.t -> t

    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t
  end

  (** Obtains the root of the graph for traversal.  [None] is returned if
      the graph is empty. *)
  val root : t -> Node.t option
end

module Heap_snapshot : sig
  type t
  type heap_snapshot = t

  module Entries : sig
    (** An immutable array of the total number of blocks (= boxed
        values) and the total number of words occupied by such blocks
        (including their headers) for each profiling annotation in
        the heap. *)
    type t

    val length : t -> int
    val annotation : t -> int -> Annotation.t
    val num_blocks : t -> int -> int
    val num_words_including_headers : t -> int -> int

  end

  (** The timestamp of a snapshot.  The units are as for [Sys.time]
      (unless custom timestamps are being provided, cf. the [Spacetime] module
      in the standard library). *)
  val timestamp : t -> float

  val gc_stats : t -> Gc_stats.t
  val entries : t -> Entries.t
  val words_scanned : t -> int
  val words_scanned_with_profinfo : t -> int

  module Total_allocation : sig
    type t

    val annotation : t -> Annotation.t
    val num_words_including_headers : t -> int
    val next : t -> t option
  end
  (** Total allocations across *all threads*. *)
  (* CR-someday mshinwell: change the relevant variables to be thread-local *)
  val total_allocations : t -> Total_allocation.t option

  module Event : sig
    type t

    val event_name : t -> string
    val timestamp : t -> float
  end

  module Series : sig
    type t

    (** At present, the [Trace.t] associated with a [Series.t] cannot be
        garbage collected or freed.  This should not be a problem, since
        the intention is that a post-processor reads the trace and outputs
        another format. *)
    val read : path:string -> t

    val time_of_writer_close : t -> float
    val num_threads : t -> int

    type trace_kind = Normal | Finaliser
    val trace : t -> kind:trace_kind -> thread_index:int -> Trace.t option

    val frame_table : t -> Frame_table.t
    val shape_table : t -> Shape_table.t
    val num_snapshots : t -> int
    val snapshot : t -> index:int -> heap_snapshot
    val events : t -> Event.t list

    (** Returns [true] iff call count information was recorded in the
        series. *)
    val has_call_counts : t -> bool
  end
end
