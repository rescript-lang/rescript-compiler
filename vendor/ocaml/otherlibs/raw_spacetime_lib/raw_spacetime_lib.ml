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
end = struct
  type t = {
    minor_words : int;
    promoted_words : int;
    major_words : int;
    minor_collections : int;
    major_collections : int;
    heap_words : int;
    heap_chunks : int;
    compactions : int;
    top_heap_words : int;
  }

  let minor_words t = t.minor_words
  let promoted_words t = t.promoted_words
  let major_words t = t.major_words
  let minor_collections t = t.minor_collections
  let major_collections t = t.major_collections
  let heap_words t = t.heap_words
  let heap_chunks t = t.heap_chunks
  let compactions t = t.compactions
  let top_heap_words t = t.top_heap_words
end

module Program_counter = struct
  module OCaml = struct
    type t = Int64.t

    let to_int64 t = t
  end

  module Foreign = struct
    type t = Int64.t

    let to_int64 t = t
  end
end

module Function_identifier = struct
  type t = Int64.t

  let to_int64 t = t
end

module Function_entry_point = struct
  type t = Int64.t

  let to_int64 t = t
end

module Int64_map = Map.Make (Int64)

module Frame_table = struct
  type raw = (Int64.t * (Printexc.Slot.t list)) list

  type t = Printexc.Slot.t list Int64_map.t

  let demarshal chn : t =
    let raw : raw = Marshal.from_channel chn in
    List.fold_left (fun map (pc, rev_location_list) ->
        Int64_map.add pc (List.rev rev_location_list) map)
      Int64_map.empty
      raw

  let find_exn = Int64_map.find
end

module Shape_table = struct
  type part_of_shape =
    | Direct_call of { call_site : Int64.t; callee : Int64.t; }
    | Indirect_call of Int64.t
    | Allocation_point of Int64.t

  let _ = Direct_call { call_site = 0L; callee = 0L; }
  let _ = Indirect_call 0L
  let _ = Allocation_point 0L

  type raw = (Int64.t * (part_of_shape list)) list

  type t = {
    shapes : part_of_shape list Int64_map.t;
    call_counts : bool;
  }

  let part_of_shape_size t = function
    | Direct_call _ -> if t.call_counts then 2 else 1
    | Indirect_call _ -> 1
    | Allocation_point _ -> 3

  let demarshal chn ~call_counts : t =
    let raw : raw = Marshal.from_channel chn in
    let shapes =
      List.fold_left (fun map (key, data) -> Int64_map.add key data map)
        Int64_map.empty
        raw
    in
    { shapes;
      call_counts;
    }

  let find_exn func_id t = Int64_map.find func_id t.shapes
  let call_counts t = t.call_counts
end

module Annotation = struct
  type t = int

  let to_int t = t
end

module Trace = struct
  type node
  type ocaml_node
  type foreign_node
  type uninstrumented_node

  type t = node option
  type trace = t

  (* This function unmarshals into malloc blocks, which mean that we
     obtain a straightforward means of writing [compare] on [node]s. *)
  external unmarshal : in_channel -> 'a
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_unmarshal_trie"

  let unmarshal in_channel =
    let trace = unmarshal in_channel in
    if trace = () then
      None
    else
      Some ((Obj.magic trace) : node)

  let node_is_null (node : node) =
    ((Obj.magic node) : unit) == ()

  let foreign_node_is_null (node : foreign_node) =
    ((Obj.magic node) : unit) == ()

  external node_num_header_words : unit -> int
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_node_num_header_words" "noalloc"

  let num_header_words = lazy (node_num_header_words ())

  module OCaml = struct
    type field_iterator = {
      node : ocaml_node;
      offset : int;
      part_of_shape : Shape_table.part_of_shape;
      remaining_layout : Shape_table.part_of_shape list;
      shape_table : Shape_table.t;
    }

    module Allocation_point = struct
      type t = field_iterator

      let program_counter t =
        match t.part_of_shape with
        | Shape_table.Allocation_point call_site -> call_site
        | _ -> assert false

      external annotation : ocaml_node -> int -> Annotation.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_allocation_point_annotation"
          "noalloc"

      let annotation t = annotation t.node t.offset

      external count : ocaml_node -> int -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_allocation_point_count"
          "noalloc"

      let num_words_including_headers t = count t.node t.offset
    end

    module Direct_call_point = struct
      type _ t = field_iterator

      let call_site t =
        match t.part_of_shape with
        | Shape_table.Direct_call { call_site; _ } -> call_site
        | _ -> assert false

      let callee t =
        match t.part_of_shape with
        | Shape_table.Direct_call { callee; _ } -> callee
        | _ -> assert false

      external callee_node : ocaml_node -> int -> 'target
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_direct_call_point_callee_node"

      let callee_node (type target) (t : target t) : target =
        callee_node t.node t.offset

      external call_count : ocaml_node -> int -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_direct_call_point_call_count"

      let call_count t =
        if Shape_table.call_counts t.shape_table then
          Some (call_count t.node t.offset)
        else
          None
    end

    module Indirect_call_point = struct
      type t = field_iterator

      let call_site t =
        match t.part_of_shape with
        | Shape_table.Indirect_call call_site -> call_site
        | _ -> assert false

      module Callee = struct
        (* CR-soon mshinwell: we should think about the names again.  This is
           a "c_node" but it isn't foreign. *)
        type t = {
          node : foreign_node;
          call_counts : bool;
        }

        let is_null t = foreign_node_is_null t.node

        (* CR-soon mshinwell: maybe rename ...c_node_call_site -> c_node_pc,
           since it isn't a call site in this case. *)
        external callee : foreign_node -> Function_entry_point.t
          = "caml_spacetime_only_works_for_native_code"
            "caml_spacetime_c_node_call_site"

        let callee t = callee t.node

        (* This can return a node satisfying "is_null" in the case of an
           uninitialised tail call point.  See the comment in the C code. *)
        external callee_node : foreign_node -> node
          = "caml_spacetime_only_works_for_native_code"
            "caml_spacetime_c_node_callee_node" "noalloc"

        let callee_node t = callee_node t.node

        external call_count : foreign_node -> int
          = "caml_spacetime_only_works_for_native_code"
            "caml_spacetime_c_node_call_count"

        let call_count t =
          if t.call_counts then Some (call_count t.node)
          else None

        external next : foreign_node -> foreign_node
          = "caml_spacetime_only_works_for_native_code"
            "caml_spacetime_c_node_next" "noalloc"

        let next t =
          let next = { t with node = next t.node; } in
          if foreign_node_is_null next.node then None
          else Some next
      end

      external callees : ocaml_node -> int -> foreign_node
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_indirect_call_point_callees"
          "noalloc"

      let callees t =
        let callees =
          { Callee.
            node = callees t.node t.offset;
            call_counts = Shape_table.call_counts t.shape_table;
          }
        in
        if Callee.is_null callees then None
        else Some callees
    end

    module Field = struct
      type t = field_iterator

      type direct_call_point =
        | To_ocaml of ocaml_node Direct_call_point.t
        | To_foreign of foreign_node Direct_call_point.t
        | To_uninstrumented of
            uninstrumented_node Direct_call_point.t

      type classification =
        | Allocation of Allocation_point.t
        | Direct_call of direct_call_point
        | Indirect_call of Indirect_call_point.t

      external classify_direct_call_point : ocaml_node -> int -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_classify_direct_call_point"
          "noalloc"

      let classify t =
        match t.part_of_shape with
        | Shape_table.Direct_call callee ->
          let direct_call_point =
            match classify_direct_call_point t.node t.offset with
            | 0 ->
              (* We should never classify uninitialised call points here. *)
              assert false
            | 1 -> To_ocaml t
            | 2 -> To_foreign t
            | _ -> assert false
          in
          Direct_call direct_call_point
        | Shape_table.Indirect_call _ -> Indirect_call t
        | Shape_table.Allocation_point _ -> Allocation t

      (* CR-soon mshinwell: change to "is_unused"? *)
      let is_uninitialised t =
        let offset_to_node_hole =
          match t.part_of_shape with
          | Shape_table.Direct_call _ -> Some 0
          | Shape_table.Indirect_call _ -> Some 0
          | Shape_table.Allocation_point _ -> None
        in
        match offset_to_node_hole with
        | None -> false
        | Some offset_to_node_hole ->
          (* There are actually two cases:
             1. A normal unused node hole, which says Val_unit;
             2. An unused tail call point.  This will contain a pointer to the
                start of the current node, but it also has the bottom bit
                set. *)
          let offset = t.offset + offset_to_node_hole in
          Obj.is_int (Obj.field (Obj.repr t.node) offset)

      let rec next t =
        match t.remaining_layout with
        | [] -> None
        | part_of_shape::remaining_layout ->
          let size =
            Shape_table.part_of_shape_size t.shape_table t.part_of_shape
          in
          let offset = t.offset + size in
          assert (offset < Obj.size (Obj.repr t.node));
          let t =
            { node = t.node;
              offset;
              part_of_shape;
              remaining_layout;
              shape_table = t.shape_table;
            }
          in
          skip_uninitialised t

      and skip_uninitialised t =
        if not (is_uninitialised t) then Some t
        else next t
    end

    module Node = struct
      type t = ocaml_node

      external function_identifier : t -> Function_identifier.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_function_identifier"

      external next_in_tail_call_chain : t -> t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_tail_chain" "noalloc"

      external compare : t -> t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_compare_node" "noalloc"

      let fields t ~shape_table =
        let id = function_identifier t in
        match Shape_table.find_exn id shape_table with
        | exception Not_found -> None
        | [] -> None
        | part_of_shape::remaining_layout ->
          let t =
            { node = t;
              offset = Lazy.force num_header_words;
              part_of_shape;
              remaining_layout;
              shape_table;
            }
          in
          Field.skip_uninitialised t
    end
  end

  module Foreign = struct
    module Node = struct
      type t = foreign_node

      external compare : t -> t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_compare_node" "noalloc"

      let fields t =
        if foreign_node_is_null t then None
        else Some t
    end

    module Allocation_point = struct
      type t = foreign_node

      external program_counter : t -> Program_counter.Foreign.t
        (* This is not a mistake; the same C function works. *)
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_call_site"

      external annotation : t -> Annotation.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_profinfo" "noalloc"

      external num_words_including_headers : t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_allocation_count" "noalloc"
    end

    module Call_point = struct
      type t = foreign_node

      external call_site : t -> Program_counter.Foreign.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_call_site"

      (* May return a null node.  See comment above and the C code. *)
      external callee_node : t -> node
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_callee_node" "noalloc"
    end

    module Field = struct
      type t = foreign_node

      type classification =
        | Allocation of Allocation_point.t
        | Call of Call_point.t

      external is_call : t -> bool
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_is_call" "noalloc"

      let classify t =
        if is_call t then Call t
        else Allocation t

      external next : t -> t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_next" "noalloc"

      let next t =
        let next = next t in
        if foreign_node_is_null next then None
        else Some next
    end
  end

  module Node = struct
    module T = struct
      type t = node

      external compare : t -> t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_compare_node" "noalloc"
    end

    include T

    type classification =
      | OCaml of OCaml.Node.t
      | Foreign of Foreign.Node.t

    (* CR-soon lwhite: These functions should work in bytecode *)
    external is_ocaml_node : t -> bool
      = "caml_spacetime_only_works_for_native_code"
        "caml_spacetime_is_ocaml_node" "noalloc"

    let classify t =
      if is_ocaml_node t then OCaml ((Obj.magic t) : ocaml_node)
      else Foreign ((Obj.magic t) : foreign_node)

    let of_ocaml_node (node : ocaml_node) : t = Obj.magic node
    let of_foreign_node (node : foreign_node) : t = Obj.magic node

    module Map = Map.Make (T)
    module Set = Set.Make (T)
  end

  let root t = t
end

module Heap_snapshot = struct

  module Entries = struct
    type t = int array  (* == "struct snapshot_entries" *)

    let length t =
      let length = Array.length t in
      assert (length mod 3 = 0);
      length / 3

    let annotation t idx = t.(idx*3)
    let num_blocks t idx = t.(idx*3 + 1)
    let num_words_including_headers t idx = t.(idx*3 + 2)
  end

  type total_allocations =
    | End
    | Total of {
        annotation : Annotation.t;
        count : int;
        next : total_allocations;
      }

  let (_ : total_allocations) =  (* suppress compiler warning *)
    Total { annotation = 0; count = 0; next = End; }

  type t = {
    timestamp : float;
    gc_stats : Gc_stats.t;
    entries : Entries.t;
    words_scanned : int;
    words_scanned_with_profinfo : int;
    total_allocations : total_allocations;
  }

  type heap_snapshot = t

  let timestamp t = t.timestamp
  let gc_stats t = t.gc_stats
  let entries t = t.entries
  let words_scanned t = t.words_scanned
  let words_scanned_with_profinfo t = t.words_scanned_with_profinfo

  module Total_allocation = struct
    type t = total_allocations  (* [End] is forbidden *)

    let annotation = function
      | End -> assert false
      | Total { annotation; _ } -> annotation

    let num_words_including_headers = function
      | End -> assert false
      | Total { count; _ } -> count

    let next = function
      | End -> assert false
      | Total { next = End; _ } -> None
      | Total { next; _ } -> Some next
  end

  let total_allocations t =
    match t.total_allocations with
    | End -> None
    | (Total _) as totals -> Some totals

  module Event = struct
    type t = {
      event_name : string;
      time : float;
    }

    let event_name t = t.event_name
    let timestamp t = t.time
  end

  module Series = struct
    type t = {
      num_snapshots : int;
      time_of_writer_close : float;
      frame_table : Frame_table.t;
      shape_table : Shape_table.t;
      traces_by_thread : Trace.t array;
      finaliser_traces_by_thread : Trace.t array;
      snapshots : heap_snapshot array;
      events : Event.t list;
      call_counts : bool;
    }

    let pathname_suffix_trace = "trace"

    (* The order of these constructors must match the C code. *)
    type what_comes_next =
      | Snapshot
      | Traces
      | Event

    (* Suppress compiler warning 37. *)
    let _ : what_comes_next list = [Snapshot; Traces; Event;]

    let rec read_snapshots_and_events chn snapshots events =
      let next : what_comes_next = Marshal.from_channel chn in
      match next with
      | Snapshot ->
        let snapshot : heap_snapshot = Marshal.from_channel chn in
        read_snapshots_and_events chn (snapshot :: snapshots) events
      | Event ->
        let event_name : string = Marshal.from_channel chn in
        let time : float = Marshal.from_channel chn in
        let event = { Event. event_name; time; } in
        read_snapshots_and_events chn snapshots (event :: events)
      | Traces ->
        (Array.of_list (List.rev snapshots)), List.rev events

    let read ~path =
      let chn = open_in path in
      let magic_number : int = Marshal.from_channel chn in
      let magic_number_base = magic_number land 0xffff_ffff in
      let version_number = (magic_number lsr 32) land 0xffff in
      let features = (magic_number lsr 48) land 0xffff in
      if magic_number_base <> 0xace00ace then begin
        failwith "Raw_spacetime_lib: not a Spacetime profiling file"
      end else begin
        match version_number with
        | 0 ->
          let call_counts =
            match features with
            | 0 -> false
            | 1 -> true
            | _ ->
              failwith "Raw_spacetime_lib: unknown Spacetime profiling file \
                feature set"
          in
          let snapshots, events = read_snapshots_and_events chn [] [] in
          let num_snapshots = Array.length snapshots in
          let time_of_writer_close : float = Marshal.from_channel chn in
          let frame_table = Frame_table.demarshal chn in
          let shape_table = Shape_table.demarshal chn ~call_counts in
          let num_threads : int = Marshal.from_channel chn in
          let traces_by_thread = Array.init num_threads (fun _ -> None) in
          let finaliser_traces_by_thread =
            Array.init num_threads (fun _ -> None)
          in
          for thread = 0 to num_threads - 1 do
            let trace : Trace.t = Trace.unmarshal chn in
            let finaliser_trace : Trace.t = Trace.unmarshal chn in
            traces_by_thread.(thread) <- trace;
            finaliser_traces_by_thread.(thread) <- finaliser_trace
          done;
          close_in chn;
          { num_snapshots;
            time_of_writer_close;
            frame_table;
            shape_table;
            traces_by_thread;
            finaliser_traces_by_thread;
            snapshots;
            events;
            call_counts;
          }
        | _ ->
          failwith "Raw_spacetime_lib: unknown Spacetime profiling file \
            version number"
      end

    type trace_kind = Normal | Finaliser

    let num_threads t = Array.length t.traces_by_thread

    let trace t ~kind ~thread_index =
      if thread_index < 0 || thread_index >= num_threads t then None
      else
        match kind with
        | Normal -> Some t.traces_by_thread.(thread_index)
        | Finaliser -> Some t.finaliser_traces_by_thread.(thread_index)

    let num_snapshots t = t.num_snapshots
    let snapshot t ~index = t.snapshots.(index)
    let frame_table t = t.frame_table
    let shape_table t = t.shape_table
    let time_of_writer_close t = t.time_of_writer_close
    let events t = t.events
    let has_call_counts t = t.call_counts
  end
end
