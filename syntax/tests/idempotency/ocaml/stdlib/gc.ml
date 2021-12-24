(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type stat = {
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_free : int;
  fragments : int;
  compactions : int;
  top_heap_words : int;
  stack_size : int;
}

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : int;
  mutable max_overhead : int;
  mutable stack_limit : int;
  mutable allocation_policy : int;
  window_size : int;
}

external stat : unit -> stat = "caml_gc_stat"
external quick_stat : unit -> stat = "caml_gc_quick_stat"
external counters : unit -> (float * float * float) = "caml_gc_counters"
external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words_unboxed"
external get : unit -> control = "caml_gc_get"
external set : control -> unit = "caml_gc_set"
external minor : unit -> unit = "caml_gc_minor"
external major_slice : int -> int = "caml_gc_major_slice"
external major : unit -> unit = "caml_gc_major"
external full_major : unit -> unit = "caml_gc_full_major"
external compact : unit -> unit = "caml_gc_compaction"
external get_minor_free : unit -> int = "caml_get_minor_free"
external get_bucket : int -> int = "caml_get_major_bucket" [@@noalloc]
external get_credit : unit -> int = "caml_get_major_credit" [@@noalloc]
external huge_fallback_count : unit -> int = "caml_gc_huge_fallback_count"

open Printf

let print_stat c =
  let st = stat () in
  fprintf c "minor_collections: %d\n" st.minor_collections;
  fprintf c "major_collections: %d\n" st.major_collections;
  fprintf c "compactions:       %d\n" st.compactions;
  fprintf c "\n";
  let l1 = String.length (sprintf "%.0f" st.minor_words) in
  fprintf c "minor_words:    %*.0f\n" l1 st.minor_words;
  fprintf c "promoted_words: %*.0f\n" l1 st.promoted_words;
  fprintf c "major_words:    %*.0f\n" l1 st.major_words;
  fprintf c "\n";
  let l2 = String.length (sprintf "%d" st.top_heap_words) in
  fprintf c "top_heap_words: %*d\n" l2 st.top_heap_words;
  fprintf c "heap_words:     %*d\n" l2 st.heap_words;
  fprintf c "live_words:     %*d\n" l2 st.live_words;
  fprintf c "free_words:     %*d\n" l2 st.free_words;
  fprintf c "largest_free:   %*d\n" l2 st.largest_free;
  fprintf c "fragments:      %*d\n" l2 st.fragments;
  fprintf c "\n";
  fprintf c "live_blocks: %d\n" st.live_blocks;
  fprintf c "free_blocks: %d\n" st.free_blocks;
  fprintf c "heap_chunks: %d\n" st.heap_chunks


let allocated_bytes () =
  let (mi, pro, ma) = counters () in
  (mi +. ma -. pro) *. float_of_int (Sys.word_size / 8)


external finalise : ('a -> unit) -> 'a -> unit = "caml_final_register"
external finalise_last : (unit -> unit) -> 'a -> unit =
  "caml_final_register_called_without_value"
external finalise_release : unit -> unit = "caml_final_release"


type alarm = bool ref
type alarm_rec = {active : alarm; f : unit -> unit}

let rec call_alarm arec =
  if !(arec.active) then begin
    finalise call_alarm arec;
    arec.f ();
  end


let create_alarm f =
  let arec = { active = ref true; f = f } in
  finalise call_alarm arec;
  arec.active


let delete_alarm a = a := false
