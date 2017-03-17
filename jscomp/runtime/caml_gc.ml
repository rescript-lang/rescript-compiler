(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)





(** *)

let dummy_stat : Gc.stat = 
  { minor_words = 0.; 
    promoted_words =0. ;
    major_words = 0. ;
    minor_collections = 0;
    major_collections  = 0;
    heap_words = 0; 
    heap_chunks = 0;
    live_words = 0;
    live_blocks = 0;
    free_words = 0;
    free_blocks = 0;
    largest_free = 0;
    fragments = 0;
    compactions = 0;
    top_heap_words = 0;
    stack_size = 0  }

let caml_gc_stat  () =  
  dummy_stat
let caml_gc_quick_stat () = 
  dummy_stat

let caml_gc_counters : unit -> (float * float * float) = fun () -> (0.,0.,0.)

let caml_gc_get : unit -> Gc.control = fun () -> 
  Gc.{
   minor_heap_size = 0; 
   major_heap_increment = 0;
   space_overhead = 0;
   verbose = 0;
   max_overhead = 0;
   stack_limit = 0;
   allocation_policy = 0;}
 
let caml_gc_set : Gc.control -> unit = fun _ -> ();;

let caml_gc_minor : unit -> unit = fun _ -> ();;

let caml_gc_major_slice : int -> int = fun _ -> 0

let caml_gc_major : unit -> unit = fun  () -> ();;

let caml_gc_full_major : unit -> unit = fun () -> ();;

let caml_gc_compaction : unit -> unit = fun () -> ();;

let caml_final_register : ('a -> unit) -> 'a -> unit = 
  fun _ _ -> ();;

let caml_final_release : unit -> unit = fun _ -> ();;
