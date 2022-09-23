(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Hash tables *)


type ('a, 'b) t = ('a,'b) Hashtbl.t



let {create;
    clear;reset;copy;add;find;find_opt;find_all;mem;remove;replace;iter;filter_map_inplace;fold;length;randomize;is_randomized;stats;hash;seeded_hash;hash_param;seeded_hash_param} = (module Hashtbl)

let add tbl ~key ~data = add tbl key data

let replace tbl ~key ~data = replace tbl key data

let iter ~f tbl = iter (fun key data -> f ~key ~data) tbl 

let filter_map_inplace ~f tbl =
   filter_map_inplace (fun key data -> f ~key ~data) tbl 

let fold ~f tbl ~init = 
  fold (fun key data acc -> f ~key ~data acc) tbl init

type statistics = Hashtbl.statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}


(* Functorial interface *)

module type HashedType = Hashtbl.HashedType

module type SeededHashedType = Hashtbl.SeededHashedType


module type S =
sig
  type key
  and 'a t
  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key:key -> data:'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt: 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key:key -> data:'a -> unit
  val mem : 'a t -> key -> bool
  val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
  val filter_map_inplace:
    f:(key:key -> data:'a -> 'a option) -> 'a t -> unit
  val fold :
      f:(key:key -> data:'a -> 'b -> 'b) ->
      'a t -> init:'b -> 'b
  val length : 'a t -> int
  val stats: 'a t -> statistics
end

module type SeededS =
sig
  type key
  and 'a t
  val create : ?random:bool -> int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key:key -> data:'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key:key -> data:'a -> unit
  val mem : 'a t -> key -> bool
  val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
  val filter_map_inplace:
    f:(key:key -> data:'a -> 'a option) -> 'a t -> unit
  val fold :
      f:(key:key -> data:'a -> 'b -> 'b) ->
      'a t -> init:'b -> 'b
  val length : 'a t -> int
  val stats: 'a t -> statistics
end

module MakeSeeded(H: SeededHashedType): (SeededS with type key = H.t) = struct
  include Hashtbl.MakeSeeded(H)
  let add tbl ~key ~data = add tbl key data
  let replace tbl ~key ~data = replace tbl key data

  let iter ~f tbl = iter (fun key data -> f ~key ~data) tbl 
  



  
  let filter_map_inplace ~f tbl =
     filter_map_inplace (fun key data -> f ~key ~data) tbl 
  
  let fold ~f tbl ~init = 
    fold (fun key data acc -> f ~key ~data acc) tbl init
    
end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
  end
