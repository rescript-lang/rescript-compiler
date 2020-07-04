(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

[@@@warnerror "+55"]
(** adapted from stdlib *)

type ('key,'a) t =
  | Empty
  | Node of {
    l : ('key,'a) t ;
    k : 'key ;
    v : 'a ;
    r : ('key,'a) t ;
    h : int
  }

let  empty = Empty
let rec map x f = match x with
    Empty ->
    Empty
  | Node ({l; v ; r} as x) ->
    let l' = map l f in
    let d' = f v in
    let r' = map r f in
    Node { x with  l = l';  v = d'; r = r'}

let rec mapi x f = match x with
    Empty ->
    Empty
  | Node ({l; k ; v ; r} as x) ->
    let l' = mapi l f in
    let v' = f k v in
    let r' = mapi r f in
    Node {x with l = l'; v = v'; r = r'}

let [@inline] calc_height a b = (if a >= b  then a else b) + 1 
let [@inline] singleton x d = Node {l = Empty; k = x; v = d; r = Empty; h = 1}
let [@inline] height = function
  | Empty -> 0
  | Node {h} -> h

let [@inline] unsafe_node l x d r h =   
  Node {l; k = x; v = d;r; h}

let [@inline] create l k v r =
  Node{l; k; v; r; h= calc_height (height l) (height r)}




let rec cardinal_aux acc  = function
  | Empty -> acc 
  | Node {l; r} -> 
    cardinal_aux  (cardinal_aux (acc + 1)  r ) l 

let cardinal s = cardinal_aux 0 s 

let rec bindings_aux accu = function
  | Empty -> accu
  | Node {l;k;v;r} -> bindings_aux ((k, v) :: bindings_aux accu r) l

let bindings s =
  bindings_aux [] s

let rec fill_array_with_f (s : _ t) i arr  f : int =    
  match s with 
  | Empty -> i 
  | Node {l; k; v; r} -> 
    let inext = fill_array_with_f l i arr f in 
    Array.unsafe_set arr inext (f k v);
    fill_array_with_f r (inext + 1) arr f

let rec fill_array_aux (s : _ t) i arr : int =    
  match s with 
  | Empty -> i 
  | Node {l;k;v;r} -> 
    let inext = fill_array_aux l i arr in 
    Array.unsafe_set arr inext (k,v);
    fill_array_aux r (inext + 1) arr 


let to_sorted_array (s : ('key,'a) t)  : ('key * 'a ) array =    
  match s with 
  | Empty -> [||]
  | Node {l;k;v;r} -> 
    let len = 
      cardinal_aux (cardinal_aux 1 r) l in 
    let arr =
      Array.make len (k,v) in  
    ignore (fill_array_aux s 0 arr : int);
    arr 

let to_sorted_array_with_f (type key a b ) (s : (key,a) t)  (f : key -> a -> b): b array =    
  match s with 
  | Empty -> [||]
  | Node {l;k;v;r} -> 
    let len = 
      cardinal_aux (cardinal_aux 1 r) l in 
    let arr =
      Array.make len (f k v) in  
    ignore (fill_array_with_f s 0 arr f: int);
    arr     

let rec keys_aux accu = function
    Empty -> accu
  | Node {l; k;r} -> keys_aux (k :: keys_aux accu r) l

let keys s = keys_aux [] s





let bal l x d r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then begin
    let [@warning "-8"] Node ({l=ll; r = lr} as l) = l in
    if height ll >= height lr then
      create ll l.k l.v (create lr x d r)
    else         
      let [@warning "-8"] Node ({l=lrl; r=lrr} as lr) = lr in 
      create (create ll l.k l.v lrl) lr.k lr.v (create lrr x d r)      
  end else if hr > hl + 2 then begin
    let [@warning "-8"] Node ({l=rl; r=rr} as r) = r in 
    if height rr >= height rl then
      create (create l x d rl) r.k r.v rr
    else 
      let [@warning "-8"] Node ({l=rll;  r=rlr} as rl) = rl in 
      create (create l x d rll) rl.k rl.v (create rlr r.k r.v rr)
  end else
    Node{l; k=x; v=d; r; h=calc_height hl hr}



let [@inline] is_empty = function Empty -> true | _ -> false

let rec min_binding_exn = function
    Empty -> raise Not_found
  | Node{l=Empty; k; v} -> (k, v)
  | Node{l} -> min_binding_exn l


let rec remove_min_binding = function
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node{l=Empty;r} -> r
  | Node{l; k; v ; r} -> bal (remove_min_binding l) k v r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_exn t2 in
    bal t1 x d (remove_min_binding t2)


let rec iter x f = match x with 
    Empty -> ()
  | Node{l; k ; v ; r} ->
    iter l f; f k v; iter r f



let rec fold m accu f =
  match m with
    Empty -> accu
  | Node {l; k; v; r} ->
    fold r (f k v (fold l accu f)) f 

let rec for_all x p = match x with 
    Empty -> true
  | Node{l; k = v; v = d; r} -> p v d && for_all l p && for_all r p

let rec exists x p = match x with
    Empty -> false
  | Node{l; k = v; v = d; r} -> p v d || exists l p || exists r p

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min k v = function
  | Empty -> singleton k v
  | Node tree ->
    bal (add_min k v tree.l) tree.k tree.v tree.r

let rec add_max k v = function
  | Empty -> singleton k v
  | Node tree ->
    bal tree.l tree.k tree.v (add_max k v tree.r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match l with
  | Empty -> add_min v d r
  | Node xl ->
    match r with  
    | Empty -> add_max v d l 
    | Node  xr ->
      let lh = xl.h in  
      let rh = xr.h in 
      if lh > rh + 2 then bal xl.l xl.k xl.v (join xl.r v d r) else
      if rh > lh + 2 then bal (join l v d xr.l) xr.k xr.v xr.r else
        create l v d r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_exn t2 in
    join t1 x d (remove_min_binding t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

    
module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val compare_key: key -> key -> int 
    val is_empty: 'a t -> bool
    val mem: 'a t -> key -> bool
    val to_sorted_array : 
      'a t -> (key * 'a ) array
    val to_sorted_array_with_f : 
      'a t -> (key -> 'a -> 'b) -> 'b array  
    val add: 'a t -> key -> 'a -> 'a t
    (** [add x y m] 
        If [x] was already bound in [m], its previous binding disappears. *)

    val adjust: 'a t -> key -> ('a option->  'a) ->  'a t 
    (** [adjust acc k replace ] if not exist [add (replace None ], otherwise 
        [add k v (replace (Some old))]
    *)
    
    val singleton: key -> 'a -> 'a t

    val remove: 'a t -> key -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val merge:
         'a t -> 'b t ->
         (key -> 'a option -> 'b option -> 'c option) ->  'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        @since 3.12.0
     *)

    val disjoint_merge : 'a t -> 'a t -> 'a t
     (* merge two maps, will raise if they have the same key *)



    val iter: 'a t -> (key -> 'a -> unit) ->  unit
    (** [iter f m] applies [f] to all bindings in map [m].
        The bindings are passed to [f] in increasing order. *)

    val fold: 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order) *)

    val for_all: 'a t -> (key -> 'a -> bool) -> bool
    (** [for_all p m] checks if all the bindings of the map.
        order unspecified
     *)

    val exists: 'a t -> (key -> 'a -> bool) -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p]. 
        order unspecified
     *)

    (* val filter: 'a t -> (key -> 'a -> bool) -> 'a t *)
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
        order unspecified
     *)

    (* val partition: 'a t -> (key -> 'a -> bool) ->  'a t * 'a t *)
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map. *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering *)
    val keys : 'a t -> key list 
    (* Increasing order *)



    (* val split: 'a t -> key -> 'a t * 'a option * 'a t *)
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find_exn: 'a t -> key ->  'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
    val find_opt:  'a t ->  key ->'a option
    val find_default: 'a t -> key  ->  'a  -> 'a 
    val map: 'a t -> ('a -> 'b) -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: 'a t ->  (key -> 'a -> 'b) -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val of_list : (key * 'a) list -> 'a t 
    val of_array : (key * 'a ) array -> 'a t 
    val add_list : (key * 'b) list -> 'b t -> 'b t

  end
