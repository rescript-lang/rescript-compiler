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
(** Adapted by authors of BuckleScript without using functors          *)

type ('key, 'a, 'id) t0 =
    Empty
  | Node of ('key, 'a, 'id) t0 * 'key * 'a * ('key, 'a, 'id) t0 * int

type ('key, 'a, 'id) enumeration0 =
    End 
   | More of 'key * 'a * ('key, 'a, 'id) t0 * ('key, 'a, 'id) enumeration0
  
let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let singleton0 x d = Node(Empty, x, d, Empty, 1)

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> assert false
    | Node(ll, lv, ld, lr, _) ->
      if height ll >= height lr then
        create ll lv ld (create lr x d r)
      else begin
        match lr with
          Empty -> assert false
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> assert false
    | Node(rl, rv, rd, rr, _) ->
      if height rr >= height rl then
        create (create l x d rl) rv rd rr
      else begin
        match rl with
          Empty -> assert false
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let empty0 = Empty

let isEmpty0 = function Empty -> true | _ -> false    

let rec minBinding0 = function
    Empty -> None
  | Node(Empty, x, d, r, _) -> Some (x, d)
  | Node(l, x, d, r, _) -> minBinding0 l
  
let rec maxBinding0 = function
    Empty -> None
  | Node(l, x, d, Empty, _) -> Some (x, d)
  | Node(l, x, d, r, _) -> maxBinding0 r

(* only internal use for a non empty map*)
let rec minBindingAssert0 = function
    Empty -> assert false
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> minBindingAssert0 l

  
(* only internal use for a non empty map*)  
let rec remove_minBinding = function
    Empty -> assert false
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_minBinding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = minBindingAssert0 t2 in
    bal t1 x d (remove_minBinding t2)

let rec iter0 f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter0 f l; f v d [@bs]; iter0 f r

let rec map0 f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = map0 f l in
    let d' = f d [@bs] in
    let r' = map0 f r in
    Node(l', v, d', r', h)

let rec mapi0 f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = mapi0 f l in
    let d' = f v d [@bs] in
    let r' = mapi0 f r in
    Node(l', v, d', r', h)

let rec fold0 f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
    fold0 f r (f v d (fold0 f l accu) [@bs])

let rec forAll0 p = function
    Empty -> true
  | Node(l, v, d, r, _) -> p v d [@bs] && forAll0 p l && forAll0 p r

let rec exists0 p = function
    Empty -> false
  | Node(l, v, d, r, _) -> p v d [@bs] || exists0 p l || exists0 p r

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_minBinding k v = function
  | Empty -> singleton0 k v
  | Node (l, x, d, r, h) ->
    bal (add_minBinding k v l) x d r

let rec add_maxBinding k v = function
  | Empty -> singleton0 k v
  | Node (l, x, d, r, h) ->
    bal l x d (add_maxBinding k v r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match (l, r) with
    (Empty, _) -> add_minBinding v d r
  | (_, Empty) -> add_maxBinding v d l
  | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
    if lh > rh + 2 then bal ll lv ld (join lr v d r) else
    if rh > lh + 2 then bal (join l v d rl) rv rd rr else
      create l v d r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, Node _) ->
    let (x, d) = minBindingAssert0 t2 in
    join t1 x d (remove_minBinding t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2    

let rec filter0 p = function
    Empty -> Empty
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter0 p l in
    let pvd = p v d [@bs] in
    let r' = filter0 p r in
    if pvd then join l' v d r' else concat l' r'

let rec partition0 p = function
    Empty -> (Empty, Empty)
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition0 p l in
    let pvd = p v d [@bs] in
    let (rt, rf) = partition0 p r in
    if pvd
    then (join lt v d rt, concat lf rf)
    else (concat lt rt, join lf v d rf)  

let rec cons_enum m e =
  match m with
    Empty -> e
  | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))
    
let rec cardinal0 = function
    Empty -> 0
  | Node(l, _, _, r, _) -> cardinal0 l + 1 + cardinal0 r

let rec bindings_aux accu = function
    Empty -> accu
  | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

let bindings0 s =
  bindings_aux [] s  