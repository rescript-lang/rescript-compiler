
type ('elt, 'id) t0 =
    | Empty 
    | Node of ('elt, 'id) t0 * 'elt * ('elt, 'id) t0 * int
(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)

type ('elt, 'id)enumeration0 = 
    | End 
    | More of 'elt * ('elt, 'id) t0 * ('elt, 'id) enumeration0    

let height = function
    Empty -> 0
  | Node(_, _, _, h) -> h

(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> assert false
    | Node(ll, lv, lr, _) ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else begin
        match lr with
          Empty -> assert false
        | Node(lrl, lrv, lrr, _)->
          create (create ll lv lrl) lrv (create lrr v r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> assert false
    | Node(rl, rv, rr, _) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else begin
        match rl with
          Empty -> assert false
        | Node(rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr)
      end
  end else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

let singleton0 x = Node(Empty, x, Empty, 1)

(* Beware: those two functions assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree; it
   does not test for equality with the current min (or max) element.
   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_element v = function
  | Empty -> singleton0 v
  | Node (l, x, r, h) ->
    bal (add_min_element v l) x r

let rec add_max_element v = function
  | Empty -> singleton0 v
  | Node (l, x, r, h) ->
    bal l x (add_max_element v r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
    if lh > rh + 2 then bal ll lv (join lr v r) else
    if rh > lh + 2 then bal (join l v rl) rv rr else
      create l v r

(* Smallest and greatest element of a set *)

let rec min0 = function
    Empty -> None
  | Node(Empty, v, r, _) -> Some v
  | Node(l, v, r, _) -> min0 l

let rec max0 = function
    Empty -> None
  | Node(l, v, Empty, _) -> Some v
  | Node(l, v, r, _) -> max0 r

(* Remove the smallest element of the given set *)

(* Input is non empty data *)
let rec min_eltAssert0 = function
    Empty -> assert false
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> min_eltAssert0 l
(* Input is non empty data *)
let rec remove_min_elt = function
    Empty -> assert false
  | Node(Empty, v, r, _) -> r
  | Node(l, v, r, _) -> bal (remove_min_elt l) v r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, Node _) -> bal t1 (min_eltAssert0 t2) (remove_min_elt t2)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, Node _) -> join t1 (min_eltAssert0 t2) (remove_min_elt t2)
   
(* Implementation of the set operations *)

let empty0 = Empty

let isEmpty0 = function Empty -> true | _ -> false

let rec cons_enum s e =
  match s with
    Empty -> e
  | Node(l, v, r, _) -> cons_enum l (More(v, r, e))
  

let rec iter0 f = function
    Empty -> ()
  | Node(l, v, r, _) -> iter0 f l; f v [@bs]; iter0 f r

let rec fold0 f s accu =
  match s with
    Empty -> accu
  | Node(l, v, r, _) -> fold0 f r (f v (fold0 f l accu) [@bs])

let rec forAll0 p = function
    Empty -> true
  | Node(l, v, r, _) -> p v [@bs] && forAll0 p l && forAll0 p r

let rec exists0 p = function
    Empty -> false
  | Node(l, v, r, _) -> p v [@bs] || exists0 p l || exists0 p r

let rec filter0 p = function
    Empty -> Empty
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter0 p l in
    let pv = p v [@bs] in
    let r' = filter0 p r in
    if pv then join l' v r' else concat l' r'

let rec partition0 p = function
    Empty -> (Empty, Empty)
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition0 p l in
    let pv = p v [@bs] in
    let (rt, rf) = partition0 p r in
    if pv
    then (join lt v rt, concat lf rf)
    else (concat lt rt, join lf v rf)

let rec cardinal0 = function
    Empty -> 0
  | Node(l, v, r, _) -> cardinal0 l + 1 + cardinal0 r

let rec elements_aux accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements0 s =
  elements_aux [] s


  