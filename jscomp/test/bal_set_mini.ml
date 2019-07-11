type 'a t = Empty | Node of 'a t * 'a * 'a t * int

let rec height = function Empty -> 0 | Node (_, _, _, h) -> h

let create l v r =
  let hl = height l in
  let hr = height r in
  Node (l, v, r, if hl >= hr then hl + 1 else hr + 1)

let bal l v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Empty -> Empty (* impossible *)
    | Node (ll, lv, lr, _) -> (
        if height ll >= height lr then create ll lv (create lr v r)
        else
          match lr with
          | Empty -> Empty (* impossible *)
          | Node (lrl, lrv, lrr, _) ->
              create (create ll lv lrl) lrv (create lrr v r) )
  else if hr > hl + 2 then
    match r with
    | Empty -> Empty (* impossible *)
    | Node (rl, rv, rr, _) -> (
        if height rr >= height rl then create (create l v rl) rv rr
        else
          match rl with
          | Empty -> Empty (* impossible *)
          | Node (rll, rlv, rlr, _) ->
              create (create l v rll) rlv (create rlr rv rr) )
  else Node (l, v, r, if hl >= hr then hl + 1 else hr + 1)

let compare_int (x : int) y = if x > y then 1 else if x = y then 0 else -1

let rec add x = function
  | Empty -> Node (Empty, x, Empty, 1)
  | Node (l, v, r, _) as t ->
      let c = compare_int x v in
      if c = 0 then t
      else if c < 0 then bal (add x l) v r
      else bal l v (add x r)

let rec min_elt def = function
  | Empty -> def
  | Node (Empty, v, r, _) -> v
  | Node (l, v, r, _) -> min_elt v l

(* let rec remove_min_elt tree = match tree with | Empty -> Empty (* impossible
   *) | Node(Empty, v, r, _) -> r | Node(l, v, r, _) -> bal (remove_min_elt l)
   v r *)
let rec remove_min_elt l v r =
  match l with
  | Empty -> r
  | Node (ll, lv, lr, _) -> bal (remove_min_elt ll lv lr) v r

let internal_merge l r =
  match (l, r) with
  | Empty, t -> t
  | t, Empty -> t
  | _, Node (rl, rv, rr, _) -> bal l (min_elt rv r) (remove_min_elt rl rv rr)

let rec remove x tree =
  match tree with
  | Empty -> Empty
  | Node (l, v, r, _) ->
      let c = compare_int x v in
      if c = 0 then internal_merge l r
      else if c < 0 then bal (remove x l) v r
      else bal l v (remove x r)

let rec mem x = function
  | Empty -> false
  | Node (l, v, r, _) ->
      let c = compare_int x v in
      c = 0 || mem x (if c < 0 then l else r)

let () =
  let v = ref Empty in
  let iter = 1_00_000 in
  for i = 0 to iter do
    v := add i !v
  done ;
  for i = 0 to iter do
    if not (mem i !v) then print_endline "impossible"
  done ;
  for i = 0 to iter do
    v := remove i !v
  done ;
  match !v with Empty -> () | Node _ -> print_endline "impossible"
