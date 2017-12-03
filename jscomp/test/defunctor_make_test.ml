

module Comparable : sig
    type 'a comparator
    val getComapre : 'a comparator -> ('a -> 'a -> int [@bs])
    module type C = sig
      type id
      type key
      val compare : key comparator
    end
    type ('key, 'id) compare = (module C with type key = 'key and type id = 'id)
    module Make ( M : sig
        type key
        val compare : key -> key -> int [@bs]
      end) :
      C with type key = M. key 
end = struct
  type 'a comparator = ('a -> 'a -> int [@bs])
    let getComapre : 'a comparator -> ('a -> 'a -> int [@bs]) = fun x -> x
    module type C = sig
      type id
      type key
      val compare : key comparator
    end
    type ('key, 'id) compare = (module C with type key = 'key and type id = 'id)
    
    module Make (M : sig type key
        val compare : (key -> key -> int [@bs])
      end) =
      struct
        type id
        type key = M.key
        let compare = M.compare
      end
end

type ('a, 'key) t =
  | Empty
  | Node of ('a, 'key) t * 'key * 'a * ('a, 'key) t * int
;;
let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
      if height ll >= height lr then
        create ll lv ld (create lr x d r)
      else begin
        match lr with
          Empty -> invalid_arg "Map.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
      if height rr >= height rl then
        create (create l x d rl) rv rd rr
      else begin
        match rl with
          Empty -> invalid_arg "Map.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let rec add x data compare = function
    Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare x v [@bs] in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add x data compare l) v d r
    else
      bal l v d (add x data compare r)

(* type ('k, 'id) compare = *)
(*   (module  C with type id  = 'id and type key = 'k) *)

type ('k,'v, 'id) t1 =
  {
    compare : ('k, 'id) Comparable.compare ;
    data : ('v, 'k) t
  }

let  add (type k) (type v) (type id) x data (v : (k,v, id) t1)  =
  let module X = (val v.compare) in 
  {compare = v.compare ;
   data = add x data  (Comparable.getComapre X.compare ) v.data;}

let empty (v : _ Comparable.compare) =
  {compare = v; data = Empty }



    
(* module Make (X : sig type key end) = struct
  type nonrec key = X.key
  type id 
end
module U = struct
  include Make ( struct type key = int end)
  let compare = Pervasives.compare end *)


module V0 = Comparable.Make ( struct type key = int 
  let compare = fun[@bs]  (x : key) y  -> Pervasives.compare (x : key ) y end)
module V1 = Comparable.Make( struct type key = int 
  let compare = fun [@bs] (x:key) y -> Pervasives.compare x y end)
let v0 = empty (module V0)
let v1 = empty (module V1)

let v3 = add 3 "a" v0
;; Js.log v3
(* let () = v0 = v1 *)

(* let v1 = empty *)
(*     (module (struct type id  type key = int let compare = Pervasives.compare end)) *)

(* let v2 = empty [%bs.map compare] *)
(* let _ = u = v *)
  
(* local variables: *)
(* compile-command: "ocamlc.opt -c xx.ml" *)
(* end: *)
