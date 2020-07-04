
(* we don't create [map_poly], since some operations require raise an exception which carries [key] *)
[@@@warnerror"a"]

#ifdef TYPE_FUNCTOR
module Make(Ord: Map.OrderedType) = struct
  type key = Ord.t
  let compare_key = Ord.compare 
#elif defined TYPE_STRING
  type key = string 
  let compare_key = Ext_string.compare
#elif defined TYPE_INT
  type key = int
  let compare_key = Ext_int.compare
#elif defined TYPE_IDENT
  type key = Ident.t
  let compare_key = Ext_ident.compare
#else
  [%error "unknown type"]
#endif

type + 'a t = (key,'a) Map_gen.t
exception Duplicate_key of key 

let empty = Map_gen.empty 
let is_empty = Map_gen.is_empty
let iter = Map_gen.iter
let fold = Map_gen.fold
let for_all = Map_gen.for_all 
let exists = Map_gen.exists 
let singleton = Map_gen.singleton 
let cardinal = Map_gen.cardinal
let bindings = Map_gen.bindings
let to_sorted_array = Map_gen.to_sorted_array
let to_sorted_array_with_f = Map_gen.to_sorted_array_with_f
let keys = Map_gen.keys



let map = Map_gen.map 
let mapi = Map_gen.mapi
let bal = Map_gen.bal 
let height = Map_gen.height 


let rec add (tree : _ Map_gen.t as 'a) x data  : 'a = match tree with 
  | Empty ->
    singleton x data
  | Node {l; k ; v ; r; h} ->
    let c = compare_key x k in
    if c = 0 then
      Map_gen.unsafe_node l x data r h (* at least need update data *)
    else if c < 0 then
      bal (add l x data ) k v r
    else
      bal l k v (add r x data )


let rec adjust (tree : _ Map_gen.t as 'a) x replace  : 'a = 
  match tree with 
  | Empty ->
    singleton x (replace None)
  | Node ({l; k ; r} as tree) ->
    let c = compare_key x k in
    if c = 0 then
      Map_gen.unsafe_node l x (replace  (Some tree.v))  r tree.h
    else if c < 0 then
      bal (adjust l x  replace ) k tree.v r
    else
      bal l k tree.v (adjust r x  replace )


let rec find_exn (tree : _ Map_gen.t ) x = match tree with 
  | Empty ->
    raise Not_found
  | Node tree ->
    let c = compare_key x tree.k in
    if c = 0 then tree.v
    else find_exn (if c < 0 then tree.l else tree.r) x

let rec find_opt (tree : _ Map_gen.t ) x = match tree with 
  | Empty -> None 
  | Node tree ->
    let c = compare_key x tree.k in
    if c = 0 then Some tree.v
    else find_opt (if c < 0 then tree.l else tree.r) x

let rec find_default (tree : _ Map_gen.t ) x  default     = match tree with 
  | Empty -> default  
  | Node tree ->
    let c = compare_key x tree.k in
    if c = 0 then tree.v
    else find_default (if c < 0 then tree.l else tree.r) x default

let rec mem (tree : _ Map_gen.t )  x= match tree with 
  | Empty ->
    false
  | Node{l; k = v;  r} ->
    let c = compare_key x v in
    c = 0 || mem (if c < 0 then l else r) x 

let rec remove (tree : _ Map_gen.t as 'a) x : 'a = match tree with 
  | Empty -> empty
  | Node{l; k = v; v = d; r} ->
    let c = compare_key x v in
    if c = 0 then
      Map_gen.merge l r
    else if c < 0 then
      bal (remove l x) v d r
    else
      bal l v d (remove r x )


let rec split (tree : _ Map_gen.t as 'a) x : 'a * _ option * 'a  = 
  match tree with 
  | Empty ->
    (empty, None, empty)
  | Node {l; k ; v ; r} ->
    let c = compare_key x k in
    if c = 0 then (l, Some v, r)
    else if c < 0 then
      let (ll, pres, rl) = split l x in 
      (ll, pres, Map_gen.join rl k v r)
    else
      let (lr, pres, rr) = split r x in 
      (Map_gen.join l k v lr, pres, rr)

let rec merge (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) f  : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> empty
  | Node ({ k  } as s1), _ when s1.h >= height s2 ->
    let (l, v, r) = split s2 k in
    Map_gen.concat_or_join 
      (merge s1.l l f) k 
      (f k (Some s1.v) v) 
      (merge s1.r r f)
  | _, Node ({k  } as s2) ->
    let (l, v, r) = split s1 k in
    Map_gen.concat_or_join 
      (merge l s2.l f) 
      k
      (f k v (Some s2.v)) 
      (merge r s2.r f)
  | _ ->
    assert false

let rec disjoint_merge  (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> empty
  | Node ({k} as s1), _ when s1.h >= height s2 ->
    begin match split s2 k with 
      | l, None, r -> 
        Map_gen.join 
          (disjoint_merge  s1.l l)
          k 
          s1.v 
          (disjoint_merge s1.r r)
      | _, Some _, _ ->
        raise (Duplicate_key  k)
    end        
  | _, Node ({k} as s2) ->
    begin match  split s1 k with 
      | (l, None, r) -> 
        Map_gen.join 
          (disjoint_merge  l s2.l) k s2.v 
          (disjoint_merge  r s2.r)
      | (_, Some _, _) -> 
        raise (Duplicate_key k)
    end
  | _ ->
    assert false






let add_list (xs : _ list ) init = 
  Ext_list.fold_left xs init (fun  acc (k,v) -> add acc k v )

let of_list xs = add_list xs empty

let of_array xs = 
  Ext_array.fold_left xs empty (fun acc (k,v) -> add acc k v ) 
#ifdef TYPE_FUNCTOR
end
#endif
