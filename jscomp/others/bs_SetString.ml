# 2 "set.cppo.ml"
type elt = string


# 10
module N = Bs_internalAVLset

type ('elt, 'id) t0 = ('elt, 'id) N.t0 

type ('elt, 'id) enumeration0 = 
  ('elt, 'id) N.enumeration0 
=
    End 
  | More of 'elt * ('elt, 'id) t0 * ('elt, 'id) enumeration0

type t = (elt, unit) t0
type enumeration = (elt,unit) enumeration0

let empty = N.empty0      
let isEmpty = N.isEmpty0
let singleton = N.singleton0
let min = N.min0
let max = N.max0
let iter = N.iter0      
let fold = N.fold0
let forAll = N.forAll0
let exists = N.exists0    
let filter = N.filter0
let partition = N.partition0
let cardinal = N.cardinal0
let elements = N.elements0 
let checkInvariant = N.checkInvariant
(* Insertion of one element *)

let rec add (x : elt) (t : t) : t =
  match N.toOpt t with 
    None -> N.(return @@ node ~left:empty ~key:x ~right:empty ~h:1)
  | Some nt (* Node(l, v, r, _) as t *) ->
    let v = N.key nt in  
    if x = v then t else
    if x < v then N.(bal (add x (left nt)) v (right nt))
    else N.(bal (left nt) v (add x (right nt)))



(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec splitAux (x : elt) (n : _ N.node) : t * bool * t =   
  let l,v,r = N.(left n , key n, right n) in  
  if x = v then (l, true, r)
  else if x < v then
    match N.toOpt l with 
    | None -> 
      N.(empty , false, return n)
    | Some l -> 
      let (ll, pres, rl) = splitAux x l in (ll, pres, N.join rl v r)
  else
    match N.toOpt r with 
    | None ->
      N.(return n, false, empty)
    | Some r -> 
      let (lr, pres, rr) = splitAux x r in (N.join l v lr, pres, rr)


let rec split (x : elt) (t : t) : t * bool *  t =
  match N.toOpt t with 
    None ->
    N.(empty, false, empty)
  | Some n (* Node(l, v, r, _)*) ->    
    splitAux x n 
      

let rec mem (x : elt) (t : t) =
  match N.toOpt t with 
  | None -> false
  | Some n (* Node(l, v, r, _) *) ->                
    let v = N.key n in 
    x = v || mem x N.(if x < v then (left n) else (right n))

let rec remove (x : elt) (t : t) : t = 
  match N.toOpt t with 
  | None -> t
  | Some n (* Node(l, v, r, _) *) ->
    let l,v,r = N.(left n, key n, right n) in 
    if x = v then N.merge l r else
    if x < v then N.bal (remove x l) v r 
    else N.bal l v (remove x r)

let rec union (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s2
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) *) ->    
    let h1, h2 = N.(h n1 , h n2) in             
    if h1 >= h2 then
      if h2 = 1 then add (N.key n2) s1 else begin
        let l1, v1, r1 = N.(left n1, key n1, right n1) in      
        let (l2, _, r2) = splitAux v1 n2 in
        N.join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add (N.key n1) s2 else begin
      let l2, v2, r2 = N.(left n2 , key n2, right n2) in 
      let (l1, _, r1) = splitAux v2 n1 in
      N.join (union l1 l2) v2 (union r1 r2)
    end

let rec inter (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s1
  | (_, None) -> s2 
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in  
    match splitAux v1 n2 with
      (l2, false, r2) ->
      N.concat (inter l1 l2) (inter r1 r2)
    | (l2, true, r2) ->
      N.join (inter l1 l2) v1 (inter r1 r2)

let rec diff (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
  | (None, _) 
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in
    match splitAux v1 n2 with
      (l2, false, r2) ->
      N.join (diff l1 l2) v1 (diff r1 r2)
    | (l2, true, r2) ->
      N.concat (diff l1 l2) (diff r1 r2)


let rec compare_aux e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    if (v1 : elt) <> v2
    then if v1 < v2 then -1 else 1
    else compare_aux (N.cons_enum r1 e1) (N.cons_enum r2 e2)

let cmp s1 s2 =
  compare_aux (N.cons_enum s1 End) (N.cons_enum s2 End)

let rec eq_aux e1 e2 =
  match (e1, e2) with
    (End, End) -> true
  | (End, More _)  -> false
  | (More _, End) -> false
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    (v1 : elt) = v2 &&
     eq_aux (N.cons_enum r1 e1) (N.cons_enum r2 e2)  

let eq s1 s2 = 
  eq_aux (N.cons_enum s1 End) (N.cons_enum s2 End)

let rec subset (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
    None, _ ->
    true
  | _, None ->
    false
  | Some t1, Some t2 (* Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) *) ->
    let l1,v1,r1 = N.(left t1, key t1, right t1) in  
    let l2,v2,r2 = N.(left t2, key t2, right t2) in 
    if (v1 : elt) = v2 then
      subset l1 l2 && subset r1 r2
    else if v1 < v2 then
      subset N.(return @@ node ~left:l1 ~key:v1 ~right:empty ~h:0) l2 && subset r1 s2
    else
      subset N.(return @@ node ~left:empty ~key:v1 ~right:r1 ~h:0) r2 && subset l1 s2


let rec findOpt (x : elt) (n :t)  = 
  match N.toOpt n with 
  | None -> None
  | Some t (* Node(l, v, r, _) *) ->    
    let v = N.key t in     
    if x = v then Some v
    else findOpt x N.(if x < v then (left t) else (right t))

let rec findAssert (x : elt) (n :t)  = 
  match N.toOpt n with 
  | None -> [%assert "Not_found"]
  | Some t (* Node(l, v, r, _) *) ->    
    let v = N.key t in     
    if x = v then Some v
    else findAssert x N.(if x < v then (left t) else (right t))
    
    
(* FIXME: use [sorted] attribute *)    
let ofArray (xs : elt array) : t =     
  let result = ref N.empty in 
  for i = 0 to Array.length xs - 1 do  
    result := add (Bs_Array.unsafe_get xs i) !result
  done ;
  !result 


(*
let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node (Empty, x0, Empty, 1), l
    | 2, x0 :: x1 :: l -> Node (Node(Empty, x0, Empty, 1), x1, Empty, 2), l
    | 3, x0 :: x1 :: x2 :: l ->
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2),l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)
*)  
(*
let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add x1 (singleton x0)
  | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
  | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq Pervasives.compare l)
*)
