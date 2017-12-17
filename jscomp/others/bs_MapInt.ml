# 4 "map.cppo.ml"
type key = int

# 9
module N = Bs_internalAVLtree

type ('key, 'a, 'id) t0 = ('key,'a,'id) N.t0 

type + 'a t = (key,'a, unit) N.t0 

type ('key, 'a, 'id) enumeration0 = ('key,'a,'id) N.enumeration0 =
    End 
  | More of 'key * 'a * ('key, 'a, 'id) t0 * ('key, 'a, 'id) enumeration0

type  'a enumeration = 
  (key,'a, unit) enumeration0



let empty = N.empty0      
let isEmpty = N.isEmpty0
let singleton = N.singleton0
let minBinding = N.minBinding0
let maxBinding = N.maxBinding0
let iter = N.iter0      
let map  = N.map0
let mapi = N.mapi0
let fold = N.fold0
let forAll = N.forAll0
let exists = N.exists0    
let filter = N.filter0
let partition = N.partition0
let cardinal = N.cardinal0
let bindings = N.bindings0  
let checkInvariant = N.checkInvariant

let rec add  (x : key) (data : _) t = 
  match N.toOpt t with
  | None -> 
    N.(return @@ node ~left:empty ~key:x ~value:data ~right:empty ~h:1)
  | Some n (* Node(l, v, d, r, h) *) ->
    let l,k,v,r = N.(left n, key n, value n, right n) in 
    if x = k then
      N.(return @@ node ~left:l ~key:x ~value:data ~right:r ~h:(h n))
    else if x < k then
      N.(bal (add x data l) k v r)
    else
      N.(bal l k v (add x data r))

let rec findOpt (x : key) n =
  match N.toOpt n with 
    None -> None
  | Some n  ->
    let v = N.key n in 
    if x = v then Some (N.value n)
    else findOpt x (if x < v then N.left n else N.right n)

let rec findAssert (x : key) n = 
  match N.toOpt n with 
  | None ->
    [%assert "Not_found"]
  | Some n  ->
    let v = N.key n in 
    if x = v then (N.value n)
    else findAssert x (if x < v then (N.left n) else (N.right n))

let rec findWithDefault ~def (x : key) n =
  match N.toOpt n with 
  | None -> def    
  | Some n -> 
    let v = N.key n in 
    if x = v then (N.value n)
    else findWithDefault ~def x (if x < v then (N.left n) else (N.right n))

let rec mem (x : key) n = 
  match N.toOpt n with 
    None -> false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = N.key n in 
    x = v || mem x (if x < v then N.left n else N.right n)

let rec remove (x : key) n = 
  match N.toOpt n with 
  |  None -> n    
  |  Some n ->
    let l,v,r = N.(left n, key n, right n) in 
    if x = v then
      N.merge l r
    else if x < v then
      N.(bal (remove x l) v (value n) r)
    else
      N.(bal l v (value n) (remove x r))

let rec splitAux (x : key) (n : _ N.node) : _ t0 * _ option  * _ t0 =  
  let l,v,d,r = N.(left n , key n, value n, right n) in  
  if x = v then (l, Some d, r)
  else     
    if x < v then
      match N.toOpt l with 
      | None -> 
        N.(empty , None, return n)
      | Some l -> 
        let (ll, pres, rl) = splitAux x l in (ll, pres, N.join rl v d r)
    else
      match N.toOpt r with 
      | None ->
        N.(return n, None, empty)
      | Some r -> 
        let (lr, pres, rr) = splitAux x r in (N.join l v d lr, pres, rr)
      

let rec split (x : key) n =
  match N.toOpt n with 
    None ->
    N.(empty, None, empty)
  | Some n -> 
    splitAux x n 

let rec merge f s1 s2 =
  match N.(toOpt s1, toOpt s2) with
    (None, None) -> N.empty
  | Some n (* (Node (l1, v1, d1, r1, h1), _)*), _ when N.(h n >= height s2) ->
    let (l1,v1,d1,r1) = N.(left n, key n, value n, right n ) in 
    let (l2, d2, r2) = split v1 s2 in
    N.concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2 [@bs]) (merge f r1 r2)
  | (_, Some n) (* Node (l2, v2, d2, r2, h2) *)  ->
    let (l2, v2, d2, r2) = N.(left n, key n, value n, right n) in 
    let (l1, d1, r1) = split v2 s1 in
    N.concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2) [@bs]) (merge f r1 r2)
  | _ ->
    assert false

let compare cmp m1 m2 =
  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      if (v1 : key) <> v2 then if v1 < v2 then -1 else 1 else
        let c = cmp d1 d2 [@bs] in
        if c <> 0 then c else
          compare_aux (N.cons_enum r1 e1) (N.cons_enum r2 e2)
  in compare_aux (N.cons_enum m1 End) (N.cons_enum m2 End)

let equal cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      (v1 : key) = v2  && cmp d1 d2 [@bs] &&
      equal_aux (N.cons_enum r1 e1) (N.cons_enum r2 e2)
  in equal_aux (N.cons_enum m1 End) (N.cons_enum m2 End)


let ofArray  (xs : _ array) : _ t0 =     
  let result = ref N.empty in 
  for i = 0 to Array.length xs - 1 do  
    let k, v = (Bs_Array.unsafe_get xs i) in 
    result := add  k v !result
  done ;
  !result 




