#ifdef TYPE_INT
module I = Bs_internalSetInt
#elif defined TYPE_STRING
module I = Bs_internalSetString
#else
  [%error "unknown type"]
#endif  
module N = Bs_internalAVLset
module A = Bs_Array

type elt = I.elt
type t = I.t 


let empty = N.empty0      
let isEmpty = N.isEmpty0
let singleton = N.singleton0
let minimum = N.minOpt0
let minNull = N.minNull0
let maximum = N.maxOpt0
let maxNull = N.maxNull0
let forEach = N.iter0      
let fold = N.fold0
let forAll = N.forAll0
let exists = N.exists0    
let filter = N.filterShared0
let partition = N.partitionShared0
let size = N.length0
let toList = N.toList0
let toArray = N.toArray0
let ofSortedArrayUnsafe = N.ofSortedArrayUnsafe0
let checkInvariant = N.checkInvariant

let rec add  (t : t) (x : elt) : t =
  match N.toOpt t with 
    None -> N.singleton0 x 
  | Some nt  ->
    let v = N.key nt in  
    if x = v then t else
      let l, r = N.(left nt , right nt) in 
      if x < v then 
        let ll = add l x in 
        if ll == l then t 
        else N.bal ll v r
      else 
        let rr = add r x in 
        if rr == r then t
        else N.bal l v (add  r x)

let addArray h arr =   
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.unsafe_get arr i in 
    v := add !v  key 
  done ;
  !v 


let rec remove (t : t) (x : elt) : t = 
  match N.toOpt t with 
  | None -> t
  | Some n  ->
    let l,v,r = N.(left n, key n, right n) in 
    if x = v then 
      match N.toOpt l, N.toOpt r with 
      | None, _ -> r 
      | _, None -> l 
      | _, Some rn -> 
        let v = ref (N.key rn) in 
        let r = N.removeMinAuxWithRef rn v in 
        N.bal l !v r
    else
    if x < v then 
      let ll = remove l x in  
      if ll == l then t  
      else N.bal ll v r 
    else 
      let rr = remove r x in 
      if rr == r then t
      else N.bal l v rr
          
let removeArray h arr = 
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.unsafe_get arr i in 
    v := remove !v  key 
  done ;
  !v 
          
let ofArray = I.ofArray
let cmp = I.cmp 
let eq = I.eq 
let get = I.findOpt
let getNull = I.findNull
let getExn = I.findExn                
let subset = I.subset 
let has = I.mem 

let rec splitAuxNoPivot (n : _ N.node) (x : elt) : t * t =   
  let l,v,r = N.(left n , key n, right n) in  
  if x = v then l,  r
  else if x < v then
    match N.toOpt l with 
    | None -> 
      N.empty , N.return n
    | Some l -> 
      let ll,  rl = splitAuxNoPivot l x in 
      ll,  N.joinShared rl v r
  else
    match N.toOpt r with 
    | None ->
      N.return n,  N.empty
    | Some r -> 
      let lr,  rr = splitAuxNoPivot r x in
      N.joinShared l v lr,  rr


let rec splitAuxPivot (n : _ N.node) (x : elt) pres : t  * t =   
  let l,v,r = N.(left n , key n, right n) in  
  if x = v then begin 
    pres := true;  
    (l, r)
  end
  else if x < v then
    match N.toOpt l with 
    | None -> 
      N.empty, N.return n
    | Some l -> 
      let ll,  rl = splitAuxPivot l x pres in 
      ll,  N.joinShared rl v r
  else
    match N.toOpt r with 
    | None ->
      N.return n,  N.empty
    | Some r -> 
      let lr,  rr = splitAuxPivot r x pres in
      N.joinShared l v lr,  rr


let split  (t : t) (x : elt) =
  match N.toOpt t with 
    None ->
    (N.empty,  N.empty), false
  | Some n  ->    
    let pres = ref false in 
    let v = splitAuxPivot n  x pres  in 
    v, !pres

let rec union (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s2
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) *) ->    
    let h1, h2 = N.(h n1 , h n2) in             
    if h1 >= h2 then
      if h2 = 1 then add  s1 (N.key n2) else begin
        let l1, v1, r1 = N.(left n1, key n1, right n1) in      
        let (l2,  r2) = splitAuxNoPivot n2 v1 in
        N.joinShared (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add  s2 (N.key n1) else begin
      let l2, v2, r2 = N.(left n2 , key n2, right n2) in 
      let (l1, r1) = splitAuxNoPivot n1 v2 in
      N.joinShared (union l1 l2) v2 (union r1 r2)
    end

let rec inter (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
    (None, _) 
  | (_, None) -> N.empty
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in  
    let pres = ref false in 
    let l2,r2 =  splitAuxPivot n2 v1 pres in 
    let ll = inter l1 l2 in 
    let rr = inter r1 r2 in 
    if !pres then N.joinShared ll v1 rr 
    else N.concatShared ll rr 

let rec diff (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
  | (None, _) 
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in
    let pres = ref false in 
    let l2, r2 = splitAuxPivot  n2 v1 pres in 
    let ll = diff  l1 l2 in 
    let rr = diff  r1 r2 in 
    if !pres then N.concatShared ll rr 
    else N.joinShared ll v1 rr 


    

