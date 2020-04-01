# 4 "others/belt_Set.cppo.ml"
module I = Belt_internalSetString
# 8 "others/belt_Set.cppo.ml"
module N = Belt_internalAVLset
module A = Belt_Array

type value = I.value
type t = I.t 


let empty = None
let isEmpty = N.isEmpty
let minimum = N.minimum
let minUndefined = N.minUndefined
let maximum = N.maximum
let maxUndefined = N.maxUndefined

let forEach = N.forEach
let forEachU = N.forEachU
let reduce = N.reduce
let reduceU = N.reduceU
let every = N.every
let everyU = N.everyU
let some = N.some                
let someU = N.someU
let keep = N.keepShared              
let keepU = N.keepSharedU
let partition = N.partitionShared
let partitionU = N.partitionSharedU
                   
let size = N.size
let toList = N.toList
let toArray = N.toArray
let fromSortedArrayUnsafe = N.fromSortedArrayUnsafe
let checkInvariantInternal = N.checkInvariantInternal

let rec add  (t : t) (x : value) : t =
  match t with 
    None -> N.singleton x 
  | Some nt  ->
    let v = nt.value in  
    if x = v then t else
      let {N.left = l; right =  r} = nt in 
      if x < v then 
        let ll = add l x in 
        if ll == l then t 
        else N.bal ll v r
      else 
        let rr = add r x in 
        if rr == r then t
        else N.bal l v rr

let mergeMany h arr =   
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.getUnsafe arr i in 
    v .contents<- add v.contents  key 
  done ;
  v.contents 


let rec remove (t : t) (x : value) : t = 
  match t with 
  | None -> t
  | Some n  ->
    let {N.left = l; value = v; right = r} = n in 
    if x = v then 
      match l, r with 
      | None, _ -> r 
      | _, None -> l 
      | _, Some rn -> 
        let v = ref rn.value in 
        let r = N.removeMinAuxWithRef rn v in 
        N.bal l v.contents r
    else
    if x < v then 
      let ll = remove l x in  
      if ll == l then t  
      else N.bal ll v r 
    else 
      let rr = remove r x in 
      if rr == r then t
      else N.bal l v rr
          
let removeMany h arr = 
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.getUnsafe arr i in 
    v .contents<- remove v.contents  key 
  done ;
  v.contents 
          
let fromArray = I.fromArray
let cmp = I.cmp 
let eq = I.eq 
let get = I.get
let getUndefined = I.getUndefined
let getExn = I.getExn                
let subset = I.subset 
let has = I.has

let rec splitAuxNoPivot (n : _ N.node) (x : value) : t * t =   
  let {N.left = l; value = v; right = r} = n in  
  if x = v then l,  r
  else if x < v then
    match l with 
    | None -> 
      None , Some n
    | Some l -> 
      let ll,  rl = splitAuxNoPivot l x in 
      ll,  N.joinShared rl v r
  else
    match r with 
    | None ->
      Some n,  None
    | Some r -> 
      let lr,  rr = splitAuxNoPivot r x in
      N.joinShared l v lr,  rr


let rec splitAuxPivot (n : _ N.node) (x : value) pres : t  * t =   
  let {N.left = l; value = v; right = r} = n in  
  if x = v then begin 
    pres .contents<- true;
    (l, r)
  end
  else if x < v then
    match l with 
    | None -> 
      None, Some n
    | Some l -> 
      let ll,  rl = splitAuxPivot l x pres in 
      ll,  N.joinShared rl v r
  else
    match r with 
    | None ->
      Some n,  None
    | Some r -> 
      let lr,  rr = splitAuxPivot r x pres in
      N.joinShared l v lr,  rr


let split  (t : t) (x : value) =
  match t with 
    None ->
    (None,  None), false
  | Some n  ->    
    let pres = ref false in 
    let v = splitAuxPivot n  x pres  in 
    v, pres.contents

let rec union (s1 : t) (s2 : t) =
  match s1, s2 with
    (None, _) -> s2
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) *) ->    
    let h1, h2 = n1.height, n2.height in             
    if h1 >= h2 then
      if h2 = 1 then add  s1 n2.value else begin
        let {N.left = l1; value = v1; right = r1} = n1 in      
        let (l2,  r2) = splitAuxNoPivot n2 v1 in
        N.joinShared (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add  s2 n1.value else begin
      let {N.left = l2; value = v2; right = r2} = n2 in 
      let (l1, r1) = splitAuxNoPivot n1 v2 in
      N.joinShared (union l1 l2) v2 (union r1 r2)
    end

let  rec intersect (s1 : t) (s2 : t) =
  match s1, s2 with
    (None, _) 
  | (_, None) -> None
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let {N.left = l1; value = v1; right  = r1} = n1 in  
    let pres = ref false in 
    let l2,r2 =  splitAuxPivot n2 v1 pres in 
    let ll = intersect l1 l2 in 
    let rr = intersect r1 r2 in 
    if pres.contents then N.joinShared ll v1 rr 
    else N.concatShared ll rr 

let rec diff (s1 : t) (s2 : t) =
  match s1, s2 with
  | (None, _) 
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let {N.left = l1; value = v1; right = r1} = n1 in
    let pres = ref false in 
    let l2, r2 = splitAuxPivot  n2 v1 pres in 
    let ll = diff  l1 l2 in 
    let rr = diff  r1 r2 in 
    if pres.contents then N.concatShared ll rr 
    else N.joinShared ll v1 rr 


    

