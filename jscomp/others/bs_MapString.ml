# 2 "map.cppo.ml"
type key = string
module I = Bs_internalMapString

# 11
module N = Bs_internalAVLtree
module A = Bs_Array 
module S = Bs_Sort

type 'a t = (key, 'a) N.t0

let empty = N.empty0      
let isEmpty = N.isEmpty0
let singleton = N.singleton0

let minKeyOpt = N.minKeyOpt0
let minKeyNull = N.minKeyNull0
let maxKeyOpt = N.maxKeyOpt0
let maxKeyNull = N.maxKeyNull0                   
let minimum = N.minKVOpt0
let minNull = N.minKVNull0
let maximum = N.maxKVOpt0
let maxNull = N.maxKVNull0
let forEach = N.iter0      
let map  = N.map0
let mapi = N.mapi0
let fold = N.fold0
let every = N.every0
let some = N.some0    
let keepBy = N.filterShared0
let partition = N.partitionShared0
let size = N.length0
let toList = N.toList0
let toArray = N.toArray0 
let keysToArray = N.keysToArray0
let valuesToArray = N.valuesToArray0 
let checkInvariant = N.checkInvariant

let rec set  t (newK : key) (newD : _)  = 
  match N.toOpt t with
  | None -> 
    N.singleton0 newK newD
  | Some n  ->
    let k = N.key n in 
    if newK = k then
      N.return (N.updateValue n newD)
    else
      let v = N.value n in 
      if newK < k then
        N.bal (set (N.left n) newK newD) k v (N.right n)
      else
        N.bal (N.left n) k v (set (N.right n) newK newD)
        
let rec update  t (x : key) f  = 
  match N.toOpt t with
  | None -> 
    begin match f None [@bs] with 
    | None -> t 
    | Some data -> 
      N.singleton0 x data 
    end 
  | Some n  ->
    let k = N.key n in 
    if x = k then
      begin match f (Some (N.value n)) [@bs] with 
      | None ->
        let l, r = N.left n, N.right n in
        begin match N.toOpt l, N.toOpt r with
          | None, _ -> r
          | _, None -> l
          | _, Some rn ->
            let kr, vr = ref (N.key rn), ref (N.value rn) in
            let r = N.removeMinAuxWithRef rn kr vr in
            N.bal l !kr !vr r 
        end
      | Some data -> N.return (N.updateValue n data )
      end 
    else
      let l,r,v = N.left n, N.right n , N.value n in 
      if x < k then
        let ll = (update l x f) in
        if l == ll then t 
        else N.bal ll  k v r
      else
        let rr = (update r x f) in
        if r == rr then t 
        else N.bal l k v rr 

let rec removeAux n (x : key) = 
    let l,v,r = N.(left n, key n, right n) in 
    if x = v then
      match N.toOpt l, N.toOpt r with
      | None, _ -> r 
      | _, None -> l 
      | _, Some rn -> 
        let kr, vr = ref (N.key rn), ref (N.value rn) in 
        let r = N.removeMinAuxWithRef rn kr vr in 
        N.bal l !kr !vr r 
    else if x < v then
      match N.toOpt l with 
      | None -> N.return n
      | Some left -> 
        let ll = removeAux left x in 
        if ll == l then N.return n 
        else N.(bal ll v (value n) r)
    else
      match N.toOpt r with 
      | None -> N.return n 
      | Some right -> 
        let rr = removeAux right x  in 
        N.bal l v (N.value n) rr

let remove n x = 
  match N.toOpt n with 
  | None -> N.empty0
  | Some n -> removeAux n x 

let rec removeArrayAux t xs i len  =
  if i < len then
    let ele = A.getUnsafe xs i in
    let u =  removeAux t ele  in
    match N.toOpt u with
    | None -> u
    | Some t -> removeArrayAux t xs (i + 1) len
  else
    N.return t
      
let removeArray t keys =
  let len = A.length keys in
  match N.toOpt t with
  | None -> N.empty0
  | Some t ->  removeArrayAux t keys 0 len

let mergeArray h arr =   
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key,value = A.getUnsafe arr i in 
    v := set !v key value
  done ;
  !v 

let has = I.mem 
let cmp = I.cmp 
let eq = I.eq 
let get = I.findOpt
let getNull = I.findNull 
let getWithDefault = I.findWithDefault 
let getExn = I.findExn
let split = I.split 
let merge = I.merge 
let ofArray = I.ofArray
