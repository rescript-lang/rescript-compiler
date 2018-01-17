#ifdef TYPE_STRING
type key = string
module I = Bs_internalMapString
#elif defined TYPE_INT
type key = int
module I = Bs_internalMapInt    
#else 
  [%error "unknown type"]
#endif 

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
let minKeyValueOpt = N.minKVOpt0
let minKeyValueNull = N.minKVNull0
let maxKeyValueOpt = N.maxKVOpt0
let maxKeyValueNull = N.maxKVNull0
let iter = N.iter0      
let map  = N.map0
let mapi = N.mapi0
let fold = N.fold0
let forAll = N.forAll0
let exists = N.exists0    
let filter = N.filterShared0
let partition = N.partitionShared0
let length = N.length0
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
      N.updateKV n newK newD
    else
      let v = N.value n in 
      if newK < k then
        N.bal (set (N.left n) newK newD) k v (N.right n)
      else
        N.bal (N.left n) k v (set (N.right n) newK newD)
        
let rec setWithOpt  t (x : key) f  = 
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
      begin match f (Some k) [@bs] with 
      | None -> t 
      | Some data -> N.updateKV n x data 
      end 
    else
      let v = N.value n in 
      if x < k then
        N.bal (setWithOpt (N.left n) x f) k v (N.right n)
      else
        N.bal (N.left n) k v (setWithOpt (N.right n) x f)        

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
    let ele = A.unsafe_get xs i in
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

let mem = I.mem 
let cmp = I.cmp 
let eq = I.eq 
let get = I.findOpt
let getNull = I.findNull 
let getWithDefault = I.findWithDefault 
let getExn = I.findExn
let split = I.split 
let merge = I.merge 
let ofArray = I.ofArray
