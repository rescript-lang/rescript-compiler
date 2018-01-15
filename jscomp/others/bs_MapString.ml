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
let minKVOpt = N.minKVOpt0
let minKVNull = N.minKVNull0
let maxKVOpt = N.maxKVOpt0
let maxKVNull = N.maxKVNull0
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
let checkInvariant = N.checkInvariant

let rec add  t (x : key) (data : _)  = 
  match N.toOpt t with
  | None -> 
    N.singleton0 x data 
  | Some n  ->
    let k = N.key n in 
    if x = k then
      N.updateKV n x data 
    else
      let v = N.value n in 
      if x < k then
        N.bal (add (N.left n) x data ) k v (N.right n)
      else
        N.bal (N.left n) k v (add (N.right n) x data )
        
let rec remove n (x : key) = 
  match N.toOpt n with 
  |  None -> n    
  |  Some n ->
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
      N.(bal (remove l x ) v (value n) r)
    else
      N.(bal l v (value n) (remove r x ))

let mem = I.mem 
let cmp = I.cmp 
let eq = I.eq 
let findOpt = I.findOpt
let findNull = I.findNull 
let findWithDefault = I.findWithDefault 
let split = I.split 
let merge = I.merge 
let ofArray = I.ofArray