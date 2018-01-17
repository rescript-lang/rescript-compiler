# 6 "mapm.cppo.ml"
module I = Bs_internalMapString
module S = Bs_SortString
type key = string
# 12
module N = Bs_internalAVLtree
module A = Bs_Array 




type 'a t = {
  mutable data : 'a I.t
} [@@bs.deriving abstract]


let empty () = t ~data:N.empty0      
let isEmpty m = N.isEmpty0 (data m)
let singleton k v = t ~data:(N.singleton0 k v)

let minKeyNull m = N.minKeyNull0 (data m)
let minKeyOpt m = N.minKeyOpt0 (data m)
let maxKeyNull m = N.maxKeyNull0 (data m)
let maxKeyOpt m = N.maxKeyOpt0 (data m)
let minKeyValueOpt m = N.minKVOpt0 (data m)
let minKeyValueNull m = N.minKVNull0 (data m)
let maxKeyValueOpt m = N.maxKVOpt0 (data m)
let maxKeyValueNull m = N.maxKVNull0 (data m)

let addOnly (m : _ t) k v = 
  let old_data = data m in 
  let v = I.addMutate old_data k v in 
  if v != old_data then 
    dataSet m v 

let add (d : 'a t) (k : key) (v : 'a) : 'a t=  
  addOnly d k v; 
  d
let iter d f = N.iter0 (data d) f     
let map d f = t ~data:(N.map0 (data d) f)
let mapi d f = t ~data:(N.mapi0 (data d) f) 
let fold d acc f  = N.fold0 (data d) acc f 
let forAll d f = N.forAll0 (data d) f 
let exists d f = N.exists0 (data d) f    

let length d = N.length0 (data d)
let toList d = N.toList0 (data d)
let toArray d = N.toArray0 (data d)
let keysToArray d = N.keysToArray0 (data d)
let valuesToArray d = N.valuesToArray0 (data d)
let checkInvariant d = N.checkInvariant (data d)
let mem d v = I.mem (data d) v 


let rec removeMutateAux nt (x : key)= 
  let k = N.key nt in 
  if x = k then 
    let l,r = N.(left nt, right nt) in       
    match N.(toOpt l, toOpt r) with 
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
      N.return (N.balMutate nt)
    | None, Some _ ->
      r  
    | (Some _ | None ), None ->  l 
  else 
    begin 
      if x < k then 
        match N.toOpt (N.left nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (removeMutateAux l x );
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.right nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeMutateAux r x);
          N.return (N.balMutate nt)
    end
let removeMutate nt x = 
  match N.toOpt nt with 
  | None -> nt 
  | Some nt -> removeMutateAux nt x 

let removeOnly d v = 
  let old_data = data d in 
  let v = removeMutate old_data v in 
  if v != old_data then 
    dataSet d v   

let remove d v = 
  removeOnly d v; 
  d 
  

let cmp = I.cmp 
let eq = I.eq 


(* let split = I.split  *)
(* let merge = I.merge  *)


let ofArray xs = 
  t  ~data:(I.ofArray xs)

let cmp d0 d1 = 
  I.cmp (data d0) (data d1)
let eq d0 d1 = 
  I.eq (data d0) (data d1)
let findOpt d x = 
  I.findOpt (data d) x 
let findNull d x = I.findNull (data d) x 
let findWithDefault d x def = I.findWithDefault (data d) x def  
let findExn d x = I.findExn (data d) x 
