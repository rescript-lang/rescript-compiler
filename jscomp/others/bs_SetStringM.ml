# 4 "setm.cppo.ml"
module I = Bs_internalSetString
# 8
module N = Bs_internalAVLset


type elt = I.elt


type t = {
  mutable root : I.t
} [@@bs.deriving abstract]

let empty  () = t ~root:N.empty0 
let isEmpty d = 
  N.isEmpty0 (root d)
let singleton x = 
  t ~root:(N.singleton0 x)
let minOpt d = 
  N.minOpt0 (root d)
let maxOpt d = 
  N.maxOpt0 (root d)
let iter d =
  N.iter0 (root d)     
let fold d = 
  N.fold0 (root d)
let forAll d = 
  N.forAll0 (root d)
let exists d = 
  N.exists0 (root d)   
let filter d = 
  N.filter0 (root d)
let partition d = 
  N.partition0 (root d)
let length d = 
  N.length0 (root d)
let toList d =
  N.toList0 (root d)
let toArray d = 
  N.toArray0 (root d)
let checkInvariant d = 
  N.checkInvariant (root d)

let add d k = 
  let old_root = root d in 
  let v = I.addMutate old_root k in 
  if v != old_root then 
    rootSet d v ;
  d 
let addOnly d k = 
  let old_root = root d in 
  let v = I.addMutate old_root k in 
  if v != old_root then 
    rootSet d v 

let addArrayOnly d arr = 
  let old_root = root d in 
  let v = I.addArrayMutate old_root arr in 
  if v != old_root then 
    rootSet d v 
    
let addArray d arr = 
  let old_root = root d in 
  let v = I.addArrayMutate old_root arr in 
  if v != old_root then 
    rootSet d v ;
  d   

let remove d v = 
  let old_root = root d in 
  let v = I.removeMutate old_root v in 
  if v != old_root then 
    rootSet d v ;
  d 
let removeOnly d v = 
  let old_root = root d in 
  let v = I.removeMutate old_root v in 
  if v != old_root then 
    rootSet d v 
  
let ofArray xs = 
  t  ~root:(I.ofArray xs)
let cmp d0 d1 = 
  I.cmp (root d0) (root d1)
let diff d0 d1 = 
  t ~root:(I.diff (root d0) (root d1))
let eq d0 d1 = 
  I.eq (root d0) (root d1)
let findOpt d x = 
  I.findOpt (root d) x 
let split = I.split 
let subset = I.subset 
let inter = I.inter 
let union = I.union
let mem d x = I.mem (root d) x 
