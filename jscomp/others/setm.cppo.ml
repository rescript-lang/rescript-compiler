#ifdef TYPE_INT
module I = Bs_internalSetInt
#elif defined TYPE_STRING
module I = Bs_internalSetString
#else
  [%error "unknown type"]
#endif  
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
let minNull d =
  N.minNull0 (root d)
let maxOpt d = 
  N.maxOpt0 (root d)
let maxNull d =
  N.maxNull0 (root d)
let iter d f =
  N.iter0 (root d) f     
let fold d acc cb = 
  N.fold0 (root d) acc cb 
let forAll d p = 
  N.forAll0 (root d) p 
let exists d  p = 
  N.exists0 (root d) p   
let filter d p = 
  t ~root:(N.filter0 (root d) p )
let partition d p = 
  let a , b = N.partition0 (root d) p in 
  t ~root:a, t ~root:b
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
let split d  p =  
  let a,b,c =  I.split (root d) p  in 
  t ~root:a, b, t ~root:c
let subset a b = I.subset  (root a) (root b)
let inter a b  = t ~root:(I.inter (root a) (root b))
let union a b = t ~root:(I.union (root a) (root b))
let mem d x = I.mem (root d) x 
