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
  mutable data : I.t
} [@@bs.deriving abstract]

let empty  () = t ~data:N.empty0 
let isEmpty d = 
  N.isEmpty0 (data d)
let singleton x = 
  t ~data:(N.singleton0 x)
let minOpt d = 
  N.minOpt0 (data d)
let minNull d =
  N.minNull0 (data d)
let maxOpt d = 
  N.maxOpt0 (data d)
let maxNull d =
  N.maxNull0 (data d)
let iter d f =
  N.iter0 (data d) f     
let fold d acc cb = 
  N.fold0 (data d) acc cb 
let forAll d p = 
  N.forAll0 (data d) p 
let exists d  p = 
  N.exists0 (data d) p   
let filter d p = 
  t ~data:(N.filter0 (data d) p )
let partition d p = 
  let a , b = N.partition0 (data d) p in 
  t ~data:a, t ~data:b
let length d = 
  N.length0 (data d)
let toList d =
  N.toList0 (data d)
let toArray d = 
  N.toArray0 (data d)
let ofSortedArrayUnsafe xs =
  t ~data:(N.ofSortedArrayUnsafe0 xs)    
let checkInvariant d = 
  N.checkInvariant (data d)

let addOnly d k = 
  let old_data = data d in 
  let v = I.addMutate old_data k in 
  if v != old_data then 
    dataSet d v   

let add d k = 
  addOnly d k;
  d 


let addArrayOnly d arr = 
  let old_data = data d in 
  let v = I.addArrayMutate old_data arr in 
  if v != old_data then 
    dataSet d v 
    
let addArray d arr = 
  let old_data = data d in 
  let v = I.addArrayMutate old_data arr in 
  if v != old_data then 
    dataSet d v ;
  d   

let removeOnly d v = 
  let old_data = data d in 
  let v = I.removeMutate old_data v in 
  if v != old_data then 
    dataSet d v   

let remove d v = 
  removeOnly d v; 
  d 

  
let ofArray xs = 
  t  ~data:(I.ofArray xs)

let cmp d0 d1 = 
  I.cmp (data d0) (data d1)
let diff d0 d1 = 
  t ~data:(I.diff (data d0) (data d1))
let eq d0 d1 = 
  I.eq (data d0) (data d1)
let findOpt d x = 
  I.findOpt (data d) x 
let split d  p =  
  let a,b,c =  I.split (data d) p  in 
  t ~data:a, b, t ~data:c
let subset a b = I.subset  (data a) (data b)
let inter a b  = t ~data:(I.inter (data a) (data b))
let union a b = t ~data:(I.union (data a) (data b))
let mem d x = I.mem (data d) x 
