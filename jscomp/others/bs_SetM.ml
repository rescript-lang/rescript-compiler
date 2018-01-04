
module N = Bs_internalAVLset
module I = Bs_internalSet
module B = Bs_BagM
type ('k,'id) t0 = ('k,'id) I.t0 

type ('elt,'id) t = (('elt,'id) Bs_Cmp.t , ('elt,'id) t0) B.bag  

let empty0 = N.empty0      
let ofArray0 = I.ofArray0
let isEmpty0 = N.isEmpty0
let mem0 = I.mem0
let add0 = I.addMutate
let singleton0 = N.singleton0
let remove0 = I.removeMutate
let union0 = I.union0 
let inter0 = I.inter0
let diff0 = I.diff0
let subset0 = I.subset0
let cmp0 = I.cmp0
let eq0 = I.eq0 
let iter0 = N.iter0      
let fold0 = N.fold0
let forAll0 = N.forAll0
let exists0 = N.exists0    
let filter0 = N.filter0
let partition0 = N.partition0
let length0 = N.length0
let toList0 = N.toList0
let toArray0 = N.toArray0
let minOpt0 = N.minOpt0
let maxOpt0 = N.maxOpt0
let split0 = I.split0
let ofSortedArrayUnsafe0 = N.ofSortedArrayUnsafe0
let findOpt0 = I.findOpt0
let findAssert0 = I.findAssert0

let empty dict = 
  B.bag ~dict ~data:N.empty0
let isEmpty d = 
  N.isEmpty0 (B.data d)
let singleton dict x = 
  B.bag ~data:(N.singleton0 x) ~dict 
let minOpt d = 
  N.minOpt0 (B.data d)
let minNull d =
  N.minNull0 (B.data d)
let maxOpt d = 
  N.maxOpt0 (B.data d)
let maxNull d =
  N.maxNull0 (B.data d)
let iter d f =
  N.iter0 (B.data d) f     
let fold d acc cb = 
  N.fold0 (B.data d) acc cb 
let forAll d p = 
  N.forAll0 (B.data d) p 
let exists d  p = 
  N.exists0 (B.data d) p   
let filter d p = 
  let data, dict = B.(data d, dict d) in  
  B.bag ~data:(N.filter0 data p ) ~dict 
let partition d p = 
  let data, dict = B.(data d, dict d) in 
  let a , b = N.partition0 data p in 
  B.bag ~data:a ~dict, B.bag ~data:b ~dict
let length d = 
  N.length0 (B.data d)
let toList d =
  N.toList0 (B.data d)
let toArray d = 
  N.toArray0 (B.data d)
let ofSortedArrayUnsafe ~dict xs : _ t =
  B.bag ~data:(N.ofSortedArrayUnsafe0 xs) ~dict   
let checkInvariant d = 
  N.checkInvariant (B.data d)


let addOnly (type elt) (type id) (m : (elt,id) t) e = 
  let dict, oldRoot = B.(dict m, data m) in 
  let module M = (val dict) in 
  let newRoot = I.addMutate ~cmp:M.cmp oldRoot e  in 
  if newRoot != oldRoot then 
    B.dataSet m newRoot

let add m e = 
  addOnly m e;
  m

(* TODO: addArray *)  

let removeOnly (type elt) (type id) (d : (elt,id) t) v =  
  let dict, oldRoot = B.(dict d, data d) in 
  let module M = (val dict) in 
  let newRoot = I.removeMutate ~cmp:M.cmp oldRoot v in 
  if newRoot != oldRoot then 
    B.dataSet d newRoot

let remove d v =     
  removeOnly d v; 
  d 

let cmp (type elt) (type id) (d0 : (elt,id) t) d1 = 
  let dict = B.dict d0 in 
  let module M = (val dict) in 
  I.cmp0 ~cmp:M.cmp (B.data d0) (B.data d1)

let diff (type elt) (type id) (d0 : (elt,id) t) d1 = 
  let dict = B.dict d0 in 
  let module M = (val dict) in 
  B.bag 
  ~data:(I.diff0 ~cmp:M.cmp (B.data d0) (B.data d1))
  ~dict

let eq (type elt) (type id) (d0 : (elt,id) t)  d1 = 
  let dict = B.dict d0 in 
  let module M = (val dict) in 
  I.eq0 ~cmp:M.cmp (B.data d0) (B.data d1)

let findOpt (type elt) (type id) (d : (elt,id) t) x = 
  let dict = B.dict d in 
  let module M = (val dict) in 
  I.findOpt0 ~cmp:M.cmp (B.data d) x 

let findAssert (type elt) (type id) (d : (elt,id) t) x = 
  let dict = B.dict d in 
  let module M = (val dict) in 
  I.findAssert0 ~cmp:M.cmp (B.data d) x 
  
let ofArray (type elt) (type id) (dict : (elt,id) Bs_Cmp.t) data =  
  let module M = (val dict) in 
  B.bag ~dict ~data:(I.ofArray0 ~cmp:M.cmp data)

let split (type elt) (type id) (d : (elt,id) t)  p : _ t * bool * _ t =  
  let dict = B.dict d in 
  let module M = (val dict) in 
  let a,b,c =  I.split0 ~cmp:M.cmp (B.data d) p  in 
  B.bag ~data:a ~dict, b, B.bag ~data:c ~dict 

let subset (type elt) (type id) (a : (elt,id) t) b = 
  let dict = B.dict a in 
  let module M = (val dict) in 
  I.subset0  ~cmp:M.cmp (B.data a) (B.data b)

let inter (type elt) (type id) (a : (elt,id) t) b  = 
  let dict = B.dict a in 
  let module M = (val dict) in 
  B.bag ~data:(I.inter0 ~cmp:M.cmp (B.data a) (B.data b))
  ~dict

let union (type elt) (type id) (a : (elt,id) t) b = 
  let dict = B.dict a in 
  let module M = (val dict) in 
  B.bag 
  ~data:(I.union0 ~cmp:M.cmp (B.data a) (B.data b)) 
  ~dict

let mem (type elt) (type id) (d : (elt,id) t) x =
  let dict = B.dict d in 
  let module M = (val dict) in 
  I.mem0 ~cmp:M.cmp (B.data d) x 
