
module N = Bs_internalAVLset
module I = Bs_internalSet
module B = Bs_BagM
type ('k,'id) t0 = ('k,'id) I.t0 

type ('elt,'id) t = (('elt,'id) Bs_Cmp.t , ('elt,'id) t0) B.bag  

let empty0 = N.empty0      
let ofArray0 = I.ofArray0
let isEmpty0 = N.isEmpty0
let mem0 = I.mem0
let add0 = I.add0
let singleton0 = N.singleton0
let remove0 = I.remove0
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
let isEmpty m =   
  N.isEmpty0 (B.data m)
let singleton dict x =   
  B.bag ~dict ~data:(N.singleton0 x)
let minOpt d =  
  N.minOpt0 (B.data d)
let maxOpt d =   
  N.maxOpt0 (B.data d)
let minNull d =  
  N.minNull0 (B.data d)
let maxNull d =   
  N.maxNull0 (B.data d)



let add (type elt) (type id) (m : (elt,id) t) e =  
  let dict, oldRoot = B.(dict m, data m) in 
  let module M = (val dict) in 
  let newRoot = I.addMutate ~cmp:M.cmp oldRoot e  in 
  if newRoot != oldRoot then 
    B.dataSet m newRoot;
  m  