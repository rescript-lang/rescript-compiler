
module N = Bs_internalAVLset
module I = Bs_internalSet
module B = Bs_Bag 
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
  B.bag
    ~dict 
    ~data:N.empty0

let ofArray (type elt) (type id) (dict : (elt,id) Bs_Cmp.t) data = 
  let module M = (val dict ) in 
  B.bag
    ~dict 
    ~data:(ofArray0 ~cmp:M.cmp data)

let isEmpty m = N.isEmpty0 (B.data m)


let mem (type elt) (type id) e (m : (elt,id) t) = 
  let dict, data = B.(dict m, data m) in 
  let module M = (val dict) in 
  mem0 ~cmp:(M.cmp) e data

let add (type elt) (type id) (m : (elt,id) t) e =   
  let dict, data = B.(dict m, data m) in
  let module M = (val dict) in 
  B.bag ~dict 
    ~data:(add0 ~cmp:(M.cmp)  data e)

let singleton dict e =     
  B.bag ~dict
    ~data:(N.singleton0 e)


let remove (type elt) (type id) e (m : (elt,id) t) =      
  let dict, data = B.(dict m, data m) in   
  let module M = (val dict) in 
  B.bag ~dict ~data:(remove0 ~cmp:M.cmp e data)

let union (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let dict, mdata, ndata = B.(dict m, data m, data n) in   
  let module M = (val dict) in 
  B.bag ~data:(union0 ~cmp:M.cmp mdata ndata)
    ~dict


let inter (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let dict, mdata, ndata = B.(dict m, data m, data n) in 
  let module M = (val dict) in 
  B.bag ~data:(inter0 ~cmp:M.cmp mdata ndata)
    ~dict 


let diff (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let dict, mdata, ndata = B.(dict m, data m, data n) in 
  let module M = (val dict) in 
  B.bag ~dict ~data:(diff0 ~cmp:M.cmp mdata ndata )


let cmp (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let dict, mdata, ndata = B.(dict m, data m, data n) in 
  let module M = (val dict) in 
  cmp0 ~cmp:M.cmp mdata ndata

let eq (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let dict, mdata, ndata = B.(dict m, data m, data n) in 
  let module M = (val dict) in 
  eq0 ~cmp:M.cmp mdata ndata  

let subset (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let dict, mdata, ndata = B.(dict m, data m, data n) in 
  let module M = (val dict) in 
  subset0 ~cmp:M.cmp mdata ndata  

let iter m f  = N.iter0 (B.data m) f 

let fold m acc f = N.fold0 (B.data m) acc f

let forAll m f  = N.forAll0  (B.data m) f

let exists m f = N.exists0  (B.data m) f 

let filter m f  = 
  let data, dict = B.(data m, dict m) in 
  B.bag ~dict ~data:(N.filter0 data f )

let partition m f  = 
  let mdata, dict = B.(data m, dict m) in   
  let l,r = N.partition0 mdata f in   
  B.bag ~data:l ~dict, B.bag ~data:r ~dict

let length m = N.length0 (B.data m) 

let toList m = N.toList0 (B.data m)
let toArray m = N.toArray0 (B.data m)
let minOpt m = N.minOpt0 (B.data m)
let maxOpt m = N.maxOpt0 (B.data m)

let split (type elt) (type id) e (m : (elt,id) t) = 
  let dict, data = B.(dict m, data m) in  
  let module M = (val dict) in 
  let l, b, r = split0 ~cmp:M.cmp e data in 
  B.bag ~dict ~data:l,
  b,
  B.bag ~dict ~data:r

let findOpt (type elt) (type id) e (m : (elt,id) t) =   
  let dict, data = B.(dict m, data m) in   
  let module M = (val dict) in 
  findOpt0 ~cmp:M.cmp e data

let findAssert (type elt) (type id) e (m : (elt,id) t) =   
  let dict, data = B.(dict m, data m) in 
  let module M = (val dict) in 
  findAssert0 ~cmp:M.cmp e data


let ofSortedArrayUnsafe ~dict xs  =
  B.bag ~dict ~data:(N.ofSortedArrayUnsafe0 xs)









