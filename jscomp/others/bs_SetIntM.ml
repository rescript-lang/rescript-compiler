

module N = Bs_internalAVLset


module I = Bs_internalSetInt

type elt = I.elt
type t = I.t 


let empty  = N.empty0      
(* No value restriction ? *)
let isEmpty = N.isEmpty0
let singleton = N.singleton0
let min = N.min0
let max = N.max0
let iter = N.iter0      
let fold = N.fold0
let forAll = N.forAll0
let exists = N.exists0    
let filter = N.filter0
let partition = N.partition0
let length = N.length0
let elements = N.elements0 
let toArray = N.toArray0 
let checkInvariant = N.checkInvariant

let add = I.addMutate
let addArray = I.addArrayMutate

let remove = I.removeMutate
let ofArray = I.ofArray
let cmp = I.cmp 
let diff = I.diff
let eq = I.eq 
let findOpt = I.findOpt
let split = I.split 
let subset = I.subset 
let inter = I.inter 
let union = I.union
let mem = I.mem 
