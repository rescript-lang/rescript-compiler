
module N = Bs_internalAVLset
module I = Bs_internalSet

type ('k,'id) t = {
  dict : ('k,'id) Bs_Cmp.t ; 
  mutable root : ('k,'id) N.t0
} [@@bs.deriving abstract]


let add (type elt) (type id) (m : (elt,id) t) e =  
  let dict, oldRoot = dict m, root m in 
  let module M = (val dict) in 
  let newRoot = I.addMutate ~cmp:M.cmp oldRoot e  in 
  if newRoot != oldRoot then 
    rootSet m newRoot;
  m  