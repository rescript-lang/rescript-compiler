#ifdef TYPE_INT
module I = Bs_internalSetInt
#elif defined TYPE_STRING
module I = Bs_internalSetString
#else
  [%error "unknown type"]
#endif  
module N = Bs_internalAVLset
module A = Bs_Array 

type elt = I.elt


type t = {
  mutable data : I.t
} [@@bs.deriving abstract]



let rec removeMutateAux nt (x : elt)= 
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

let addArrayMutate t  xs =       
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    v := I.addMutate !v (A.unsafe_get xs i)
  done ;
  !v    




  
let removeMutate nt x = 
  match N.toOpt nt with 
  | None -> nt 
  | Some nt -> removeMutateAux nt x 



    
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
  let v = addArrayMutate old_data arr in 
  if v != old_data then 
    dataSet d v 
    
let addArray d arr = 
  let old_data = data d in 
  let v = addArrayMutate old_data arr in 
  if v != old_data then 
    dataSet d v ;
  d   

let removeOnly d v = 
  let old_data = data d in 
  let v = removeMutate old_data v in 
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
