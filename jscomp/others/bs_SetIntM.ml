# 2 "setm.cppo.ml"
module I = Bs_internalSetInt
module S = Bs_SortInt
# 10
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
  t ~data:(N.filterCopy (data d) p )
let partition d p = 
  let a , b = N.partitionCopy (data d) p in 
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
let eq d0 d1 = 
  I.eq (data d0) (data d1)
let findOpt d x = 
  I.findOpt (data d) x 
let split d  key =  
  let s = data d in  
  let arr = N.toArray0 s in 
  let i = S.binSearch arr key   in   
  let len = A.length arr in 
  if i < 0 then 
    let next = - i -1 in 
    (t
      ~data:(N.ofSortedArrayAux arr 0 next)
    , 
    t
      ~data:(N.ofSortedArrayAux arr next (len - next))
    ), false
  else 
    (t
      ~data:(N.ofSortedArrayAux arr 0 i)
    ,
    t
      ~data:(N.ofSortedArrayAux arr (i+1) (len - i - 1))
      ), true   
  
let subset a b = I.subset  (data a) (data b)
let inter dataa datab  = 
  let dataa, datab = data dataa, data datab in
    match N.toOpt dataa, N.toOpt datab with 
    | None, _ -> empty ()
    | _, None -> empty ()
    | Some dataa0, Some datab0 ->  
    let sizea, sizeb = 
        N.lengthNode dataa0, N.lengthNode datab0 in          
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    (* let p = Bs_Cmp.getCmp M.cmp in  *)
    if ((A.unsafe_get tmp (sizea - 1) < 
        A.unsafe_get tmp sizea))
      || 
      (
      (A.unsafe_get tmp (totalSize - 1) <
      A.unsafe_get tmp 0)
      )
       then empty ()
    else 
    let tmp2 = A.makeUninitializedUnsafe (min sizea sizeb) in 
    let k = S.inter tmp 0 sizea tmp sizea sizeb tmp2 0  in 
    t ~data:(N.ofSortedArrayAux tmp2 0 k)
  
let diff dataa datab : t = 
  let dataa, datab = data dataa, data datab in
  match N.toOpt dataa, N.toOpt datab with 
  | None, _ -> empty ()
  | _, None -> t ~data:(N.copy dataa)
  | Some dataa0, Some datab0 -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in  
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    (* let p = Bs_Cmp.getCmp M.cmp in  *)
    if ( (A.unsafe_get tmp (sizea - 1)) < 
        (A.unsafe_get tmp sizea))
      ||       
      (A.unsafe_get tmp (totalSize - 1)
      < A.unsafe_get tmp 0) 
       then t ~data:(N.copy dataa) 
    else 
    let tmp2 = A.makeUninitializedUnsafe sizea in 
    let k = S.diff tmp 0 sizea tmp sizea sizeb tmp2 0  in 
    t ~data:(N.ofSortedArrayAux tmp2 0 k)

let union (dataa : t)  (datab : t) : t = 
  let dataa, datab = data dataa, data datab in
   match N.toOpt dataa, N.toOpt datab with 
  | None, _ -> t ~data:(N.copy datab) 
  | _, None -> t ~data:(N.copy dataa) 
  | Some dataa0, Some datab0 
    -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in 
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ;
    ignore @@ N.fillArray datab0 sizea tmp ;
    (* let p = (Bs_Cmp.getCmp M.cmp)  in  *)
    if 
      (A.unsafe_get tmp (sizea - 1) < 
      A.unsafe_get tmp sizea)  then 
      t  ~data:(N.ofSortedArrayAux tmp 0 totalSize) 
    else   
      let tmp2 = A.makeUninitializedUnsafe totalSize in 
      let k = S.union tmp 0 sizea tmp sizea sizeb tmp2 0  in 
      t ~data:(N.ofSortedArrayAux tmp2 0 k) 
  
let mem d x = I.mem (data d) x 
