
module N = Bs_internalAVLset
module B = Bs_Bag 
module A = Bs_Array

type ('k,'id) t0 = 'k N.t0
type ('elt,'id) t = (('elt,'id) Bs_Cmp.t , ('elt,'id) t0) B.bag  

(* here we relies on reference transparence
   address equality means everything equal across time
   no need to call [bal] again
*)  
let rec add0  (t : _ t0) x  ~cmp : _ t0 =
  match N.toOpt t with 
    None -> N.singleton0 x 
  | Some nt ->
    let k = N.key nt in 
    let c = (Bs_Cmp.getCmp cmp) x k [@bs] in
    if c = 0 then t else
      let l,r = N.(left nt, right nt) in 
      if c < 0 then 
        let ll = add0 ~cmp l x in 
        if ll == l then t 
        else N.bal ll k r 
      else 
        let rr = add0 ~cmp r x in 
        if rr == r then t 
        else N.bal l k rr 

let rec remove0 ~cmp (t : _ t0) x : _ t0 = 
  match N.toOpt t with 
    None -> t
  | Some n  ->
    let l,v,r = N.(left n , key n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then 
      match N.toOpt l, N.toOpt r with 
      | (None, _) -> r 
      | (_, None) -> l 
      | (_, Some rn) -> 
        let v = ref (N.key rn) in 
        let r = N.removeMinAuxWithRef rn v in 
        N.bal l !v r 
    else
    if c < 0 then 
      let ll = remove0 ~cmp  l x in 
      if ll == l then t
      else N.bal ll v r 
    else
      let rr = remove0 ~cmp  r x in 
      if rr == r then t  
      else N.bal l v rr

let addArray0   h arr ~cmp =   
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.unsafe_get arr i in 
    v := add0 !v  ~cmp key 
  done ;
  !v 

let removeArray0 h arr ~cmp = 
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.unsafe_get arr i in 
    v := remove0 !v  ~cmp key 
  done ;
  !v 

let rec splitAuxNoPivot ~cmp (n : _ N.node) x : _ *  _ =   
  let l,v,r = N.(left n , key n, right n) in  
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
  if c = 0 then l,r
  else 
  if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.empty ,  N.return n
    | Some l -> 
      let (ll,  rl) = splitAuxNoPivot ~cmp  l x in 
      ll,  N.joinShared rl v r
  else
    match N.toOpt r with 
    | None ->
      N.return n,  N.empty
    | Some r -> 
      let lr,  rr = splitAuxNoPivot ~cmp  r x in 
      N.joinShared l v lr, rr

let rec splitAuxPivot ~cmp (n : _ N.node) x pres : _ *  _ =   
  let l,v,r = N.(left n , key n, right n) in  
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
  if c = 0 then 
    begin
      pres := true;
      l, r
    end
  else 
  if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.empty , N.return n
    | Some l -> 
      let (ll, rl) = splitAuxPivot ~cmp  l x pres in 
      ll,  N.joinShared rl v r
  else
    match N.toOpt r with 
    | None ->
      N.return n,  N.empty
    | Some r -> 
      let lr, rr = splitAuxPivot ~cmp  r x pres in 
      N.joinShared l v lr,  rr

let  split0 ~cmp  (t : _ t0) x  =
  match N.toOpt t with 
    None ->
    (N.empty, N.empty), false
  | Some n ->
    let pres = ref false in 
    let v = splitAuxPivot ~cmp n x  pres in 
    v, !pres

(* [union0 s1 s2]
   Use the pivot to split the smaller collection
*)      
let rec union0 ~cmp (s1 : _ t0) (s2 : _ t0) : _ t0=
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s2
  | (_, None) -> s1
  | Some n1, Some n2 ->
    let h1, h2 = N.(h n1 , h n2) in                 
    if h1 >= h2 then
      if h2 = 1 then add0 ~cmp s1 (N.key n2)  
      else begin
        let l1, v1, r1 = N.(left n1, key n1, right n1) in      
        let l2, r2 = splitAuxNoPivot ~cmp n2 v1 in
        N.joinShared (union0 ~cmp l1 l2) v1 (union0 ~cmp r1 r2)
      end
    else
    if h1 = 1 then add0 s2 ~cmp (N.key n1) 
    else begin
      let l2, v2, r2 = N.(left n2 , key n2, right n2) in 
      let l1, r1 = splitAuxNoPivot ~cmp n1 v2  in
      N.joinShared (union0 ~cmp l1 l2) v2 (union0 ~cmp r1 r2)
    end

let rec inter0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match N.(toOpt s1, toOpt s2) with
  | None, _ 
  | _, None -> N.empty
  | Some n1, Some n2  ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in  
    let pres = ref false in 
    let l2,r2 = splitAuxPivot ~cmp n2 v1 pres in 
    let ll = inter0 ~cmp l1 l2 in 
    let rr = inter0 ~cmp r1 r2 in 
    if !pres then N.joinShared ll v1 rr 
    else N.concatShared ll rr 

let rec diff0 ~cmp s1 s2 =
  match N.(toOpt s1, toOpt s2) with
    (None, _) 
  | (_, None) -> s1
  | Some n1, Some n2  ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in
    let pres = ref false in 
    let l2, r2 = splitAuxPivot ~cmp n2 v1 pres in 
    let ll = diff0 ~cmp l1 l2 in 
    let rr = diff0 ~cmp r1 r2 in 
    if !pres then N.concatShared ll rr 
    else N.joinShared ll v1 rr 

        

let empty0 = N.empty0      
let ofArray0 = N.ofArray0
let isEmpty0 = N.isEmpty0
let mem0 = N.mem0
let singleton0 = N.singleton0
let subset0 = N.subset0
let cmp0 = N.cmp0
let eq0 = N.eq0 
let iter0 = N.iter0      
let fold0 = N.fold0
let forAll0 = N.forAll0
let exists0 = N.exists0    
let filter0 = N.filterShared0
let partition0 = N.partitionShared0
let length0 = N.length0
let toList0 = N.toList0
let toArray0 = N.toArray0
let minOpt0 = N.minOpt0
let maxOpt0 = N.maxOpt0
let ofSortedArrayUnsafe0 = N.ofSortedArrayUnsafe0
let findOpt0 = N.findOpt0
let findNull0 = N.findNull0

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


let mem (type elt) (type id) (m : (elt,id) t) e = 
  let dict, data = B.(dict m, data m) in 
  let module M = (val dict) in 
  mem0 ~cmp:(M.cmp) data e

let add (type elt) (type id) (m : (elt,id) t) e =   
  let dict, data = B.(dict m, data m) in
  let module M = (val dict) in 
  let newData = (add0 ~cmp:(M.cmp)  data e) in 
  if newData == data then m 
  else 
    B.bag 
      ~dict 
      ~data:newData

let addArray (type elt) (type id) (m : (elt,id) t) e = 
  let dict, data = B.(dict m, data m) in 
  let module M = (val dict) in 
  let newData = addArray0 ~cmp:M.cmp data e in 
  if newData == data then m
  else B.bag ~dict ~data:newData

let removeArray (type elt) (type id) (m : (elt,id) t) e = 
  let dict, data = B.(dict m, data m) in 
  let module M = (val dict) in 
  let newData = removeArray0 ~cmp:M.cmp data e in 
  if newData == data then m
  else B.bag ~dict ~data:newData
  
let singleton dict e =     
  B.bag ~dict
    ~data:(N.singleton0 e)


let remove (type elt) (type id) (m : (elt,id) t) e =      
  let dict, data = B.(dict m, data m) in   
  let module M = (val dict) in 
  let newData = remove0 ~cmp:M.cmp data e in 
  if newData == data then m 
  else B.bag ~dict ~data:newData

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
  B.bag ~dict ~data:(N.filterShared0 data f )

let partition m f  = 
  let mdata, dict = B.(data m, dict m) in   
  let l,r = N.partitionShared0 mdata f in   
  B.bag ~data:l ~dict, B.bag ~data:r ~dict

let length m = N.length0 (B.data m) 

let toList m = N.toList0 (B.data m)
let toArray m = N.toArray0 (B.data m)

let minOpt m = N.minOpt0 (B.data m)
let minNull m = N.minNull0 (B.data m) 
let maxOpt m = N.maxOpt0 (B.data m)
let maxNull m = N.maxNull0 (B.data m)

let split (type elt) (type id) (m : (elt,id) t) e = 
  let dict, data = B.(dict m, data m) in  
  let module M = (val dict) in 
  let (l,  r), b = split0 ~cmp:M.cmp data e in 
  (B.bag ~dict ~data:l,
  B.bag ~dict ~data:r),
  b
  

let findOpt (type elt) (type id)  (m : (elt,id) t) e =   
  let dict, data = B.(dict m, data m) in   
  let module M = (val dict) in 
  findOpt0 ~cmp:M.cmp data e

let findNull (type elt) (type id) (m : (elt,id) t) e =   
  let dict, data = B.(dict m, data m) in 
  let module M = (val dict) in 
  N.findNull0 ~cmp:M.cmp data e


let ofSortedArrayUnsafe ~dict xs  =
  B.bag ~dict ~data:(N.ofSortedArrayUnsafe0 xs)









