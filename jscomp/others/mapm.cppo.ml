#ifdef TYPE_INT
module I = Bs_internalMapInt
type key = int
#elif defined TYPE_STRING
module I = Bs_internalMapString
type key = string
#else
  [%error "unknown type"]
#endif  
module N = Bs_internalAVLtree
module A = Bs_Array 




type 'a t = {
  mutable data : 'a I.t
} [@@bs.deriving abstract]


let empty () = t ~data:N.empty
let isEmpty m = N.isEmpty (data m)
let singleton k v = t ~data:(N.singleton k v)

let minKeyUndefined m = N.minKeyUndefined (data m)
let minKey m = N.minKey (data m)
let maxKeyUndefined m = N.maxKeyUndefined (data m)
let maxKey m = N.maxKey (data m)
let minimum m = N.minimum (data m)
let minUndefined m = N.minUndefined (data m)
let maximum m = N.maximum (data m)
let maxUndefined m = N.maxUndefined (data m)

let set (m : _ t) k v = 
  let old_data = data m in 
  let v = I.addMutate old_data k v in 
  if v != old_data then 
    dataSet m v 

let forEach d f = N.forEach (data d) f     
let map d f = t ~data:(N.map (data d) f)
let mapWithKey d f = t ~data:(N.mapWithKey (data d) f) 
let reduce d acc f  = N.reduce (data d) acc f 
let every d f = N.every (data d) f 
let some d f = N.some (data d) f    

let size d = N.size (data d)
let toList d = N.toList (data d)
let toArray d = N.toArray (data d)
let keysToArray d = N.keysToArray (data d)
let valuesToArray d = N.valuesToArray (data d)
let checkInvariantInternal d = N.checkInvariantInternal (data d)
let has d v = I.has (data d) v 


let rec removeMutateAux nt (x : key)= 
  let k = N.key nt in 
  if x = k then 
    let l,r = N.(left nt, right nt) in       
    match N.(toOpt l, toOpt r) with
    | None, _ -> r  
    | _, None -> l 
    | _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
      N.return (N.balMutate nt)
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

let remove d v = 
  let oldRoot = data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some root -> 
    let newRoot = removeMutateAux root v in 
    if newRoot != oldRoot then 
      dataSet d newRoot   


let rec updateDone t (x : key)  f  =   
  match N.toOpt t with 
  | None ->
    (match f None [@bs] with
    | Some data -> N.singleton x data
    | None -> t)
  | Some nt -> 
    let k = N.key nt in 
    (* let  c = (Bs_Cmp.getCmpInternal cmp) x k [@bs] in   *)
    if k = x then begin     
      match f (Some (N.value nt)) [@bs] with
      | None ->
        let l,r = N.left nt, N.right nt in
        begin match N.toOpt l, N.toOpt r with
          | None,  _ -> r
          | _, None  -> l
          | _, Some nr ->
            N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
            N.return (N.balMutate nt)
        end
      | Some data -> 
        N.valueSet nt data;
        N.return nt
    end      
    else
      let l, r = N.(left nt, right nt) in 
      (if x < k then                   
         let ll = updateDone  l x f in
         N.leftSet nt ll
       else   
         N.rightSet nt (updateDone  r x f);
      );
      N.return (N.balMutate nt)
        
let update t x f =       
  let oldRoot = data t in 
  let newRoot = updateDone oldRoot x f  in 
  if newRoot != oldRoot then 
    dataSet t newRoot 

let rec removeArrayMutateAux t xs i len   =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = removeMutateAux t ele  in 
    match N.toOpt u with 
    | None -> N.empty
    | Some t -> removeArrayMutateAux t xs (i+1) len 
  else N.return t    

let removeMany (type elt) (type id) (d : _ t) xs =  
  let oldRoot = data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    let newRoot = removeArrayMutateAux nt xs 0 len in 
    if newRoot != oldRoot then 
      dataSet d newRoot


let cmp = I.cmp 
let eq = I.eq 


(* let split = I.split  *)
(* let merge = I.merge  *)


let ofArray xs = 
  t  ~data:(I.ofArray xs)

let cmp d0 d1 = 
  I.cmp (data d0) (data d1)
let eq d0 d1 = 
  I.eq (data d0) (data d1)
let get d x =   I.get (data d) x 
let getUndefined d x = I.getUndefined (data d) x 
let getWithDefault d x def = I.getWithDefault (data d) x def  
let getExn d x = I.getExn (data d) x 
