# 2 "others/mapm.cppo.ml"
module I = Belt_internalMapInt
type key = int
# 10 "others/mapm.cppo.ml"
module N = Belt_internalAVLtree
module A = Belt_Array 




type 'a t = {
  mutable data : 'a I.t
} 


let make () = {data = None}
let isEmpty m = N.isEmpty m.data
let clear m =  m.data<- None
(* let singleton k v = t ~data:(N.singleton k v) *)

let minKeyUndefined m = N.minKeyUndefined m.data
let minKey m = N.minKey m.data
let maxKeyUndefined m = N.maxKeyUndefined m.data
let maxKey m = N.maxKey m.data
let minimum m = N.minimum m.data
let minUndefined m = N.minUndefined m.data
let maximum m = N.maximum m.data
let maxUndefined m = N.maxUndefined m.data

let set (m : _ t) k v = 
  let old_data = m.data in 
  let v = I.addMutate old_data k v in 
  if v != old_data then 
    m.data <-v 

let forEachU d f = N.forEachU d.data f
let forEach d f = forEachU d (fun[@bs] a b -> f a b)    
let mapU d f = {data = N.mapU d.data f}
let map d f = mapU d (fun[@bs] a -> f a )    
let mapWithKeyU d f = { data = (N.mapWithKeyU d.data f)}
let mapWithKey d f = mapWithKeyU d (fun [@bs] a b -> f a b)    
let reduceU d acc f  = N.reduceU d.data acc f
let reduce d acc f = reduceU d acc (fun[@bs] a b c -> f a b c)    
let everyU d f = N.everyU d.data f
let every d f = everyU d (fun[@bs] a b -> f a b)    
let someU d f = N.someU d.data f    
let some d f = someU d (fun[@bs] a b -> f a b)
let size d = N.size d.data
let toList d = N.toList d.data
let toArray d = N.toArray d.data
let keysToArray d = N.keysToArray d.data
let valuesToArray d = N.valuesToArray d.data
let checkInvariantInternal d = N.checkInvariantInternal d.data
let has d v = I.has d.data v 


let rec removeMutateAux nt (x : key)= 
  let k = nt.N.key in 
  if x = k then 
    let {N.left = l; right = r} = nt in       
    match l, r with
    | None, _ -> r  
    | _, None -> l 
    | _,  Some nr ->  
      nt.right <- (N.removeMinAuxWithRootMutate nt nr);
      Some (N.balMutate nt)
  else 
    begin 
      if x < k then 
        match  nt.left with         
        | None -> Some nt 
        | Some l ->
          nt.left <- (removeMutateAux l x );
          Some (N.balMutate nt)
      else 
        match  nt.right with 
        | None -> Some nt 
        | Some r -> 
          nt.right <- (removeMutateAux r x);
          Some (N.balMutate nt)
    end

let remove d v = 
  let oldRoot = d.data in 
  match  oldRoot with 
  | None -> ()
  | Some root -> 
    let newRoot = removeMutateAux root v in 
    if newRoot != oldRoot then 
      d.data <- newRoot   


let rec updateDone t (x : key)  f  =   
  match  t with 
  | None ->
    (match f None [@bs] with
    | Some data -> N.singleton x data
    | None -> t)
  | Some nt -> 
    let k = nt.N.key in 
    (* let  c = (Belt_Cmp.getCmpInternal cmp) x k [@bs] in   *)
    if k = x then begin     
      match f (Some nt.value) [@bs] with
      | None ->
        let {N.left = l; right = r} = nt in
        begin match  l,  r with
          | None,  _ -> r
          | _, None  -> l
          | _, Some nr ->
            nt.right <- (N.removeMinAuxWithRootMutate nt nr);
            Some (N.balMutate nt)
        end
      | Some data -> 
        nt.value <- data;
        Some nt
    end      
    else
      let {N.left = l; right = r} = nt in 
      (if x < k then                   
         let ll = updateDone  l x f in
         nt.left <- ll
       else   
         nt.right <- (updateDone  r x f);
      );
      Some (N.balMutate nt)
        
let updateU t x f =       
  let oldRoot = t.data in 
  let newRoot = updateDone oldRoot x f  in 
  if newRoot != oldRoot then 
    t.data <- newRoot 
let update t x f = updateU t x (fun[@bs] a -> f a )
let rec removeArrayMutateAux t xs i len   =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = removeMutateAux t ele  in 
    match  u with 
    | None -> None
    | Some t -> removeArrayMutateAux t xs (i+1) len 
  else Some t    

let removeMany (d : _ t) xs =  
  let oldRoot = d.data in 
  match  oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    let newRoot = removeArrayMutateAux nt xs 0 len in 
    if newRoot != oldRoot then 
      d.data <- newRoot



(* let split = I.split  *)
(* let merge = I.merge  *)


let fromArray xs = 
  { data = I.fromArray xs }

let cmpU d0 d1 f = 
  I.cmpU d0.data d1.data f
let cmp d0 d1 f = cmpU d0 d1 (fun[@bs] a b -> f a b)          

let eqU d0 d1 f = 
  I.eqU d0.data d1.data f
let eq d0 d1 f = eqU d0 d1 (fun[@bs] a b -> f a b)    

let get d x =   I.get d.data x 
let getUndefined d x = I.getUndefined d.data x 
let getWithDefault d x def = I.getWithDefault d.data x def  
let getExn d x = I.getExn d.data x 
