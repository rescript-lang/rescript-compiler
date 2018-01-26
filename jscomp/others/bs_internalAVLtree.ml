(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(** Almost rewritten  by authors of BuckleScript                       *)


type ('k, 'v) node  = {
  mutable left : ('k,'v) node Js.null;
  mutable key : 'k; 
  mutable value : 'v; 
  mutable right : ('k,'v) node Js.null;
  mutable h : int 
} [@@bs.deriving abstract]
module A = Bs_Array 
module S = Bs_Sort
external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null" 
external unsafeCoerce : 'a Js.null -> 'a = "%identity"
type ('key, 'a) t0 = ('key, 'a) node Js.null


let height (n : _ t0) =
  match toOpt n with 
    None -> 0
  | Some n -> h n 

let rec copy n =   
  match toOpt n with 
  | None -> n 
  | Some n -> 
    let l,r = left n, right n in
    return @@ node ~left:(copy l) ~right:(copy r) ~value:(value n) ~key:(key n) ~h:(h n)

let create l x d r =
  let hl, hr  = height l,  height r in
  return @@ node ~left:l ~key:x ~value:d ~right:r ~h:(if hl >= hr then hl + 1 else hr + 1)

let singleton0 x d = 
  return @@ node ~left:empty ~key:x ~value:d ~right:empty ~h:1

let heightGe l r =   
  match toOpt l, toOpt r with  
  | _, None -> true 
  | Some hl, Some hr -> h hl >= h hr 
  | None, Some _ -> false

let updateValue n newValue =
  if value n == newValue then n 
  else
    node 
    ~left:(left n)
    ~right:(right n)
    ~key:(key n)
    ~value:newValue
    ~h:(h n)

let bal l x d r =
  let hl = match toOpt l with None -> 0 | Some n -> h n in
  let hr = match toOpt r with None -> 0 | Some n -> h n in
  if hl > hr + 2 then begin
    let n = unsafeCoerce l in  
    let ll,lv,ld,lr = left n, key n, value n, right n in  
    if height ll >= height lr then
      create ll lv ld (create lr x d r)
    else begin
      let n = unsafeCoerce lr in 
      let lrl, lrv, lrd,lrr = left n, key n, value n, right n in 
      create (create ll lv ld lrl) lrv lrd (create lrr x d r)
    end
  end else if hr > hl + 2 then begin
    let n = unsafeCoerce r in 
    let rl, rv, rd, rr = left n, key n, value n, right n in  
    if height rr >= height rl then
      create (create l x d rl) rv rd rr
    else begin
      let n = unsafeCoerce rl in 
      let rll, rlv,rld,rlr = left n, key n, value n, right n in 
      create (create l x d rll) rlv rld (create rlr rv rd rr)
    end
  end else
    return @@ node ~left:l ~key:x ~value:d ~right:r ~h:(if hl >= hr then hl + 1 else hr + 1)


let rec minKey0Aux n =  
  match toOpt (left n) with 
  | None -> key n
  | Some n -> minKey0Aux n 
    
let minKeyOpt0 n =     
  match toOpt n with 
  | None -> None 
  | Some n -> Some (minKey0Aux n)

let minKeyNull0 n =   
  match toOpt n with 
  | None -> Js.null 
  | Some n -> return (minKey0Aux n)

let rec maxKey0Aux n =  
  match toOpt (right n) with 
  | None -> key n
  | Some n -> maxKey0Aux n 
    
let maxKeyOpt0 n =     
  match toOpt n with 
  | None -> None 
  | Some n -> Some (maxKey0Aux n)

let maxKeyNull0 n =   
  match toOpt n with 
  | None -> Js.null 
  | Some n -> return (maxKey0Aux n)
  
let rec minKV0Aux n =  
  match toOpt (left n) with 
  | None -> key n , value n 
  | Some n -> minKV0Aux n 

let minKVOpt0 n = 
  match toOpt n with 
    None -> None
  | Some n -> Some (minKV0Aux n)

let minKVNull0 n = 
  match toOpt n with 
  | None -> Js.null
  | Some n -> return (minKV0Aux n)

let rec maxKV0Aux n =   
  match toOpt (right n) with 
  | None -> key n, value n 
  | Some n -> maxKV0Aux n 

let maxKVOpt0 n =
  match toOpt n with 
  | None -> None 
  | Some n -> Some (maxKV0Aux n)

let maxKVNull0 n =   
  match toOpt n with 
  | None -> Js.null
  | Some n -> return (maxKV0Aux n)


let rec removeMinAuxWithRef n kr vr =   
  let ln, rn, kn, vn = left n, right n, key n, value n in 
  match toOpt ln with 
  | None ->  kr := kn; vr := vn; rn 
  | Some ln -> bal (removeMinAuxWithRef ln kr vr) kn vn rn

let empty0 = empty

let isEmpty0 x = match toOpt x with None -> true | Some _ -> false

let rec stackAllLeft v s = 
  match toOpt v with 
  | None -> s 
  | Some x -> stackAllLeft (left x) (x::s)    

let rec iter0 n f = 
  match toOpt n with 
  | None -> () 
  | Some n -> 
    iter0 (left n) f ; f (key n) (value n) [@bs]; iter0 (right n) f

let rec map0 n f = 
  match toOpt n with
    None  ->
    empty
  | Some n  ->
    let newLeft = map0 (left n) f in
    let newD = f (value n) [@bs] in
    let newRight = map0 (right n) f in
    return @@ node ~left:newLeft ~key:(key n) ~value:newD ~right:newRight ~h:(h n)

let rec mapi0 n f =
  match toOpt n with 
    None ->
    empty
  | Some n ->
    let key = key n in 
    let newLeft = mapi0 (left n) f in
    let newD = f key (value n) [@bs] in
    let newRight = mapi0 (right n) f in
    return @@ node ~left:newLeft ~key ~value:newD ~right:newRight ~h:(h n)

let rec fold0 m accu f =
  match toOpt m with
    None -> accu
  | Some n  ->
    let l, v, d, r = left n,  key n, value n, right n in 
    fold0
      r 
      (f (fold0 l accu f) v d [@bs]) f

let rec every0  n p =
  match toOpt n with 
    None -> true
  | Some n  ->    
    p (key n) (value n) [@bs] && 
    every0 (left n) p && 
    every0 (right n) p

let rec some0 n p = 
  match toOpt n with 
    None -> false
  | Some n  ->
    p (key n) (value n) [@bs] || 
    some0 (left n) p || 
    some0 (right n) p

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec addMinElement n k v  = 
  match toOpt n with
  | None -> singleton0 k v
  | Some n  
    ->
    bal (addMinElement (left n) k v ) (key n) (value n) (right n)

let rec addMaxElement n k v  = 
  match toOpt n with 
  | None -> singleton0 k v
  | Some n 
    ->      
    bal (left n) (key n) (value n) (addMaxElement (right n) k v )

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join ln v d rn =
  match (toOpt ln, toOpt rn) with
    (None, _) -> addMinElement rn v d  
  | (_, None) -> addMaxElement ln v d  
  | Some l, Some r  ->
    let (ll, lv, ld, lr, lh) = left l, key l, value l, right l, h l in 
    let (rl, rv, rd, rr, rh) = left r, key r, value r, right r, h r in  
    if lh > rh + 2 then bal ll lv ld (join lr v d rn) else
    if rh > lh + 2 then bal (join ln v d rl) rv rd rr else
      create ln v d rn

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (toOpt t1, toOpt t2) with
    (None, _) -> t2
  | (_, None) -> t1
  | (_, Some t2n) ->
    let kr, vr = ref (key t2n), ref (value t2n) in 
    let t2r = removeMinAuxWithRef t2n kr vr in 
    join t1 !kr !vr t2r 

let concatOrJoin t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2    

let rec filterShared0 n p = 
  match toOpt n with 
    None -> empty
  | Some n  ->
    (* call [p] in the expected left-to-right order *)
    let  v, d =  key n, value n  in 
    let newLeft = filterShared0 (left n) p in
    let pvd = p v d [@bs] in
    let newRight = filterShared0 (right n) p in
    if pvd then join newLeft v d newRight else concat newLeft newRight

let rec filterMap0 n p = 
  match toOpt n with 
    None -> empty
  | Some n  ->
    (* call [p] in the expected left-to-right order *)
    let  v, d =  key n, value n  in 
    let newLeft = filterMap0 (left n) p in
    let pvd = p v d [@bs] in
    let newRight = filterMap0 (right n) p in
    match pvd with 
    | None -> concat newLeft newRight
    | Some d -> join newLeft v d newRight 
    

let rec partitionShared0 n p = 
  match toOpt n with   
    None -> (empty, empty)
  | Some n  ->
    let  key, value =  key n, value n  in
    (* call [p] in the expected left-to-right order *)    
    let (lt, lf) = partitionShared0 (left n) p in
    let pvd = p key value [@bs] in
    let (rt, rf) = partitionShared0 (right n) p in
    if pvd
    then (join lt key value rt, concat lf rf)
    else (concat lt rt, join lf key value rf)  


let rec lengthNode n = 
  let l, r = left n, right n in  
  let sizeL = 
    match toOpt l with 
    | None -> 0
    | Some l -> 
      lengthNode l  in 
  let sizeR = 
    match toOpt r with 
    | None -> 0
    | Some r -> lengthNode r in 
  1 + sizeL + sizeR      

let rec length0 n =
  match toOpt n with 
  | None -> 0
  | Some n  ->
    lengthNode n   

let rec toListAux accu n = 
  match toOpt n with 
  | None -> accu
  | Some n  ->    
    toListAux ((key n, value n) :: toListAux accu (right n)) (left n)

let toList0 s =
  toListAux [] s  


let rec checkInvariantInternal (v : _ t0) = 
  match toOpt v with 
  | None -> true 
  | Some n -> 
    let l,r = left n , right n in 
    let diff = height l - height r  in 
    diff <=2 && diff >= -2 && checkInvariantInternal l && checkInvariantInternal r 


let rec fillArrayKey n i arr =     
  let l,v,r = left n, key n, right n in 
  let next = 
    match toOpt l with 
    | None -> i 
    | Some l -> 
      fillArrayKey l i arr in 
  A.setUnsafe arr next v;
  let rnext = next + 1 in 
  match toOpt r with 
  | None -> rnext 
  | Some r -> 
    fillArrayKey r rnext arr     

let rec fillArrayValue n i arr =     
  let l,r = left n,  right n in 
  let next = 
    match toOpt l with 
    | None -> i 
    | Some l -> 
      fillArrayValue l i arr in 
  A.setUnsafe arr next (value n);
  let rnext = next + 1 in 
  match toOpt r with 
  | None -> rnext 
  | Some r -> 
    fillArrayValue r rnext arr         

let rec fillArray n i arr =     
  let l,v,r = left n, key n, right n in 
  let next = 
    match toOpt l with 
    | None -> i 
    | Some l -> 
      fillArray l i arr in 
  A.setUnsafe arr next (v, value n) ;
  let rnext = next + 1 in 
  match toOpt r with 
  | None -> rnext 
  | Some r -> 
    fillArray r rnext arr 

type cursor =     
  { mutable forward : int; mutable backward : int } [@@bs.deriving abstract]

let rec fillArrayWithPartition n cursor arr p =     
  let l,v,r = left n, key n, right n in 
  (match toOpt l with 
   | None -> ()
   | Some l -> 
     fillArrayWithPartition l cursor arr p);  
  (if p v [@bs] then begin        
      let c = forward cursor in 
      A.setUnsafe arr c (v,value n);
      forwardSet cursor (c + 1)
    end  
   else begin 
     let c = backward cursor in 
     A.setUnsafe arr c (v, value n);
     backwardSet cursor (c - 1)
   end);     
  match toOpt r with 
  | None -> ()
  | Some r -> 
    fillArrayWithPartition r cursor arr  p 

let rec fillArrayWithFilter n i arr p =     
  let l,v,r = left n, key n, right n in 
  let next = 
    match toOpt l with 
    | None -> i 
    | Some l -> 
      fillArrayWithFilter l i arr p in 
  let rnext =
    if p v [@bs] then        
      (A.setUnsafe arr next (v, value n);
       next + 1
      )
    else next in   
  match toOpt r with 
  | None -> rnext 
  | Some r -> 
    fillArrayWithFilter r rnext arr  p 


let toArray0 n =   
  match toOpt n with 
  | None -> [||]
  | Some n ->  
    let size = lengthNode n in 
    let v = A.makeUninitializedUnsafe size in 
    ignore (fillArray n 0 v : int);  (* may add assertion *)
    v 

let keysToArray0 n =     
  match toOpt n with 
  | None -> [||]
  | Some n ->  
    let size = lengthNode n in 
    let v = A.makeUninitializedUnsafe size in 
    ignore (fillArrayKey n 0 v : int);  (* may add assertion *)
    v 

let valuesToArray0 n =     
  match toOpt n with 
  | None -> [||]
  | Some n ->  
    let size = lengthNode n in 
    let v = A.makeUninitializedUnsafe size in 
    ignore (fillArrayValue n 0 v : int);  (* may add assertion *)
    v         
    
let rec ofSortedArrayRevAux arr off len =     
  match len with 
  | 0 -> empty0
  | 1 -> let k, v = (A.getUnsafe arr off) in singleton0 k v 
  | 2 ->  
    let (x0,y0),(x1,y1) = A.(getUnsafe arr off, getUnsafe arr (off - 1) ) 
    in 
    return @@ node ~left:(singleton0 x0 y0) ~key:x1 ~value:y1 ~h:2 ~right:empty0
  | 3 -> 
    let (x0,y0),(x1,y1),(x2,y2) = 
      A.(getUnsafe arr off, 
         getUnsafe arr (off - 1), 
         getUnsafe arr (off - 2)) in 
    return @@ node ~left:(singleton0 x0 y0)
      ~right:(singleton0 x2 y2)
      ~key:x1
      ~value:y1
      ~h:2
  | _ ->  
    let nl = len / 2 in 
    let left = ofSortedArrayRevAux arr off nl in 
    let midK,midV = A.getUnsafe arr (off - nl) in 
    let right = 
      ofSortedArrayRevAux arr (off - nl - 1) (len - nl - 1) in 
    create left midK midV right    


let rec ofSortedArrayAux arr off len =     
  match len with 
  | 0 -> empty0
  | 1 -> let k, v = (A.getUnsafe arr off) in singleton0 k v 
  | 2 ->  
    let (x0,y0),(x1,y1) = A.(getUnsafe arr off, getUnsafe arr (off + 1) ) 
    in 
    return @@ node ~left:(singleton0 x0 y0) ~key:x1 ~value:y1 ~h:2 ~right:empty0
  | 3 -> 
    let (x0,y0),(x1,y1),(x2,y2) = 
      A.(getUnsafe arr off, 
         getUnsafe arr (off + 1), 
         getUnsafe arr (off + 2)) in 
    return @@ node ~left:(singleton0 x0 y0)
      ~right:(singleton0 x2 y2)
      ~key:x1 ~value:y1
      ~h:2
  | _ ->  
    let nl = len / 2 in 
    let left = ofSortedArrayAux arr off nl in 
    let midK, midV = A.getUnsafe arr (off + nl) in 
    let right = 
      ofSortedArrayAux arr (off + nl + 1) (len - nl - 1) in 
    create left midK midV right    

let ofSortedArrayUnsafe0 arr =     
  ofSortedArrayAux arr 0 (A.length arr)

let rec compareAux e1 e2 ~kcmp ~vcmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    let c = (Bs_Cmp.getCmp kcmp) (key h1) (key h2) [@bs] in 
    if c = 0 then 
      let cx = vcmp (value h1) (value h2) [@bs] in 
      if cx = 0 then
        compareAux ~kcmp ~vcmp 
          (stackAllLeft  (right h1) t1 )
          (stackAllLeft (right h2) t2)
      else  cx
    else c 
  | _, _ -> 0   

let rec eqAux e1 e2 ~kcmp ~vcmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    if (Bs_Cmp.getCmp kcmp) (key h1) (key h2) [@bs] = 0 && 
       vcmp (value h1) (value h2) [@bs] then
      eqAux ~kcmp ~vcmp (
        stackAllLeft  (right h1) t1 ) (stackAllLeft (right h2) t2)
    else  false    
  | _, _ -> true 

let cmp0 s1 s2 ~kcmp ~vcmp =
  let len1,len2 = length0 s1, length0 s2 in 
  if len1 = len2 then 
    compareAux (stackAllLeft s1 []) (stackAllLeft s2 []) ~kcmp ~vcmp 
  else  if len1 < len2 then -1 else 1 

let eq0  s1 s2 ~kcmp ~vcmp =
  let len1, len2 = length0 s1, length0 s2 in 
  if len1 = len2 then
    eqAux (stackAllLeft s1 []) (stackAllLeft s2 []) ~kcmp ~vcmp
  else false

let rec findOpt0  n x ~cmp = 
  match toOpt n with 
    None -> None
  | Some n (* Node(l, v, d, r, _) *)  ->
    let v = key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some (value n)
    else findOpt0 ~cmp  (if c < 0 then left n else right n) x 

let rec findNull0  n x ~cmp =
  match toOpt n with 
  | None -> Js.null
  | Some n  ->
    let v = key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then return (value n )
    else findNull0 ~cmp  (if c < 0 then left n else right n) x 

let rec findExn0   n x  ~cmp = 
  match toOpt n with 
    None ->
    [%assert "findExn0"]
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then value n 
    else findExn0 ~cmp  (if c < 0 then left n else right n) x

let rec findWithDefault0   n x def ~cmp = 
  match toOpt n with 
    None ->
    def
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then value n 
    else findWithDefault0 ~cmp  (if c < 0 then left n else right n) x def

let rec mem0  n x ~cmp = 
  match toOpt n with 
    None ->
    false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp (if c < 0 then left n else right n) x


(******************************************************************)

(* 
  L rotation, return root node
*)
let rotateWithLeftChild k2 = 
  let k1 = unsafeCoerce (left k2) in 
  (leftSet k2 (right k1)); 
  (rightSet k1 (return k2 ));
  let hlk2, hrk2 = (height (left k2), (height (right k2))) in  
  (hSet k2 
     (Pervasives.max hlk2 hrk2 + 1));
  let hlk1, hk2 = (height (left k1), (h k2)) in 
  (hSet k1 (Pervasives.max hlk1 hk2 + 1));
  k1  
(* right rotation *)
let rotateWithRightChild k1 =   
  let k2 = unsafeCoerce (right k1) in 
  (rightSet k1 (left k2));
  (leftSet k2 (return k1));
  let hlk1, hrk1 = ((height (left k1)), (height (right k1))) in 
  (hSet k1 (Pervasives.max  hlk1 hrk1 + 1));
  let hrk2, hk1 = (height (right k2), (h k1)) in 
  (hSet k2 (Pervasives.max  hrk2 hk1 + 1));
  k2 

(*
  double l rotation
*)  
let doubleWithLeftChild k3 =   
  let v = rotateWithRightChild (unsafeCoerce (left k3)) in 
  (leftSet k3 (return v ));
  rotateWithLeftChild k3 

let doubleWithRightChild k2 = 
  let v = rotateWithLeftChild (unsafeCoerce (right k2)) in   
  (rightSet k2 (return v));
  rotateWithRightChild k2

let heightUpdateMutate t = 
  let hlt, hrt = (height (left t),(height (right t))) in 
  hSet t (Pervasives.max hlt hrt  + 1);
  t

let balMutate nt  =  
  let l, r = (left nt, right nt) in  
  let hl, hr =  (height l, height r) in 
  if hl > 2 +  hr then 
    let l = unsafeCoerce l in 
    let ll, lr = (left l , right l)in
    (if heightGe ll lr then 
       heightUpdateMutate (rotateWithLeftChild nt)
     else 
       heightUpdateMutate (doubleWithLeftChild nt)
    )
  else 
  if hr > 2 + hl  then 
    let r = unsafeCoerce r in 
    let rl,rr = (left r, right r) in 
    (if heightGe rr rl then 
       heightUpdateMutate (rotateWithRightChild nt) 
     else 
       heightUpdateMutate (doubleWithRightChild nt)
    ) 
  else 
    begin
      hSet nt (max hl hr + 1);
      nt
    end

let rec updateMutate (t : _ t0) x data ~cmp =   
  match toOpt t with 
  | None -> singleton0 x data
  | Some nt -> 
    let k = key nt in 
    let  c = (Bs_Cmp.getCmp cmp) x k [@bs] in  
    if c = 0 then begin     
      valueSet nt data;
      return nt
    end      
    else
      let l, r = (left nt, right nt) in 
      (if c < 0 then                   
         let ll = updateMutate ~cmp l x data in
         leftSet nt ll
       else   
         rightSet nt (updateMutate ~cmp r x data);
      );
      return (balMutate nt)  

let ofArray0 (xs : _ array) ~cmp =   
  let len = A.length xs in 
  if len = 0 then empty0
  else
    let next = 
      ref (S.strictlySortedLength xs 
             (fun[@bs] (x0,_) (y0,_) -> 
                (Bs_Cmp.getCmp cmp) x0 y0 [@bs] < 0
             ))
    in 
    let result  = ref (
        if !next >= 0 then 
          ofSortedArrayAux xs 0 !next 
        else begin   
          next := - !next; 
          ofSortedArrayRevAux xs (!next - 1) (!next)
        end  
      ) in 
    for i = !next to len - 1 do 
      let k, v = (A.getUnsafe xs i)  in 
      result := updateMutate ~cmp !result k v 
    done ;
    !result         


let rec removeMinAuxWithRootMutate nt n = 
  let rn, ln = right n, left n in 
  match toOpt ln with 
  | None -> 
    keySet nt (key n);
    rn 
  | Some ln -> 
    leftSet n (removeMinAuxWithRootMutate nt ln); 
    return (balMutate n)    
