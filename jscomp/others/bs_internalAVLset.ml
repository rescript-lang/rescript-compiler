
type 'elt node  = {
  mutable left : 'elt node Js.null;
  mutable key : 'elt ; 
  mutable right : 'elt node Js.null;
  mutable h : int 
}
[@@bs.deriving abstract]

module A = Bs_Array

external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"
external unsafeCoerce : 'a Js.null -> 'a = "%identity"
type 'elt t0 =  'elt node Js.null
(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)


let height (n : _ t0) =
  match toOpt n with 
  | None -> 0
  | Some n -> h n 

let rec copy n =   
  match toOpt n with 
  | None -> n 
  | Some n -> 
    let l,r = left n, right n in 
    return @@ node 
      ~left:(copy l) ~right:(copy r)
      ~key:(key n) ~h:(h n)
(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create (l : _ t0) v (r : _ t0) =
  let hl = match toOpt l with None -> 0 | Some n -> h n in
  let hr = match toOpt r with None -> 0 | Some n -> h n in
  return @@ node ~left:l ~key:v ~right:r ~h:(if hl >= hr then hl + 1 else hr + 1)

let heightGe l r = 
  match toOpt l, toOpt r with 
  | _ , None -> true 
  | Some hl, Some hr -> h hl >= h hr 
  | None, Some _ -> false
(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)
(* TODO: inline all [create] operation, save duplicated [h] calcuation *)
let bal l v r =
  let hl = match toOpt l with None -> 0 | Some n -> h n in
  let hr = match toOpt r with None -> 0 | Some n -> h n in
  if hl > hr + 2 then begin
    let n = unsafeCoerce l in   (* [l] could not be empty *)
    let ll,lv,lr = left n, key n, right n in 
    if heightGe ll  lr then
      create ll lv (create lr v r)
    else begin
      let n = unsafeCoerce lr in (* [lr] could not be empty*)
      let lrl, lrv, lrr = left n, key n, right n in 
      create (create ll lv lrl) lrv (create lrr v r)
    end
  end else if hr > hl + 2 then begin
    let n = unsafeCoerce r in  (* [r] could not be empty *)
    let rl,rv,rr = left n, key n, right n in 
    if heightGe rr  rl then
      create (create l v rl) rv rr
    else begin
      let n = unsafeCoerce rl in (* [rl] could not be empty *)
      let rll, rlv, rlr = left n, key n, right n in 
      create (create l v rll) rlv (create rlr rv rr)
    end
  end else
    return @@ node ~left:l ~key:v ~right:r ~h:(if hl >= hr then hl + 1 else hr + 1)

let singleton0 x = return @@ node ~left:empty ~key:x ~right:empty ~h:1


let rec min0Aux n = 
  match toOpt (left n) with 
  | None -> key n
  | Some n -> min0Aux n 

let  minOpt0 n =
  match toOpt n with 
    None -> None
  | Some n -> Some (min0Aux n)

let minNull0 n =   
  match toOpt n with 
  | None -> Js.null
  | Some n -> return (min0Aux n) 

let rec max0Aux n =   
  match toOpt (right n) with 
  | None -> key n
  | Some n -> max0Aux n 

let maxOpt0 n = 
  match toOpt n with 
  | None -> None
  | Some n -> Some (max0Aux n)

let maxNull0 n =   
  match toOpt n with 
  | None -> Js.null
  | Some n -> return (max0Aux n)

let rec removeMinAuxWithRef n v = 
  let rn, ln = right n, left n  in 
  match toOpt ln with   
  | None ->  v:= key n ; rn
  | Some ln -> bal (removeMinAuxWithRef ln v) (key n) rn
  



(* Implementation of the set operations *)

let empty0 = empty

let isEmpty0 n = match toOpt n with Some _ -> false | None -> true 

let rec stackAllLeft v s = 
  match toOpt v with 
  | None -> s 
  | Some x -> stackAllLeft (left x) (x::s)
  

let rec iter0 n f = 
  match toOpt n with 
  | None -> ()
  | Some n  ->
    iter0 (left n) f; f (key n) [@bs]; iter0  (right n) f


let rec fold0 s accu f =
  match toOpt s with
  | None -> accu
  | Some n  -> 
    let l,k,r = left n, key n , right n in 
    fold0 
      r
      (f (fold0  l accu f) k [@bs]) f

let rec forAll0 n p  = 
  match toOpt n with  
  | None -> true
  | Some n  -> 
    p (key n) [@bs] && 
    forAll0  (left n) p &&
    forAll0 (right n) p

let rec exists0 n p = 
  match toOpt n with 
  | None -> false
  | Some n  -> 
    p (key n) [@bs] || 
    exists0 (left n) p || 
    exists0 (right n) p 


(* [addMinElement v n] and [addMaxElement v n] 
   assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree.
   They are only used during the "join" operation which
   respects this precondition.
*)

let rec addMinElement n v =
  match toOpt n with 
  | None -> singleton0 v
  | Some n  ->
    bal (addMinElement (left n) v)  (key n) (right n)

let rec addMaxElement n v = 
  match toOpt n with 
  | None -> singleton0 v
  | Some n  ->
    bal (left n) (key n) (addMaxElement (right n) v)
    
(* [join ln v rn] return a balanced tree simliar to [create ln v rn]
   bal, but no assumptions are made on the
   relative heights of [ln] and [rn]. *)

let rec joinShared ln v rn =
  match (toOpt ln, toOpt rn) with
    (None, _) -> addMinElement rn v 
  | (_, None) -> addMaxElement ln v
  | Some l, Some r ->   
    let lh = h l in     
    let rh = h r in 
    if lh > rh + 2 then bal (left l) (key l) (joinShared (right l) v rn) else
    if rh > lh + 2 then bal (joinShared ln v (left r)) (key r) (right r) else
      create ln v rn
  
(* [concat l r]
   No assumption on the heights of l and r. *)

let concatShared t1 t2 =
  match (toOpt t1, toOpt t2) with
    (None, _) -> t2
  | (_, None) -> t1
  | (_, Some t2n) -> 
    let v = ref (key t2n ) in 
    let t2r = removeMinAuxWithRef t2n v in 
    joinShared t1 !v t2r  
    
    

let rec partitionShared0  n p =
  match toOpt n with 
  |  None -> (empty, empty)
  | Some n  ->
    let l,v,r = left n, key n, right n in 
    let (lt, lf) = partitionShared0 l p in    
    let pv = p v [@bs] in
    let (rt, rf) = partitionShared0 r p in
    if pv
    then (joinShared lt v rt, concatShared lf rf)
    else (concatShared lt rt, joinShared lf v rf)

let rec lengthAux n = 
  let l, r = left n, right n in  
  let sizeL = 
    match toOpt l with 
    | None -> 0
    | Some l -> 
      lengthAux l  in 
  let sizeR = 
    match toOpt r with 
    | None -> 0
    | Some r -> lengthAux r in 
  1 + sizeL + sizeR  

let rec length0 n =
  match toOpt n with 
  | None -> 0
  | Some n  ->
    lengthAux n 

let rec toListAux accu n = 
  match toOpt n with 
  | None -> accu
  | Some n  ->
    let l,k,r = left n, key n, right n in 
    toListAux 
      (k :: toListAux accu r)
      l

let toList0 s =
  toListAux [] s

let rec checkInvariant (v : _ t0) = 
  match toOpt v with 
  | None -> true 
  | Some n -> 
    let l,r = left n , right n in 
    let diff = height l - height r  in 
    diff <=2 && diff >= -2 && checkInvariant l && checkInvariant r 



let rec fillArray n i arr =     
  let l,v,r = left n, key n, right n in 
  let next = 
    match toOpt l with 
    | None -> i 
    | Some l -> 
      fillArray l i arr in 
  A.unsafe_set arr next v ;
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
      A.unsafe_set arr c v;
      forwardSet cursor (c + 1)
  end  
  else begin 
    let c = backward cursor in 
    A.unsafe_set arr c v ;
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
      (A.unsafe_set arr next v;
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
    let size = lengthAux n in 
    let v = A.makeUninitializedUnsafe size in 
    ignore (fillArray n 0 v : int);  (* may add assertion *)
    v 

let rec ofSortedArrayRevAux arr off len =     
  match len with 
  | 0 -> empty0
  | 1 -> singleton0 (A.unsafe_get arr off)
  | 2 ->  
    let x0,x1 = A.(unsafe_get arr off, unsafe_get arr (off - 1) ) 
    in 
    return @@ node ~left:(singleton0 x0) ~key:x1 ~h:2 ~right:empty0
  | 3 -> 
    let x0,x1,x2 = 
      A.(unsafe_get arr off, 
         unsafe_get arr (off - 1), 
         unsafe_get arr (off - 2)) in 
    return @@ node ~left:(singleton0 x0)
      ~right:(singleton0 x2)
      ~key:x1
      ~h:2
  | _ ->  
    let nl = len / 2 in 
    let left = ofSortedArrayRevAux arr off nl in 
    let mid = A.unsafe_get arr (off - nl) in 
    let right = 
      ofSortedArrayRevAux arr (off - nl - 1) (len - nl - 1) in 
    create left mid right    
    

let rec ofSortedArrayAux arr off len =     
  match len with 
  | 0 -> empty0
  | 1 -> singleton0 (A.unsafe_get arr off)
  | 2 ->  
    let x0,x1 = A.(unsafe_get arr off, unsafe_get arr (off + 1) ) 
    in 
    return @@ node ~left:(singleton0 x0) ~key:x1 ~h:2 ~right:empty0
  | 3 -> 
    let x0,x1,x2 = 
      A.(unsafe_get arr off, 
         unsafe_get arr (off + 1), 
         unsafe_get arr (off + 2)) in 
    return @@ node ~left:(singleton0 x0)
      ~right:(singleton0 x2)
      ~key:x1
      ~h:2
  | _ ->  
    let nl = len / 2 in 
    let left = ofSortedArrayAux arr off nl in 
    let mid = A.unsafe_get arr (off + nl) in 
    let right = 
      ofSortedArrayAux arr (off + nl + 1) (len - nl - 1) in 
    create left mid right    


let rec filterShared0 n p =
  match toOpt n with 
  | None -> empty
  | Some n  ->
    let l,v,r = left n, key n, right n in  
    let newL = filterShared0 l p in
    let pv = p v [@bs] in
    let newR = filterShared0 r p in
    if pv then 
      (if l == newL && r == newR then 
        return n
      else joinShared newL v newR)
    else concatShared newL newR
(* ATT: functional methods in general can be shared with 
    imperative methods, however, it does not apply when functional 
    methods makes use of referential equality
*)

let rec filterCopy n p : _ t0= 
  match toOpt n with 
  | None -> empty 
  | Some n -> 
    let size = lengthAux n in 
    let  v = A.makeUninitializedUnsafe size in 
    let last =     
      fillArrayWithFilter n 0 v p in 
    ofSortedArrayAux v 0 last 

let partitionCopy n p  =     
  match toOpt n with 
  | None -> empty, empty  
  | Some n -> 
    let size = lengthAux n in 
    let v = A.makeUninitializedUnsafe size in 
    let backward = size - 1 in 
    let cursor = cursor ~forward:0 ~backward in 
    fillArrayWithPartition n cursor v p ;
    let forwardLen = forward cursor in 
    ofSortedArrayAux v 0 forwardLen,  
    ofSortedArrayRevAux v backward (size  - forwardLen)

let rec mem0 ~cmp  (t: _ t0) x =
  match  toOpt t with 
  | None -> false
  | Some n ->
    let v = key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp (if c < 0 then left n else right n) x


let rec compareAux e1 e2 ~cmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    let c = (Bs_Cmp.getCmp cmp) (key h1) (key h2) [@bs] in 
    if c = 0 then
      compareAux ~cmp 
        (stackAllLeft  (right h1) t1)
        (stackAllLeft (right h2) t2)
    else c 
  | _, _ -> 0   

let cmp0 s1 s2 ~cmp = 
  let len1,len2 = length0 s1, length0 s2 in    
  if len1 = len2 then
   compareAux ~cmp (stackAllLeft s1 []) (stackAllLeft s2 [])
  else if len1 < len2 then -1 else 1 


let eq0 ~cmp s1 s2 =
  cmp0 ~cmp s1 s2 = 0


let rec subset0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match (toOpt s1, toOpt s2) with
  | None, _ -> true
  | _, None -> false
  | Some t1 , Some t2  ->
    let l1,v1,r1 = (left t1, key t1, right t1) in  
    let l2,v2,r2 = (left t2, key t2, right t2) in 
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c = 0 then
      subset0 ~cmp l1 l2 && subset0 ~cmp r1 r2
    else if c < 0 then
      subset0 ~cmp (create l1 v1 empty) l2 && 
      subset0 ~cmp r1 s2
    else
      subset0 ~cmp (create empty v1 r1 ) r2 && 
      subset0 ~cmp l1 s2

let rec findOpt0 ~cmp (n : _ t0) x = 
  match toOpt n with 
    None -> None
  | Some t (* Node(l, v, r, _) *) ->
    let v = key t in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some v
    else findOpt0 ~cmp  (if c < 0 then left t else right t) x


let rec findNull0 ~cmp (n : _ t0) x =
  match toOpt n with 
    None -> Js.null
  | Some t (* Node(l, v, r, _) *) ->
    let v = key t in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then  return v
    else findNull0 ~cmp  (if c < 0 then left t else right t) x 

let rec sortedLengthAux ~cmp (xs : _ array) prec acc len =    
  if  acc >= len then acc 
  else 
    let v = A.unsafe_get xs acc in 
    if cmp v  prec [@bs] >= 0 then 
      sortedLengthAux ~cmp xs v (acc + 1) len 
    else acc    
    
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

let rec addMutate ~cmp (t : _ t0) x =   
  match toOpt t with 
  | None -> singleton0 x 
  | Some nt -> 
    let k = key nt in 
    let  c = (Bs_Cmp.getCmp cmp) x k [@bs] in  
    if c = 0 then t 
    else
      let l, r = (left nt, right nt) in 
      (if c < 0 then                   
         let ll = addMutate ~cmp l x in
         leftSet nt ll
       else   
         rightSet nt (addMutate ~cmp r x);
      );
      return (balMutate nt)


let ofArray0 ~cmp (xs : _ array) =   
  let len = A.length xs in 
  if len = 0 then empty0
  else
    let next = sortedLengthAux 
      ~cmp:(Bs_Cmp.getCmp cmp) xs (A.unsafe_get xs 0) 1 len in 
    let result  = ref (ofSortedArrayAux  xs 0 next) in 
    for i = next to len - 1 do 
      result := addMutate ~cmp !result (A.unsafe_get xs i) 
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


let ofSortedArrayUnsafe0 arr =     
  ofSortedArrayAux arr 0 (A.length arr)