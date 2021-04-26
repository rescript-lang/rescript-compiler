
(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
[@@@bs.config {flags = [|"-bs-noassertfalse" |] }]
type 'value node  = {
  mutable value : 'value; [@bs.as "v"]
  mutable height : int; [@bs.as "h"]
  mutable left : 'value t; [@bs.as "l"]
  mutable right : 'value t; [@bs.as "r"]
}
and 'value t =  'value node option


module A = Belt_Array
module S = Belt_SortArray

type ('a, 'b) cmp = ('a, 'b) Belt_Id.cmp

(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)


let [@inline] height (n : _ t) =
  match n with
  | None -> 0
  | Some n -> n.height

let rec copy n =
  match n with
  | None -> n
  | Some n ->
    Some { left = copy n.left ; right = copy n.right;
      value = n.value; height = n.height}
    
(* Creates a new node with leftGet son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | treeHeight l - treeHeight r | <= 2.
   Inline expansion of treeHeight for better speed. *)

let [@inline] calcHeight (hl : int) hr = 
  (if hl >= hr then hl  else hr) + 1

let create (l : _ t) v (r : _ t) =
  let hl = height l in
  let hr = height r in
  Some { left = l; value = v; right = r; height = calcHeight hl hr}
  
  
let singleton x = Some { left = None; value = x; right = None; height = 1} 

let heightGe l r =
  match l, r with
  | _ , None -> true
  | Some hl, Some hr -> hl.height >= hr.height
  | None, Some _ -> false
(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | treeHeight l - treeHeight r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)
(* TODO: inline all `create` operation, save duplicated `heightGet` calcuation *)
let bal l v r =
  let hl,hr = height l, height r in 
  if hl > hr + 2 then begin
    match l with None -> assert false | Some ({left = ll;  right = lr} as l) ->
      if heightGe ll  lr then
        create ll l.value (create lr v r)
      else 
        match lr with None -> assert false | Some lr -> 
          create (create ll l.value lr.left) lr.value (create lr.right v r)    
  end else if hr > hl + 2 then 
    match r with None -> assert false | Some ({left = rl; right = rr} as r) ->
      if heightGe rr  rl then
        create (create l v rl) r.value rr
      else 
        match rl with None -> assert false | Some rl -> 
          create (create l v rl.left) rl.value (create rl.right r.value rr)
  else
    Some {left = l ; value = v ; right = r; height = calcHeight hl hr}




let rec min0Aux n =
  match n.left with
  | None -> n.value
  | Some n -> min0Aux n

let  minimum n =
  match n with
    None -> None
  | Some n -> Some (min0Aux n)

let minUndefined n =
  match n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (min0Aux n)

let rec max0Aux n =
  match n.right with
  | None -> n.value
  | Some n -> max0Aux n

let maximum n =
  match n with
  | None -> None
  | Some n -> Some (max0Aux n)

let maxUndefined n =
  match n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (max0Aux n)

let rec removeMinAuxWithRef n v =
  match n.left with
  | None ->  v.contents<- n.value ; n.right
  | Some ln -> bal (removeMinAuxWithRef ln v) n.value n.right




(* Implementation of the set operations *)



let isEmpty n = match n with Some _ -> false | None -> true

let rec stackAllLeft v s =
  match v with
  | None -> s
  | Some x -> stackAllLeft x.left (x::s)


let rec forEachU n f =
  match n with
  | None -> ()
  | Some n  ->
    forEachU n.left f; f (n.value) [@bs]; forEachU n.right f

let forEach n f = forEachU n (fun [@bs] a -> f a)

let rec reduceU s accu f =
  match s with
  | None -> accu
  | Some n  ->
    reduceU
      n.right
      (f (reduceU  n.left accu f) n.value [@bs]) f

let reduce s accu f = reduceU s accu (fun [@bs] a b -> f a b)

let rec everyU n p  =
  match n with
  | None -> true
  | Some n  ->
    p (n.value) [@bs] &&
    n.left  |. everyU  p &&
    n .right |. everyU  p

let every n p = everyU n (fun [@bs] a -> p a )

let rec someU n p =
  match n with
  | None -> false
  | Some n  ->
    p n.value [@bs] ||
    someU n.left  p ||
    someU n.right p

let some n p = someU n (fun[@bs] a -> p a )
(* `addMinElement v n` and `addMaxElement v n`
   assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree.
   They are only used during the "join" operation which
   respects this precondition.
*)

let rec addMinElement n v =
  match n with
  | None -> singleton v
  | Some n  ->
    bal (addMinElement n.left v) n.value n.right

let rec addMaxElement n v =
  match n with
  | None -> singleton v
  | Some n  ->
    bal n.left (n.value) (addMaxElement n.right v)

(* `join ln v rn` Some a balanced tree simliar to `create ln v rn`
   bal, but no assumptions are made on the
   relative heights of `ln` and `rn`. *)

let rec joinShared ln v rn =
  match (ln, rn) with
    (None, _) -> addMinElement rn v
  | (_, None) -> addMaxElement ln v
  | Some l, Some r ->
    let lh = l.height in
    let rh = r.height in
    if lh > rh + 2 then bal l.left l.value (joinShared l.right v rn) else
    if rh > lh + 2 then bal (joinShared ln v r.left) r.value r.right else
      create ln v rn

(* `concat l r`
   No assumption on the heights of l and r. *)

let concatShared t1 t2 =
  match (t1, t2) with
    (None, _) -> t2
  | (_, None) -> t1
  | (_, Some t2n) ->
    let v = ref t2n.value in
    let t2r = removeMinAuxWithRef t2n v in
    joinShared t1 v.contents t2r



let rec partitionSharedU  n p =
  match n with
  |  None -> (None, None)
  | Some n  ->
    let value = n.value in
    let (lt, lf) = partitionSharedU n.left p in
    let pv = p value [@bs] in
    let (rt, rf) = partitionSharedU n.right p in
    if pv
    then (joinShared lt value rt, concatShared lf rf)
    else (concatShared lt rt, joinShared lf value rf)

let partitionShared n p = partitionSharedU n (fun [@bs] a -> p a)

let rec lengthNode n =
  let {left = l; right = r} = n  in
  let sizeL =
    match l with
    | None -> 0
    | Some l ->
      lengthNode l  in
  let sizeR =
    match r with
    | None -> 0
    | Some r -> lengthNode r in
  1 + sizeL + sizeR

let  size n =
  match n with
  | None -> 0
  | Some n  ->
    lengthNode n

let rec toListAux n accu  =
  match n with
  | None -> accu
  | Some n  ->
    toListAux
      n.left
      ((n.value) :: toListAux n.right accu)


let toList s =
  toListAux s []

let rec checkInvariantInternal (v : _ t) =
  match v with
  | None -> ()
  | Some n ->
    let {left = l; right = r} = n   in
    let diff = height l - height r  in
    assert (diff <=2 && diff >= -2);
    checkInvariantInternal l;
    checkInvariantInternal r



let rec fillArray n i arr =
  let {left = l; value = v; right = r} = n  in
  let next =
    match l with
    | None -> i
    | Some l ->
      fillArray l i arr in
  A.setUnsafe arr next v ;
  let rnext = next + 1 in
  match r with
  | None -> rnext
  | Some r ->
    fillArray r rnext arr

type cursor =
  { mutable forward : int; mutable backward : int } 

let rec fillArrayWithPartition n cursor arr p =
  let {left = l; value = v; right = r} = n  in
  (match l with
   | None -> ()
   | Some l ->
     fillArrayWithPartition l cursor arr p);
  (if p v [@bs] then begin
      let c = cursor.forward in
      A.setUnsafe arr c v;
      cursor.forward <- (c + 1)
    end
   else begin
     let c = cursor.backward in
     A.setUnsafe arr c v ;
     cursor.backward <- (c - 1)
   end);
  match r with
  | None -> ()
  | Some r ->
    fillArrayWithPartition r cursor arr  p

let rec fillArrayWithFilter n i arr p =
  let {left = l; value = v; right = r} = n  in
  let next =
    match l with
    | None -> i
    | Some l ->
      fillArrayWithFilter l i arr p in
  let rnext =
    if p v [@bs] then
      (A.setUnsafe arr next v;
       next + 1
      )
    else next in
  match r with
  | None -> rnext
  | Some r ->
    fillArrayWithFilter r rnext arr  p


let toArray n =
  match n with
  | None -> [||]
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    ignore (fillArray n 0 v : int);  (* may add assertion *)
    v

let rec fromSortedArrayRevAux arr off len =
  match len with
  | 0 -> None
  | 1 -> singleton (A.getUnsafe arr off)
  | 2 ->
    let x0,x1 = A.(getUnsafe arr off, getUnsafe arr (off - 1) )
    in
    Some { left = (singleton x0) ; value = x1; height = 2; right = None}
  | 3 ->
    let x0,x1,x2 =
      A.(getUnsafe arr off,
         getUnsafe arr (off - 1),
         getUnsafe arr (off - 2)) in
    Some { left = (singleton x0);
      right = (singleton x2);
      value = x1;
      height = 2}
  | _ ->
    let nl = len / 2 in
    let left = fromSortedArrayRevAux arr off nl in
    let mid = A.getUnsafe arr (off - nl) in
    let right =
      fromSortedArrayRevAux arr (off - nl - 1) (len - nl - 1) in
    create left mid right


let rec fromSortedArrayAux arr off len =
  match len with
  | 0 -> None
  | 1 -> singleton (A.getUnsafe arr off)
  | 2 ->
    let x0,x1 = A.(getUnsafe arr off, getUnsafe arr (off + 1) )
    in
    Some { left = (singleton x0); value = x1; height = 2; right = None}
  | 3 ->
    let x0,x1,x2 =
      A.(getUnsafe arr off,
         getUnsafe arr (off + 1),
         getUnsafe arr (off + 2)) in
    Some { left = (singleton x0);
      right = (singleton x2);
      value = x1;
      height = 2}
  | _ ->
    let nl = len / 2 in
    let left = fromSortedArrayAux arr off nl in
    let mid = A.getUnsafe arr (off + nl) in
    let right =
      fromSortedArrayAux arr (off + nl + 1) (len - nl - 1) in
    create left mid right

let fromSortedArrayUnsafe arr =
  fromSortedArrayAux arr 0 (A.length arr)

let rec keepSharedU n p =
  match n with
  | None -> None
  | Some n  ->
    let {left = l; value = v; right = r} = n in
    let newL = keepSharedU l p in
    let pv = p v [@bs] in
    let newR = keepSharedU r p in
    if pv then
      (if l == newL && r == newR then
         Some n
       else joinShared newL v newR)
    else concatShared newL newR

let keepShared n p = keepSharedU  n (fun [@bs] a -> p a)
(* ATT: functional methods in general can be shared with
    imperative methods, however, it does not apply when functional
    methods makes use of referential equality
*)

let keepCopyU n p : _ t =
  match n with
  | None -> None
  | Some n ->
    let size = lengthNode n in
    let  v = A.makeUninitializedUnsafe size in
    let last =
      fillArrayWithFilter n 0 v p in
    fromSortedArrayAux v 0 last

let keepCopy n p = keepCopyU n (fun [@bs] x -> p x)

let partitionCopyU n p  =
  match n with
  | None -> None, None
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    let backward = size - 1 in
    let cursor = { forward = 0; backward} in
    fillArrayWithPartition n cursor v p ;
    let forwardLen = cursor.forward in
    fromSortedArrayAux v 0 forwardLen,
    fromSortedArrayRevAux v backward (size  - forwardLen)

let partitionCopy n p = partitionCopyU n (fun[@bs] a -> p a)

let rec has   (t: _ t) x ~cmp =
  match  t with
  | None -> false
  | Some n ->
    let v = n.value in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    c = 0 || has ~cmp (if c < 0 then n.left else n.right) x


let rec compareAux e1 e2 ~cmp =
  match e1,e2 with
  | h1::t1, h2::t2 ->
    let c = (Belt_Id.getCmpInternal cmp) h1.value h2.value [@bs] in
    if c = 0 then
      compareAux ~cmp
        (h1 .right |. stackAllLeft  t1)
        (h2 .right |. stackAllLeft  t2)
    else c
  | _, _ -> 0

let cmp s1 s2 ~cmp =
  let len1,len2 = size s1, size s2 in
  if len1 = len2 then
    compareAux ~cmp (stackAllLeft s1 []) (stackAllLeft s2 [])
  else if len1 < len2 then -1 else 1


let eq s1 s2 ~cmp:c =
  cmp ~cmp:c s1 s2 = 0


let rec subset (s1 : _ t) (s2 : _ t) ~cmp  =
  match (s1, s2) with
  | None, _ -> true
  | _, None -> false
  | Some t1 , Some t2  ->
    let {left = l1; value = v1; right = r1} = t1  in
    let {left = l2; value = v2; right = r2} = t2  in
    let c = (Belt_Id.getCmpInternal cmp) v1 v2 [@bs] in
    if c = 0 then
      subset ~cmp l1 l2 && subset ~cmp r1 r2
    else if c < 0 then
      subset ~cmp (create l1 v1 None) l2 &&
      subset ~cmp r1 s2
    else
      subset ~cmp (create None v1 r1 ) r2 &&
      subset ~cmp l1 s2

let rec get  (n : _ t) x ~cmp =
  match n with
    None -> None
  | Some t (* Node(l, v, r, _) *) ->
    let v = t.value in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then Some v
    else get ~cmp  (if c < 0 then t.left else t.right) x


let rec getUndefined (n : _ t) x ~cmp  =
  match n with
    None -> Js.Undefined.empty
  | Some t (* Node(l, v, r, _) *) ->
    let v = t.value in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then  Js.Undefined.return v
    else getUndefined ~cmp  (if c < 0 then t.left else t.right) x

let rec getExn  (n : _ t) x ~cmp =
  match n with
    None -> raise Not_found
  | Some t (* Node(l, v, r, _) *) ->
    let v = t.value in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then  v
    else getExn ~cmp  (if c < 0 then t.left else t.right) x


(******************************************************************)

(*
  L rotation, Some root node
*)
let rotateWithLeftChild k2 =
  match k2 .left with 
  | None -> assert false 
  | Some k1 -> 
    k2 .left <- k1 .right;
    k1 .right <-  Some k2 ;
    let hlk2, hrk2 = k2 .left|. height , k2 .right |. height in
    k2 .height <-  (Pervasives.max hlk2 hrk2 + 1);
    let hlk1, hk2 = k1 .left|. height , k2 .height  in
    k1 .height <-  (Pervasives.max hlk1 hk2 + 1);
    k1
(* right rotation *)
let rotateWithRightChild k1 =
  match k1 .right with None -> assert false 
  | Some k2 -> 
  k1 .right <- k2 .left;
  k2 .left <-  Some k1;
  let hlk1, hrk1 = k1.left |. height, k1 .right |. height in
  k1 .height <-   (Pervasives.max  hlk1 hrk1 + 1);
  let hrk2, hk1 = k2 .right |. height, k1 .height in
  k2 .height <-  (Pervasives.max  hrk2 hk1 + 1);
  k2

(*
  double l rotation
*)
let doubleWithLeftChild k3 =
  match k3.left with 
  | None -> assert false 
  | Some k3l ->   
    let v = k3l  |. rotateWithRightChild |. Some in
    k3 .left <-  v;
    k3 |. rotateWithLeftChild
(** *)

let doubleWithRightChild k2 =
  match k2.right with 
  | None -> assert false 
  | Some k2r ->   
    let v = k2r |. rotateWithLeftChild |. Some in
    k2 .right <-  v;
    rotateWithRightChild k2

let heightUpdateMutate t =
  let hlt, hrt = t .left|. height, t .right |. height  in
  t .height <-  (Pervasives.max hlt hrt  + 1);
  t

let balMutate nt  =
  let {left = l; right = r} = nt  in
  let hl, hr =  (height l, height r) in
  if hl > 2 +  hr then
    match l with None -> assert false 
    | Some {left = ll; right = lr} -> 
    (if heightGe ll lr then
       heightUpdateMutate (rotateWithLeftChild nt)
     else
       heightUpdateMutate (doubleWithLeftChild nt)
    )
  else
  if hr > 2 + hl  then
    match r with None -> assert false 
    | Some {left = rl; right = rr} -> 
    (if heightGe rr rl then
       heightUpdateMutate (rotateWithRightChild nt)
     else
       heightUpdateMutate (doubleWithRightChild nt)
    )
  else
    begin
      nt.height <- (Pervasives.max hl hr + 1);
      nt
    end

let rec addMutate ~cmp (t : _ t) x =
  match t with
  | None -> singleton x
  | Some nt ->
    let k = nt.value in
    let  c = (Belt_Id.getCmpInternal cmp) x k [@bs] in
    if c = 0 then t
    else
      let {left = l; right = r} = nt  in
      (if c < 0 then
         let ll = addMutate ~cmp l x in
         nt.left <- ll
       else
         nt.right <- (addMutate ~cmp r x);
      );
      Some (balMutate nt)


let fromArray (xs : _ array) ~cmp =
  let len = A.length xs in
  if len = 0 then None
  else
    let next = ref (S.strictlySortedLengthU xs
      (fun [@bs] x y -> (Belt_Id.getCmpInternal cmp) x y [@bs] < 0)) in
    let result =
      ref (if next.contents >= 0 then
        fromSortedArrayAux xs 0 next.contents
      else begin
        next .contents<- - next.contents ;
        fromSortedArrayRevAux xs (next.contents - 1) next.contents
      end)  in
    for i = next.contents to len - 1 do
      result .contents<- addMutate ~cmp result.contents (A.getUnsafe xs i)
    done ;
    result.contents

let rec removeMinAuxWithRootMutate nt n =
  let {right = rn; left = ln} = n  in
  match ln with
  | None ->
    nt.value <- (n.value);
    rn
  | Some ln ->
    n.left <- (removeMinAuxWithRootMutate nt ln);
    Some (balMutate n)


