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
(** Almost rewritten  by authors of ReScript                       *)

[@@@bs.config {flags = [|"-bs-noassertfalse" |] }]
type ('k, 'v) node  = {
  mutable key : 'k; [@bs.as "k"]
  mutable value : 'v; [@bs.as "v"]
  mutable height : int; [@bs.as "h"]
  mutable left : ('k,'v) t; [@bs.as "l"]
  mutable right : ('k,'v) t [@bs.as "r"]
}
and ('key, 'a) t = ('key, 'a) node option


type ('k, 'id) cmp = ('k, 'id) Belt_Id.cmp

module A = Belt_Array
module S = Belt_SortArray


let treeHeight (n : _ t) =
  match  n with
    None -> 0
  | Some n -> n.height

let rec copy n =
  match  n with
  | None -> n
  | Some n ->    
    Some { n with left = copy n.left;  right = copy n.right}

let create l x d r =
  let hl, hr  = treeHeight l,  treeHeight r in
  Some { left = l ; key = x ; value = d ; right = r ; height = (if hl >= hr then hl + 1 else hr + 1)}

let singleton x d =
  Some { left = None; key = x;  value = d; right = None; height = 1}

let heightGe l r =
  match  l,  r with
  | _, None -> true
  | Some hl, Some hr -> hl.height >= hr.height
  | None, Some _ -> false

let updateValue n newValue =
  if n.value == newValue then n
  else
    { left = n.left;
      right = n.right;
      key = n.key;
      value = newValue;
      height = n.height }

let bal l x d r =
  let hl = match  l with None -> 0 | Some n -> n.height in
  let hr = match  r with None -> 0 | Some n -> n.height in
  if hl > hr + 2 then 
    match l with None -> assert false  | Some ({left = ll;   right = lr} as l) ->
      if treeHeight ll >= treeHeight lr then
        create ll l.key l.value (create lr x d r)
      else 
        match lr with None -> assert false | Some lr -> 
          create (create ll l.key l.value lr.left) lr.key lr.value (create lr.right x d r)
  else if hr > hl + 2 then
    match r with None -> assert false | Some ({left = rl; right = rr} as r) -> 
      if treeHeight rr >= treeHeight rl then
        create (create l x d rl) r.key r.value rr
      else
        match rl with None -> assert false | Some rl ->
          create (create l x d rl.left) rl.key rl.value (create rl.right r.key r.value rr)
  else
    Some { left = l; key = x ; value = d ; right = r ; height = (if hl >= hr then hl + 1 else hr + 1)}


let rec minKey0Aux n =
  match  n.left with
  | None -> n.key 
  | Some n -> minKey0Aux n

let minKey n =
  match  n with
  | None -> None
  | Some n -> Some (minKey0Aux n)

let minKeyUndefined n =
  match  n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (minKey0Aux n)

let rec maxKey0Aux n =
  match  n.right with
  | None -> n.key 
  | Some n -> maxKey0Aux n

let maxKey n =
  match  n with
  | None -> None
  | Some n -> Some (maxKey0Aux n)

let maxKeyUndefined n =
  match  n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (maxKey0Aux n)

let rec minKV0Aux n =
  match  n.left with
  | None -> n.key  , n.value
  | Some n -> minKV0Aux n

let minimum n =
  match  n with
    None -> None
  | Some n -> Some (minKV0Aux n)

let minUndefined n =
  match  n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (minKV0Aux n)

let rec maxKV0Aux n =
  match  n.right with
  | None -> n.key , n.value
  | Some n -> maxKV0Aux n

let maximum n =
  match  n with
  | None -> None
  | Some n -> Some (maxKV0Aux n)

let maxUndefined n =
  match  n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (maxKV0Aux n)

(* TODO: use kv ref *)
let rec removeMinAuxWithRef n kr vr =
  match n.left with
  | None ->  kr .contents<- n.key; vr.contents<- n.value; n.right
  | Some ln -> bal (removeMinAuxWithRef ln kr vr) n.key n.value n.right



let isEmpty x = match  x with None -> true | Some _ -> false

let rec stackAllLeft v s =
  match  v with
  | None -> s
  | Some x -> stackAllLeft x.left (x::s)

let rec findFirstByU n p =
  match  n with
  | None -> None 
  | Some n ->
    let left =  findFirstByU n.left p in
    if left <> None then left
      else
        let  {key = v; value =  d} = n  in
        let pvd = p v d [@bs] in
        if pvd then Some(v, d)
          else 
            let right =  findFirstByU n.right p in
            if right <> None then right else None

let findFirstBy n p = findFirstByU n (fun [@bs] a b -> p a b)

let rec forEachU n f =
  match  n with
  | None -> ()
  | Some n ->
    forEachU  n.left f ;
    f n.key n.value [@bs];
    forEachU n.right f

let forEach n f = forEachU n (fun [@bs] a b -> f a b)

let rec mapU n f =
  match  n with
    None  ->
    None
  | Some n  ->
    let newLeft =  mapU n.left f in
    let newD = f n.value [@bs] in
    let newRight = mapU n.right f in
    Some { left = newLeft; key = n.key;  value = newD; right = newRight; height = n.height}

let map n f = mapU n (fun[@bs] a -> f a)

let rec mapWithKeyU n f =
  match  n with
    None ->
    None
  | Some n ->
    let key = n.key  in
    let newLeft =  mapWithKeyU n.left f in
    let newD = f key n.value [@bs] in
    let newRight =  mapWithKeyU n.right f in
    Some { left = newLeft; key; value = newD; right = newRight; height = n.height}

let mapWithKey n f = mapWithKeyU n (fun [@bs] a b -> f a b)

let rec reduceU m accu f =
  match  m with
    None -> accu
  | Some n  ->
    let {left = l; key =  v; value = d; right = r} = n  in
    reduceU
      r
      (f (reduceU l accu f) v d [@bs]) f

let reduce m accu f = reduceU m accu (fun [@bs] a b c -> f a b c)

let rec everyU  n p =
  match  n with
    None -> true
  | Some n  ->
    p n.key n.value [@bs] &&
    everyU n.left  p &&
    everyU n.right p
let every n p = everyU n (fun [@bs] a b -> p a b)

let rec someU n p =
  match  n with
    None -> false
  | Some n  ->
    p n.key n.value [@bs] ||
    someU n.left p ||
    someU n.right p
let some n p  = someU n (fun[@bs] a b -> p a b)
(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec addMinElement n k v  =
  match  n with
  | None -> singleton k v
  | Some n
    ->
    bal (addMinElement n.left k v ) n.key n.value n.right

let rec addMaxElement n k v  =
  match  n with
  | None -> singleton k v
  | Some n
    ->
    bal n.left n.key n.value (addMaxElement n.right k v )

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join ln v d rn =
  match ( ln,  rn) with
    (None, _) -> addMinElement rn v d
  | (_, None) -> addMaxElement ln v d
  | Some l, Some r  ->
    let {left = ll; key = lv; value =  ld; right = lr; height =  lh} = l  in
    let {left = rl; key = rv; value = rd; right = rr; height = rh} = r  in
    if lh > rh + 2 then bal ll lv ld (join lr v d rn) else
    if rh > lh + 2 then bal (join ln v d rl) rv rd rr else
      create ln v d rn

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match ( t1,  t2) with
    (None, _) -> t2
  | (_, None) -> t1
  | (_, Some t2n) ->
    let kr, vr = ref t2n.key, ref t2n.value in
    let t2r = removeMinAuxWithRef t2n kr vr in
    join t1 kr.contents vr.contents t2r

let concatOrJoin t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

let rec keepSharedU n p =
  match  n with
    None -> None
  | Some n  ->
    (* call `p` in the expected left-to-right order *)
    let  {key = v; value = d} =  n   in
    let newLeft =  keepSharedU n.left p in
    let pvd = p v d [@bs] in
    let newRight = keepSharedU n.right p in
    if pvd then join newLeft v d newRight else concat newLeft newRight

let keepShared n p = keepSharedU n (fun [@bs] a b -> p a b)

let rec keepMapU n p =
  match  n with
    None -> None
  | Some n  ->
    (* call `p` in the expected left-to-right order *)
    let  {key = v; value = d} =  n  in
    let newLeft = keepMapU n.left  p in
    let pvd = p v d [@bs] in
    let newRight = keepMapU n.right p in
    match pvd with
    | None -> concat newLeft newRight
    | Some d -> join newLeft v d newRight

let keepMap n p = keepMapU n (fun[@bs] a b -> p a b)

let rec partitionSharedU n p =
  match  n with
    None -> (None, None)
  | Some n  ->
    let  {key; value } =  n  in
    (* call `p` in the expected left-to-right order *)
    let (lt, lf) =  partitionSharedU n.left p in
    let pvd = p key value [@bs] in
    let (rt, rf) = partitionSharedU n.right p in
    if pvd
    then (join lt key value rt, concat lf rf)
    else (concat lt rt, join lf key value rf)

let partitionShared n p = partitionSharedU n (fun [@bs] a b -> p a b)

let rec lengthNode n =
  let {left = l; right = r } = n  in
  let sizeL =
    match  l with
    | None -> 0
    | Some l ->
      lengthNode l  in
  let sizeR =
    match  r with
    | None -> 0
    | Some r -> lengthNode r in
  1 + sizeL + sizeR

let size n =
  match  n with
  | None -> 0
  | Some n  ->
    lengthNode n

let rec toListAux n accu =
  match  n with
  | None -> accu
  | Some n  ->
    let {left = l; right =  r ; key = k; value = v} = n in
    toListAux l ((k, v) :: (toListAux r accu ))

let toList s =
  toListAux  s []


let rec checkInvariantInternal (v : _ t) =
  match  v with
  | None -> ()
  | Some n ->
    let l,r = n.left , n.right in
    let diff = treeHeight l - treeHeight r  in
    assert (diff <=2 && diff >= -2 );
    checkInvariantInternal l;
    checkInvariantInternal r


let rec fillArrayKey n i arr =
  let {left = l; key = v; right  = r} = n  in
  let next =
    match  l with
    | None -> i
    | Some l ->
      fillArrayKey l i arr in
  A.setUnsafe arr next v;
  let rnext = next + 1 in
  match  r with
  | None -> rnext
  | Some r ->
    fillArrayKey r rnext arr

let rec fillArrayValue n i arr =
  let l,r = n.left,  n.right in
  let next =
    match  l with
    | None -> i
    | Some l ->
      fillArrayValue l i arr in
  A.setUnsafe arr next n.value;
  let rnext = next + 1 in
  match  r with
  | None -> rnext
  | Some r ->
    fillArrayValue r rnext arr

let rec fillArray n i arr =
  let l,v,r = n.left, n.key , n.right in
  let next =
    match  l with
    | None -> i
    | Some l ->
      fillArray l i arr in
  A.setUnsafe arr next (v, n.value) ;
  let rnext = next + 1 in
  match  r with
  | None -> rnext
  | Some r ->
    fillArray r rnext arr



(* let rec fillArrayWithPartition n cursor arr p =
  let l,v,r = n.left, n.key , n.right in
  (match  l with
   | None -> ()
   | Some l ->
     fillArrayWithPartition l cursor arr p);
  (if p v [@bs] then begin
      let c = forwardGet cursor in
      A.setUnsafe arr c (v,n.value);
      forwardSet cursor (c + 1)
    end
   else begin
     let c = backwardGet cursor in
     A.setUnsafe arr c (v, n.value);
     backwardSet cursor (c - 1)
   end);
  match  r with
  | None -> ()
  | Some r ->
    fillArrayWithPartition r cursor arr  p

let rec fillArrayWithFilter n i arr p =
  let l,v,r = n.left, n.key , n.right in
  let next =
    match  l with
    | None -> i
    | Some l ->
      fillArrayWithFilter l i arr p in
  let rnext =
    if p v [@bs] then
      (A.setUnsafe arr next (v, n.value);
       next + 1
      )
    else next in
  match  r with
  | None -> rnext
  | Some r ->
    fillArrayWithFilter r rnext arr  p
 *)

let toArray n =
  match  n with
  | None -> [||]
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    ignore (fillArray n 0 v : int);  (* may add assertion *)
    v

let keysToArray n =
  match  n with
  | None -> [||]
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    ignore (fillArrayKey n 0 v : int);  (* may add assertion *)
    v

let valuesToArray n =
  match  n with
  | None -> [||]
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    ignore (fillArrayValue n 0 v : int);  (* may add assertion *)
    v

let rec fromSortedArrayRevAux arr off len =
  match len with
  | 0 -> None
  | 1 -> let k, v = (A.getUnsafe arr off) in singleton k v
  | 2 ->
    let (x0,y0),(x1,y1) = A.(getUnsafe arr off, getUnsafe arr (off - 1) )
    in
    Some { left = (singleton x0 y0);  key = x1; value = y1; height = 2; right = None}
  | 3 ->
    let (x0,y0),(x1,y1),(x2,y2) =
      A.(getUnsafe arr off,
         getUnsafe arr (off - 1),
         getUnsafe arr (off - 2)) in
    Some { left = (singleton x0 y0);
      right = (singleton x2 y2);
      key = x1;
      value = y1;
      height = 2 }
  | _ ->
    let nl = len / 2 in
    let left = fromSortedArrayRevAux arr off nl in
    let midK,midV = A.getUnsafe arr (off - nl) in
    let right =
      fromSortedArrayRevAux arr (off - nl - 1) (len - nl - 1) in
    create left midK midV right


let rec fromSortedArrayAux arr off len =
  match len with
  | 0 -> None
  | 1 -> let k, v = (A.getUnsafe arr off) in singleton k v
  | 2 ->
    let (x0,y0),(x1,y1) = A.(getUnsafe arr off, getUnsafe arr (off + 1) )
    in
    Some {left = (singleton x0 y0);  key = x1 ; value = y1; height = 2; right = None}
  | 3 ->
    let (x0,y0),(x1,y1),(x2,y2) =
      A.(getUnsafe arr off,
         getUnsafe arr (off + 1),
         getUnsafe arr (off + 2)) in
    Some { left = (singleton x0 y0);
      right = (singleton x2 y2);
      key = x1; value = y1;
      height = 2}
  | _ ->
    let nl = len / 2 in
    let left = fromSortedArrayAux arr off nl in
    let midK, midV = A.getUnsafe arr (off + nl) in
    let right =
      fromSortedArrayAux arr (off + nl + 1) (len - nl - 1) in
    create left midK midV right

let fromSortedArrayUnsafe arr =
  fromSortedArrayAux arr 0 (A.length arr)

let rec compareAux e1 e2 ~kcmp ~vcmp =
  match e1,e2 with
  | h1::t1, h2::t2 ->
    let c = (Belt_Id.getCmpInternal kcmp) h1.key h2.key [@bs] in
    if c = 0 then
      let cx = vcmp h1.value h2.value [@bs] in
      if cx = 0 then
        compareAux ~kcmp ~vcmp
          (stackAllLeft  h1.right t1 )
          (stackAllLeft h2.right t2)
      else  cx
    else c
  | _, _ -> 0

let rec eqAux e1 e2 ~kcmp ~veq =
  match e1,e2 with
  | h1::t1, h2::t2 ->
    if (Belt_Id.getCmpInternal kcmp) h1.key h2.key [@bs] = 0 &&
       veq h1.value h2.value [@bs] then
      eqAux ~kcmp ~veq (
        stackAllLeft  h1.right t1 ) (stackAllLeft h2.right t2)
    else  false
  | _, _ -> true

let cmpU s1 s2 ~kcmp ~vcmp =
  let len1,len2 = size s1, size s2 in
  if len1 = len2 then
    compareAux (stackAllLeft s1 []) (stackAllLeft s2 []) ~kcmp ~vcmp
  else  if len1 < len2 then -1 else 1

let cmp s1 s2 ~kcmp ~vcmp =
  cmpU s1 s2 ~kcmp ~vcmp:(fun[@bs] a b -> vcmp a b)

let eqU s1 s2 ~kcmp ~veq =
  let len1, len2 = size s1, size s2 in
  if len1 = len2 then
    eqAux (stackAllLeft s1 []) (stackAllLeft s2 []) ~kcmp ~veq
  else false

let eq s1 s2 ~kcmp ~veq =
  eqU s1 s2 ~kcmp ~veq:(fun [@bs] a b -> veq a b)

let rec get  n x ~cmp =
  match  n with
    None -> None
  | Some n (* Node(l, v, d, r, _) *)  ->
    let v = n.key  in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then Some n.value
    else get ~cmp  (if c < 0 then n.left else n.right) x

let rec getUndefined  n x ~cmp =
  match  n with
  | None -> Js.undefined
  | Some n  ->
    let v = n.key  in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then Js.Undefined.return (n.value )
    else getUndefined ~cmp  (if c < 0 then n.left else n.right) x

let rec getExn   n x  ~cmp =
  match  n with
    None ->
    raise Not_found
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = n.key  in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then n.value
    else getExn ~cmp  (if c < 0 then n.left else n.right) x

let rec getWithDefault   n x def ~cmp =
  match  n with
    None ->
    def
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = n.key  in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then n.value
    else getWithDefault ~cmp  (if c < 0 then n.left else n.right) x def

let rec has  n x ~cmp =
  match  n with
    None ->
    false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = n.key  in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    c = 0 || has ~cmp (if c < 0 then n.left else n.right) x


(******************************************************************)

(*
  L rotation, Some root node
*)
let rotateWithLeftChild k2 =
  match k2.left with None -> assert false
  | Some k1 ->
  (k2.left <- k1.right);
  (k1.right <- (Some k2 ));
  let hlk2, hrk2 = (treeHeight k2.left, (treeHeight k2.right)) in
  (k2.height <-
     (Pervasives.max hlk2 hrk2 + 1));
  let hlk1, hk2 = (treeHeight k1.left, k2.height) in
  (k1.height <- (Pervasives.max hlk1 hk2 + 1));
  k1
(* right rotation *)
let rotateWithRightChild k1 =
  match k1.right with None -> assert false
  | Some k2 ->
  (k1.right <- k2.left);
  (k2.left <- (Some k1));
  let hlk1, hrk1 = ((treeHeight k1.left), (treeHeight k1.right)) in
  (k1.height <- (Pervasives.max  hlk1 hrk1 + 1));
  let hrk2, hk1 = (treeHeight k2.right, k1.height) in
  (k2.height <- (Pervasives.max  hrk2 hk1 + 1));
  k2

(*
  double l rotation
*)
let doubleWithLeftChild k3 =
  let k3l = match k3.left with None -> assert false | Some x -> x in 
  let v = rotateWithRightChild k3l in
  (k3.left <- (Some v ));
  rotateWithLeftChild k3

let doubleWithRightChild k2 =
  let k2r = match k2.right with None -> assert false | Some x -> x in   
  let v = rotateWithLeftChild k2r in
  (k2.right <- (Some v));
  rotateWithRightChild k2

let heightUpdateMutate t =
  let hlt, hrt = (treeHeight t.left,(treeHeight t.right)) in
  t.height <- (Pervasives.max hlt hrt  + 1);
  t

let balMutate nt  =
  let l, r = (nt.left, nt.right) in
  let hl, hr =  (treeHeight l, treeHeight r) in
  if hl > 2 +  hr then
    match l with None -> assert false 
    | Some {left = ll;  right = lr} ->
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

let rec updateMutate (t : _ t) x data ~cmp =
  match  t with
  | None -> singleton x data
  | Some nt ->
    let k = nt.key in
    let  c = (Belt_Id.getCmpInternal cmp) x k [@bs] in
    if c = 0 then begin
      nt.value <- data;
      Some nt
    end
    else
      let l, r = (nt.left, nt.right) in
      (if c < 0 then
         let ll = updateMutate ~cmp l x data in
         nt.left <- ll
       else
         nt.right <- (updateMutate ~cmp r x data);
      );
      Some (balMutate nt)

let fromArray (xs : _ array) ~cmp =
  let len = A.length xs in
  if len = 0 then None
  else
    let next =
      ref (S.strictlySortedLengthU xs
             (fun[@bs] (x0,_) (y0,_) ->
                (Belt_Id.getCmpInternal cmp) x0 y0 [@bs] < 0
             ))
    in
    let result  = ref (
        if next.contents >= 0 then
          fromSortedArrayAux xs 0 next.contents
        else begin
          next .contents<- - next.contents;
          fromSortedArrayRevAux xs (next.contents - 1) (next.contents)
        end
      ) in
    for i = next.contents to len - 1 do
      let k, v = (A.getUnsafe xs i)  in
      result .contents<- updateMutate ~cmp result.contents k v
    done ;
    result.contents


let rec removeMinAuxWithRootMutate nt n =
  let rn, ln = n.right, n.left in
  match  ln with
  | None ->
    nt.key <- n.key;
    nt.value <- n.value;
    rn
  | Some ln ->
    n.left <- (removeMinAuxWithRootMutate nt ln);
    Some (balMutate n)
