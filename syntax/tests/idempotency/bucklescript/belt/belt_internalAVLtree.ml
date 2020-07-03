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
  mutable key : 'k;
  mutable value : 'v;
  mutable height : int;
  mutable left : ('k,'v) t;
  mutable right : ('k,'v) t
}
and ('key, 'a) t = ('key, 'a) node Js.null
[@@bs.deriving abstract]

type ('k, 'id) cmp = ('k, 'id) Belt_Id.cmp

module A = Belt_Array
module S = Belt_SortArray
external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"
external unsafeCoerce : 'a Js.null -> 'a = "%identity"



let treeHeight (n : _ t) =
  match toOpt n with
    None -> 0
  | Some n -> heightGet n

let rec copy n =
  match toOpt n with
  | None -> n
  | Some n ->
    let l,r = n |. (leftGet , rightGet) in
    return @@ node ~left:(copy l) ~right:(copy r) ~value:(valueGet n) ~key:(keyGet n) ~height:(heightGet n)

let create l x d r =
  let hl, hr  = treeHeight l,  treeHeight r in
  return @@ node ~left:l ~key:x ~value:d ~right:r ~height:(if hl >= hr then hl + 1 else hr + 1)

let singleton x d =
  return @@ node ~left:empty ~key:x ~value:d ~right:empty ~height:1

let heightGe l r =
  match toOpt l, toOpt r with
  | _, None -> true
  | Some hl, Some hr -> heightGet hl >= heightGet hr
  | None, Some _ -> false

let updateValue n newValue =
  if valueGet n == newValue then n
  else
    node
    ~left:(leftGet n)
    ~right:(rightGet n)
    ~key:(keyGet n)
    ~value:newValue
    ~height:(heightGet n)

let bal l x d r =
  let hl = match toOpt l with None -> 0 | Some n -> heightGet n in
  let hr = match toOpt r with None -> 0 | Some n -> heightGet n in
  if hl > hr + 2 then begin
    let ll,lv,ld,lr =
      l |. unsafeCoerce |. (leftGet , keyGet , valueGet, rightGet)
    in
    if treeHeight ll >= treeHeight lr then
      create ll lv ld (create lr x d r)
    else begin
      let lrl, lrv, lrd,lrr = lr |. unsafeCoerce |. (leftGet, keyGet, valueGet, rightGet) in
      create (create ll lv ld lrl) lrv lrd (create lrr x d r)
    end
  end else if hr > hl + 2 then begin
    let rl, rv, rd, rr = r |. unsafeCoerce |. (leftGet, keyGet , valueGet, rightGet) in
    if treeHeight rr >= treeHeight rl then
      create (create l x d rl) rv rd rr
    else begin
      let rll, rlv,rld,rlr = rl |. unsafeCoerce |. (leftGet, keyGet, valueGet, rightGet) in
      create (create l x d rll) rlv rld (create rlr rv rd rr)
    end
  end else
    return @@ node ~left:l ~key:x ~value:d ~right:r ~height:(if hl >= hr then hl + 1 else hr + 1)


let rec minKey0Aux n =
  match toOpt (leftGet n) with
  | None -> keyGet n
  | Some n -> minKey0Aux n

let minKey n =
  match toOpt n with
  | None -> None
  | Some n -> Some (minKey0Aux n)

let minKeyUndefined n =
  match toOpt n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (minKey0Aux n)

let rec maxKey0Aux n =
  match toOpt (rightGet n) with
  | None -> keyGet n
  | Some n -> maxKey0Aux n

let maxKey n =
  match toOpt n with
  | None -> None
  | Some n -> Some (maxKey0Aux n)

let maxKeyUndefined n =
  match toOpt n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (maxKey0Aux n)

let rec minKV0Aux n =
  match toOpt (leftGet n) with
  | None -> keyGet n , valueGet n
  | Some n -> minKV0Aux n

let minimum n =
  match toOpt n with
    None -> None
  | Some n -> Some (minKV0Aux n)

let minUndefined n =
  match toOpt n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (minKV0Aux n)

let rec maxKV0Aux n =
  match toOpt (rightGet n) with
  | None -> keyGet n, valueGet n
  | Some n -> maxKV0Aux n

let maximum n =
  match toOpt n with
  | None -> None
  | Some n -> Some (maxKV0Aux n)

let maxUndefined n =
  match toOpt n with
  | None -> Js.undefined
  | Some n -> Js.Undefined.return (maxKV0Aux n)


let rec removeMinAuxWithRef n kr vr =
  let ln, rn, kn, vn = leftGet n, rightGet n, keyGet n, valueGet n in
  match toOpt ln with
  | None ->  kr .contents<- kn; vr .contents<- vn; rn
  | Some ln -> bal (removeMinAuxWithRef ln kr vr) kn vn rn



let isEmpty x = match toOpt x with None -> true | Some _ -> false

let rec stackAllLeft v s =
  match toOpt v with
  | None -> s
  | Some x -> stackAllLeft (leftGet x) (x::s)

let rec findFirstByU n p =
  match toOpt n with
  | None -> None 
  | Some n ->
    let left = n |. leftGet |. findFirstByU p in
    if left <> None then left
      else
        let  v, d = n |. (keyGet, valueGet) in
        let pvd = p v d [@bs] in
        if pvd then Some(v, d)
          else 
            let right = n |. rightGet |. findFirstByU  p in
            if right <> None then right else None

let findFirstBy n p = findFirstByU n (fun [@bs] a b -> p a b)

let rec forEachU n f =
  match toOpt n with
  | None -> ()
  | Some n ->
    n |. leftGet |. forEachU  f ;
    f (keyGet n) (valueGet n) [@bs];
    n |. rightGet |. forEachU  f

let forEach n f = forEachU n (fun [@bs] a b -> f a b)

let rec mapU n f =
  match toOpt n with
    None  ->
    empty
  | Some n  ->
    let newLeft = n |. leftGet |. mapU  f in
    let newD = f (valueGet n) [@bs] in
    let newRight = n |. rightGet |. mapU  f in
    return @@ node ~left:newLeft ~key:(keyGet n) ~value:newD ~right:newRight ~height:(heightGet n)

let map n f = mapU n (fun[@bs] a -> f a)

let rec mapWithKeyU n f =
  match toOpt n with
    None ->
    empty
  | Some n ->
    let key = keyGet n in
    let newLeft = n |. leftGet |. mapWithKeyU  f in
    let newD = f key (valueGet n) [@bs] in
    let newRight = n |. rightGet |. mapWithKeyU  f in
    return @@ node ~left:newLeft ~key ~value:newD ~right:newRight ~height:(heightGet n)

let mapWithKey n f = mapWithKeyU n (fun [@bs] a b -> f a b)

let rec reduceU m accu f =
  match toOpt m with
    None -> accu
  | Some n  ->
    let l, v, d, r = n |. (leftGet,  keyGet, valueGet, rightGet) in
    reduceU
      r
      (f (reduceU l accu f) v d [@bs]) f

let reduce m accu f = reduceU m accu (fun [@bs] a b c -> f a b c)

let rec everyU  n p =
  match toOpt n with
    None -> true
  | Some n  ->
    p (keyGet n) (valueGet n) [@bs] &&
    n |. leftGet  |. everyU  p &&
    n |. rightGet |. everyU  p
let every n p = everyU n (fun [@bs] a b -> p a b)

let rec someU n p =
  match toOpt n with
    None -> false
  | Some n  ->
    p (keyGet n) (valueGet n) [@bs] ||
    n |. leftGet  |. someU  p ||
    n |. rightGet |. someU p
let some n p  = someU n (fun[@bs] a b -> p a b)
(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec addMinElement n k v  =
  match toOpt n with
  | None -> singleton k v
  | Some n
    ->
    bal (addMinElement (leftGet n) k v ) (keyGet n) (valueGet n) (rightGet n)

let rec addMaxElement n k v  =
  match toOpt n with
  | None -> singleton k v
  | Some n
    ->
    bal (leftGet n) (keyGet n) (valueGet n) (addMaxElement (rightGet n) k v )

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join ln v d rn =
  match (toOpt ln, toOpt rn) with
    (None, _) -> addMinElement rn v d
  | (_, None) -> addMaxElement ln v d
  | Some l, Some r  ->
    let ll, lv, ld, lr, lh = l |. (leftGet , keyGet , valueGet , rightGet , heightGet) in
    let rl, rv, rd, rr, rh = r |. (leftGet , keyGet , valueGet , rightGet , heightGet) in
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
    let kr, vr = ref (keyGet t2n), ref (valueGet t2n) in
    let t2r = removeMinAuxWithRef t2n kr vr in
    join t1 kr.contents vr.contents t2r

let concatOrJoin t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

let rec keepSharedU n p =
  match toOpt n with
    None -> empty
  | Some n  ->
    (* call [p] in the expected left-to-right order *)
    let  v, d =  n |. (keyGet , valueGet)  in
    let newLeft = n |. leftGet |. keepSharedU  p in
    let pvd = p v d [@bs] in
    let newRight = n |. rightGet |. keepSharedU  p in
    if pvd then join newLeft v d newRight else concat newLeft newRight

let keepShared n p = keepSharedU n (fun [@bs] a b -> p a b)

let rec keepMapU n p =
  match toOpt n with
    None -> empty
  | Some n  ->
    (* call [p] in the expected left-to-right order *)
    let  v, d =  n |. (keyGet , valueGet)  in
    let newLeft = n |. leftGet |. keepMapU  p in
    let pvd = p v d [@bs] in
    let newRight = n |. rightGet |. keepMapU  p in
    match pvd with
    | None -> concat newLeft newRight
    | Some d -> join newLeft v d newRight

let keepMap n p = keepMapU n (fun[@bs] a b -> p a b)

let rec partitionSharedU n p =
  match toOpt n with
    None -> (empty, empty)
  | Some n  ->
    let  key, value =  n |. (keyGet , valueGet)  in
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = n |. leftGet |. partitionSharedU  p in
    let pvd = p key value [@bs] in
    let (rt, rf) = n |. rightGet |. partitionSharedU  p in
    if pvd
    then (join lt key value rt, concat lf rf)
    else (concat lt rt, join lf key value rf)

let partitionShared n p = partitionSharedU n (fun [@bs] a b -> p a b)

let rec lengthNode n =
  let l, r = n |. (leftGet , rightGet) in
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

let size n =
  match toOpt n with
  | None -> 0
  | Some n  ->
    lengthNode n

let rec toListAux n accu =
  match toOpt n with
  | None -> accu
  | Some n  ->
    let l, r , k, v = n |. (leftGet, rightGet, keyGet, valueGet ) in
    l |. toListAux ((k, v) :: ( r |. toListAux accu ))

let toList s =
  toListAux  s []


let rec checkInvariantInternal (v : _ t) =
  match toOpt v with
  | None -> ()
  | Some n ->
    let l,r = leftGet n , rightGet n in
    let diff = treeHeight l - treeHeight r  in
    [%assert diff <=2 && diff >= -2 ];
    checkInvariantInternal l;
    checkInvariantInternal r


let rec fillArrayKey n i arr =
  let l,v,r = n |. (leftGet, keyGet, rightGet) in
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
  let l,r = leftGet n,  rightGet n in
  let next =
    match toOpt l with
    | None -> i
    | Some l ->
      fillArrayValue l i arr in
  A.setUnsafe arr next (valueGet n);
  let rnext = next + 1 in
  match toOpt r with
  | None -> rnext
  | Some r ->
    fillArrayValue r rnext arr

let rec fillArray n i arr =
  let l,v,r = leftGet n, keyGet n, rightGet n in
  let next =
    match toOpt l with
    | None -> i
    | Some l ->
      fillArray l i arr in
  A.setUnsafe arr next (v, valueGet n) ;
  let rnext = next + 1 in
  match toOpt r with
  | None -> rnext
  | Some r ->
    fillArray r rnext arr

type cursor =
  { mutable forward : int; mutable backward : int } [@@bs.deriving abstract]

let rec fillArrayWithPartition n cursor arr p =
  let l,v,r = leftGet n, keyGet n, rightGet n in
  (match toOpt l with
   | None -> ()
   | Some l ->
     fillArrayWithPartition l cursor arr p);
  (if p v [@bs] then begin
      let c = forwardGet cursor in
      A.setUnsafe arr c (v,valueGet n);
      forwardSet cursor (c + 1)
    end
   else begin
     let c = backwardGet cursor in
     A.setUnsafe arr c (v, valueGet n);
     backwardSet cursor (c - 1)
   end);
  match toOpt r with
  | None -> ()
  | Some r ->
    fillArrayWithPartition r cursor arr  p

let rec fillArrayWithFilter n i arr p =
  let l,v,r = leftGet n, keyGet n, rightGet n in
  let next =
    match toOpt l with
    | None -> i
    | Some l ->
      fillArrayWithFilter l i arr p in
  let rnext =
    if p v [@bs] then
      (A.setUnsafe arr next (v, valueGet n);
       next + 1
      )
    else next in
  match toOpt r with
  | None -> rnext
  | Some r ->
    fillArrayWithFilter r rnext arr  p


let toArray n =
  match toOpt n with
  | None -> [||]
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    ignore (fillArray n 0 v : int);  (* may add assertion *)
    v

let keysToArray n =
  match toOpt n with
  | None -> [||]
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    ignore (fillArrayKey n 0 v : int);  (* may add assertion *)
    v

let valuesToArray n =
  match toOpt n with
  | None -> [||]
  | Some n ->
    let size = lengthNode n in
    let v = A.makeUninitializedUnsafe size in
    ignore (fillArrayValue n 0 v : int);  (* may add assertion *)
    v

let rec fromSortedArrayRevAux arr off len =
  match len with
  | 0 -> empty
  | 1 -> let k, v = (A.getUnsafe arr off) in singleton k v
  | 2 ->
    let (x0,y0),(x1,y1) = A.(getUnsafe arr off, getUnsafe arr (off - 1) )
    in
    return @@ node ~left:(singleton x0 y0) ~key:x1 ~value:y1 ~height:2 ~right:empty
  | 3 ->
    let (x0,y0),(x1,y1),(x2,y2) =
      A.(getUnsafe arr off,
         getUnsafe arr (off - 1),
         getUnsafe arr (off - 2)) in
    return @@ node ~left:(singleton x0 y0)
      ~right:(singleton x2 y2)
      ~key:x1
      ~value:y1
      ~height:2
  | _ ->
    let nl = len / 2 in
    let left = fromSortedArrayRevAux arr off nl in
    let midK,midV = A.getUnsafe arr (off - nl) in
    let right =
      fromSortedArrayRevAux arr (off - nl - 1) (len - nl - 1) in
    create left midK midV right


let rec fromSortedArrayAux arr off len =
  match len with
  | 0 -> empty
  | 1 -> let k, v = (A.getUnsafe arr off) in singleton k v
  | 2 ->
    let (x0,y0),(x1,y1) = A.(getUnsafe arr off, getUnsafe arr (off + 1) )
    in
    return @@ node ~left:(singleton x0 y0) ~key:x1 ~value:y1 ~height:2 ~right:empty
  | 3 ->
    let (x0,y0),(x1,y1),(x2,y2) =
      A.(getUnsafe arr off,
         getUnsafe arr (off + 1),
         getUnsafe arr (off + 2)) in
    return @@ node ~left:(singleton x0 y0)
      ~right:(singleton x2 y2)
      ~key:x1 ~value:y1
      ~height:2
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
    let c = (Belt_Id.getCmpInternal kcmp) (keyGet h1) (keyGet h2) [@bs] in
    if c = 0 then
      let cx = vcmp (valueGet h1) (valueGet h2) [@bs] in
      if cx = 0 then
        compareAux ~kcmp ~vcmp
          (stackAllLeft  (rightGet h1) t1 )
          (stackAllLeft (rightGet h2) t2)
      else  cx
    else c
  | _, _ -> 0

let rec eqAux e1 e2 ~kcmp ~veq =
  match e1,e2 with
  | h1::t1, h2::t2 ->
    if (Belt_Id.getCmpInternal kcmp) (keyGet h1) (keyGet h2) [@bs] = 0 &&
       veq (valueGet h1) (valueGet h2) [@bs] then
      eqAux ~kcmp ~veq (
        stackAllLeft  (rightGet h1) t1 ) (stackAllLeft (rightGet h2) t2)
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
  match toOpt n with
    None -> None
  | Some n (* Node(l, v, d, r, _) *)  ->
    let v = keyGet n in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then Some (valueGet n)
    else get ~cmp  (if c < 0 then leftGet n else rightGet n) x

let rec getUndefined  n x ~cmp =
  match toOpt n with
  | None -> Js.undefined
  | Some n  ->
    let v = keyGet n in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then Js.Undefined.return (valueGet n )
    else getUndefined ~cmp  (if c < 0 then leftGet n else rightGet n) x

let rec getExn   n x  ~cmp =
  match toOpt n with
    None ->
    [%assert "getExn0"]
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = keyGet n in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then valueGet n
    else getExn ~cmp  (if c < 0 then leftGet n else rightGet n) x

let rec getWithDefault   n x def ~cmp =
  match toOpt n with
    None ->
    def
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = keyGet n in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    if c = 0 then valueGet n
    else getWithDefault ~cmp  (if c < 0 then leftGet n else rightGet n) x def

let rec has  n x ~cmp =
  match toOpt n with
    None ->
    false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = keyGet n in
    let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
    c = 0 || has ~cmp (if c < 0 then leftGet n else rightGet n) x


(******************************************************************)

(*
  L rotation, return root node
*)
let rotateWithLeftChild k2 =
  let k1 = unsafeCoerce (leftGet k2) in
  (leftSet k2 (rightGet k1));
  (rightSet k1 (return k2 ));
  let hlk2, hrk2 = (treeHeight (leftGet k2), (treeHeight (rightGet k2))) in
  (heightSet k2
     (Pervasives.max hlk2 hrk2 + 1));
  let hlk1, hk2 = (treeHeight (leftGet k1), (heightGet k2)) in
  (heightSet k1 (Pervasives.max hlk1 hk2 + 1));
  k1
(* right rotation *)
let rotateWithRightChild k1 =
  let k2 = unsafeCoerce (rightGet k1) in
  (rightSet k1 (leftGet k2));
  (leftSet k2 (return k1));
  let hlk1, hrk1 = ((treeHeight (leftGet k1)), (treeHeight (rightGet k1))) in
  (heightSet k1 (Pervasives.max  hlk1 hrk1 + 1));
  let hrk2, hk1 = (treeHeight (rightGet k2), (heightGet k1)) in
  (heightSet k2 (Pervasives.max  hrk2 hk1 + 1));
  k2

(*
  double l rotation
*)
let doubleWithLeftChild k3 =
  let v = rotateWithRightChild (unsafeCoerce (leftGet k3)) in
  (leftSet k3 (return v ));
  rotateWithLeftChild k3

let doubleWithRightChild k2 =
  let v = rotateWithLeftChild (unsafeCoerce (rightGet k2)) in
  (rightSet k2 (return v));
  rotateWithRightChild k2

let heightUpdateMutate t =
  let hlt, hrt = (treeHeight (leftGet t),(treeHeight (rightGet t))) in
  heightSet t (Pervasives.max hlt hrt  + 1);
  t

let balMutate nt  =
  let l, r = (leftGet nt, rightGet nt) in
  let hl, hr =  (treeHeight l, treeHeight r) in
  if hl > 2 +  hr then
    let l = unsafeCoerce l in
    let ll, lr = (leftGet l , rightGet l)in
    (if heightGe ll lr then
       heightUpdateMutate (rotateWithLeftChild nt)
     else
       heightUpdateMutate (doubleWithLeftChild nt)
    )
  else
  if hr > 2 + hl  then
    let r = unsafeCoerce r in
    let rl,rr = (leftGet r, rightGet r) in
    (if heightGe rr rl then
       heightUpdateMutate (rotateWithRightChild nt)
     else
       heightUpdateMutate (doubleWithRightChild nt)
    )
  else
    begin
      heightSet nt (Pervasives.max hl hr + 1);
      nt
    end

let rec updateMutate (t : _ t) x data ~cmp =
  match toOpt t with
  | None -> singleton x data
  | Some nt ->
    let k = keyGet nt in
    let  c = (Belt_Id.getCmpInternal cmp) x k [@bs] in
    if c = 0 then begin
      valueSet nt data;
      return nt
    end
    else
      let l, r = (leftGet nt, rightGet nt) in
      (if c < 0 then
         let ll = updateMutate ~cmp l x data in
         leftSet nt ll
       else
         rightSet nt (updateMutate ~cmp r x data);
      );
      return (balMutate nt)

let fromArray (xs : _ array) ~cmp =
  let len = A.length xs in
  if len = 0 then empty
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
  let rn, ln = rightGet n, leftGet n in
  match toOpt ln with
  | None ->
    keySet nt (keyGet n);
    rn
  | Some ln ->
    leftSet n (removeMinAuxWithRootMutate nt ln);
    return (balMutate n)
