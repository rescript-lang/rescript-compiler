
type 'elt node  = {
  mutable left : 'elt node Js.null;
  mutable key : 'elt ; 
  mutable right : 'elt node Js.null;
  mutable h : int 
}
[@@bs.deriving abstract]


external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"

type ('elt, 'id) t0 =  'elt node Js.null
(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)

type ('elt, 'id)enumeration0 = 
  | End 
  | More of 'elt * ('elt, 'id) t0 * ('elt, 'id) enumeration0    

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
(* FIXME: bal should return [node] instead of [_ t0] *)
let bal l v r =
  let hl = match toOpt l with None -> 0 | Some n -> h n in
  let hr = match toOpt r with None -> 0 | Some n -> h n in
  if hl > hr + 2 then begin
    match toOpt l with
    | None -> assert false
    | Some n (* Node(ll, lv, lr, _) *) ->
      let ll,lv,lr = left n, key n, right n in 
      if heightGe ll  lr then
        create ll lv (create lr v r)
      else begin
        match toOpt lr with
          None -> assert false
        | Some n (* (lrl, lrv, lrr, _) *) ->
          let lrl, lrv, lrr = left n, key n, right n in 
          create (create ll lv lrl) lrv (create lrr v r)
      end
  end else if hr > hl + 2 then begin
    match toOpt r with
      None -> assert false
    | Some n (* (rl, rv, rr, _) *) ->
      let rl,rv,rr = left n, key n, right n in 
      if heightGe rr  rl then
        create (create l v rl) rv rr
      else begin
        match toOpt rl with
          None -> assert false
        | Some n (* (rll, rlv, rlr, _)*) ->
          let rll, rlv, rlr = left n, key n, right n in 
          create (create l v rll) rlv (create rlr rv rr)
      end
  end else
    return @@ node ~left:l ~key:v ~right:r ~h:(if hl >= hr then hl + 1 else hr + 1)

let singleton0 x = return @@ node ~left:empty ~key:x ~right:empty ~h:1

(* Beware: those two functions assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree; it
   does not test for equality with the current min (or max) element.
   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_element v n =
  match toOpt n with 
  | None -> singleton0 v
  | Some n (* (l, x, r, h)*) ->
    bal (add_min_element v (left n))  (key n) (right n)

let rec add_max_element v n = 
  match toOpt n with 
  | None -> singleton0 v
  | Some n (* (l, x, r, h)*) ->
    bal (left n) (key n) (add_max_element v (right n))

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join ln v rn =
  match (toOpt ln, toOpt rn) with
    (None, _) -> add_min_element v rn (* could be inlined *)
  | (_, None) -> add_max_element v ln (* could be inlined *)
  | Some l, Some r ->   
    let lh = h l in     
    let rh = h r in 
    if lh > rh + 2 then bal (left l) (key l) (join (right l) v rn) else
    if rh > lh + 2 then bal (join ln v (left r)) (key r) (right r) else
      create ln v rn

(* Smallest and greatest element of a set *)
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

let  maxOpt0 n = 
  match toOpt n with 
  | None -> None
  | Some n -> Some (max0Aux n)

let maxNull0 n =   
  match toOpt n with 
  | None -> Js.null
  | Some n -> return (max0Aux n)
(* Remove the smallest element of the given set *)

let rec removeMinAux n = 
  let rn, ln = right n, left n  in 
  match toOpt ln with   
  | None -> rn
  | Some ln -> bal (removeMinAux ln) (key n) rn


(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (toOpt t1, toOpt t2) with
    (None, _) -> t2
  | (_, None) -> t1
  | (_, Some t2n) -> bal t1 (min0Aux t2n) (removeMinAux t2n)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (toOpt t1, toOpt t2) with
    (None, _) -> t2
  | (_, None) -> t1
  | (_, Some t2n) -> join t1 (min0Aux t2n) (removeMinAux t2n)

(* Implementation of the set operations *)

let empty0 = empty

let isEmpty0 n = match toOpt n with Some _ -> false | None -> true 

let rec toEnum s e =
  match toOpt s with
    None -> e
  | Some n 
    -> toEnum (left n) (More( key n, right n, e))


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

let rec filter0 n p =
  match toOpt n with 
  | None -> empty
  | Some n  ->
    (* call [p] in the expected left-to-right order *)
    let newL = filter0 (left n) p in
    let v = key n in 
    let pv = p v [@bs] in
    let newR = filter0 (right n) p in
    if pv then join newL v newR else concat newL newR

let rec partition0  n p =
  match toOpt n with 
  |  None -> (empty, empty)
  | Some n  ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition0 (left n) p in
    let v = key n in 
    let pv = p v [@bs] in
    let (rt, rf) = partition0 (right n) p in
    if pv
    then (join lt v rt, concat lf rf)
    else (concat lt rt, join lf v rf)

let rec cardinalAux n = 
  let l, r = left n, right n in  
  let sizeL = 
    match toOpt l with 
    | None -> 0
    | Some l -> 
      cardinalAux l  in 
  let sizeR = 
    match toOpt r with 
    | None -> 0
    | Some r -> cardinalAux r in 
  1 + sizeL + sizeR  

let rec length0 n =
  match toOpt n with 
  | None -> 0
  | Some n  ->
    cardinalAux n 

let rec elements_aux accu n = 
  match toOpt n with 
  | None -> accu
  | Some n  ->
    let l,k,r = left n, key n, right n in 
    elements_aux 
      (k :: elements_aux accu r)
      l

let toList0 s =
  elements_aux [] s

let rec checkInvariant (v : _ t0) = 
  match toOpt v with 
  | None -> true 
  | Some n -> 
    let l,r = left n , right n in 
    let diff = height l - height r  in 
    diff <=2 && diff >= -2 && checkInvariant l && checkInvariant r 

module A = Bs_Array

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

(* TODO: binary search tree to array efficiency
*)
let toArray0 n =   
  match toOpt n with 
  | None -> [||]
  | Some n ->  
    let size = cardinalAux n in 
    let v = Bs.Array.makeUninitializedUnsafe size in 
    ignore (fillArray n 0 v : int);  (* may add assertion *)
    v 



external unsafeCoerce : 'a Js.null -> 'a = "%identity"

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

(* let rec removeMinAuxMutate n = 
   let rn, ln = right n, left n in 
   match toOpt ln with 
   | None -> rn 
   | Some ln -> 
    leftSet n (removeMinAuxMutate ln); 
    return (balMutate n) *)



let rec removeMinAuxMutateWithRoot nt n = 
  let rn, ln = right n, left n in 
  match toOpt ln with 
  | None -> 
    keySet nt (key n);
    rn 
  | Some ln -> 
    leftSet n (removeMinAuxMutateWithRoot nt ln); 
    return (balMutate n)    


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


    
let ofSortedArrayUnsafe0 arr =     
  ofSortedArrayAux arr 0 (A.length arr)