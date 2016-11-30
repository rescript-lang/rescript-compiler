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
(** balanced tree based on stdlib distribution *)

type 'a t = 
  | Empty 
  | Node of 'a t * 'a * 'a t * int 

type 'a enumeration = 
  | End | More of 'a * 'a t * 'a enumeration


let rec cons_enum s e = 
  match s with 
  | Empty -> e 
  | Node(l,v,r,_) -> cons_enum l (More(v,r,e))

let rec height = function
  | Empty -> 0 
  | Node(_,_,_,h) -> h   

(* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> raise Not_found
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> min_elt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node(l, v, Empty, _) -> v
  | Node(l, v, r, _) -> max_elt r




let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec cardinal = function
    Empty -> 0
  | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let rec iter f = function
  | Empty -> ()
  | Node(l, v, r, _) -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
  | Empty -> accu
  | Node(l, v, r, _) -> fold f r (f v (fold f l accu))

let rec for_all p = function
  | Empty -> true
  | Node(l, v, r, _) -> p v && for_all p l && for_all p r

let rec exists p = function
  | Empty -> false
  | Node(l, v, r, _) -> p v || exists p l || exists p r


let max_int3 (a : int) b c = 
  if a >= b then 
    if a >= c then a 
    else c
  else 
  if b >=c then b
  else c     
let max_int_2 (a : int) b =  
  if a >= b then a else b 



exception Height_invariant_broken
exception Height_diff_borken 

let rec check_height_and_diff = 
  function 
  | Empty -> 0
  | Node(l,_,r,h) -> 
    let hl = check_height_and_diff l in
    let hr = check_height_and_diff r in
    if h <>  max_int_2 hl hr + 1 then raise Height_invariant_broken
    else  
      let diff = (abs (hl - hr)) in  
      if  diff > 2 then raise Height_diff_borken 
      else h     

let check tree = 
  ignore (check_height_and_diff tree)
(* 
    Invariants: 
    1. {[ l < v < r]}
    2. l and r balanced 
    3. [height l] - [height r] <= 2
*)
let create l v r = 
  let hl = match l with Empty -> 0 | Node (_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node (_,_,_,h) -> h in
  Node(l,v,r, if hl >= hr then hl + 1 else hr + 1)         

(* Same as create, but performs one step of rebalancing if necessary.
    Invariants:
    1. {[ l < v < r ]}
    2. l and r balanced 
    3. | height l - height r | <= 3.

    Proof by indunction

    Lemma: the height of  [bal l v r] will bounded by [max l r] + 1 
*)
(*
let internal_bal l v r =
  match l with
  | Empty ->
    begin match r with 
      | Empty -> Node(Empty,v,Empty,1)
      | Node(rl,rv,rr,hr) -> 
        if hr > 2 then
          begin match rl with
            | Empty -> create (* create l v rl *) (Node (Empty,v,Empty,1)) rv rr 
            | Node(rll,rlv,rlr,hrl) -> 
              let hrr = height rr in 
              if hrr >= hrl then 
                Node  
                  ((Node (Empty,v,rl,hrl+1))(* create l v rl *),
                   rv, rr, if hrr = hrl then hrr + 2 else hrr + 1) 
              else 
                let hrll = height rll in 
                let hrlr = height rlr in 
                create
                  (Node(Empty,v,rll,hrll + 1)) 
                  (* create l v rll *) 
                  rlv 
                  (Node (rlr,rv,rr, if hrlr > hrr then hrlr + 1 else hrr + 1))
                  (* create rlr rv rr *)    
          end 
        else Node (l,v,r, hr + 1)  
    end
  | Node(ll,lv,lr,hl) ->
    begin match r with 
      | Empty ->
        if hl > 2 then 
          (*if height ll >= height lr then create ll lv (create lr v r)
            else*)
          begin match lr with 
            | Empty -> 
              create ll lv (Node (Empty,v,Empty, 1)) 
            (* create lr v r *)  
            | Node(lrl,lrv,lrr,hlr) -> 
              if height ll >= hlr then 
                create ll lv
                  (Node(lr,v,Empty,hlr+1)) 
                  (*create lr v r*)
              else 
                let hlrr = height lrr in  
                create 
                  (create ll lv lrl)
                  lrv
                  (Node(lrr,v,Empty,hlrr + 1)) 
                  (*create lrr v r*)
          end 
        else Node(l,v,r, hl+1)    
      | Node(rl,rv,rr,hr) ->
        if hl > hr + 2 then           
          begin match lr with 
            | Empty ->   create ll lv (create lr v r)
            | Node(lrl,lrv,lrr,_) ->
              if height ll >= height lr then create ll lv (create lr v r)
              else 
                create (create ll lv lrl) lrv (create lrr v r)
          end 
        else
        if hr > hl + 2 then             
          begin match rl with 
            | Empty ->
              let hrr = height rr in   
              Node(
                (Node (l,v,Empty,hl + 1))
                (*create l v rl*)
                ,
                rv,
                rr,
                if hrr > hr then hrr + 1 else hl + 2 
              )
            | Node(rll,rlv,rlr,_) ->
              let hrr = height rr in 
              let hrl = height rl in 
              if hrr >= hrl then create (create l v rl) rv rr else 
                create (create l v rll) rlv (create rlr rv rr)
          end
        else  
          Node(l,v,r, if hl >= hr then hl+1 else hr + 1)
    end
*)
let internal_bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> assert false
    | Node(ll, lv, lr, _) ->   
      if height ll >= height lr then
        (* [ll] >~ [lr] 
           [ll] >~ [r] 
           [ll] ~~ [ lr ^ r]  
        *)
        create ll lv (create lr v r)
      else begin
        match lr with
          Empty -> assert false
        | Node(lrl, lrv, lrr, _)->
          (* [lr] >~ [ll]
             [lr] >~ [r]
             [ll ^ lrl] ~~ [lrr ^ r]   
          *)
          create (create ll lv lrl) lrv (create lrr v r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> assert false
    | Node(rl, rv, rr, _) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else begin
        match rl with
          Empty -> assert false
        | Node(rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr)
      end
  end else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))    

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node(Empty, v, r, _) -> r
  | Node(l, v, r, _) -> internal_bal (remove_min_elt l) v r

let singleton x = Node(Empty, x, Empty, 1)    

(* 
   All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2.
   weak form of [concat] 
*)

let internal_merge l r =
  match (l, r) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> internal_bal l (min_elt r) (remove_min_elt r)

(* Beware: those two functions assume that the added v is *strictly*
    smaller (or bigger) than all the present elements in the tree; it
    does not test for equality with the current min (or max) element.
    Indeed, they are only used during the "join" operation which
    respects this precondition.
*)

let rec add_min_element v = function
  | Empty -> singleton v
  | Node (l, x, r, h) ->
    internal_bal (add_min_element v l) x r

let rec add_max_element v = function
  | Empty -> singleton v
  | Node (l, x, r, h) ->
    internal_bal l x (add_max_element v r)

(** 
    Invariants:
    1. l < v < r 
    2. l and r are balanced 

    Proof by induction
    The height of output will be ~~ (max (height l) (height r) + 2)
    Also use the lemma from [bal]
*)
let rec internal_join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
    if lh > rh + 2 then 
      (* proof by induction:
         now [height of ll] is [lh - 1] 
      *)
      internal_bal ll lv (internal_join lr v r) 
    else
    if rh > lh + 2 then internal_bal (internal_join l v rl) rv rr 
    else create l v r


(*
    Required Invariants: 
    [t1] < [t2]  
*)
let internal_concat t1 t2 =
  match (t1, t2) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> internal_join t1 (min_elt t2) (remove_min_elt t2)

let rec filter p = function
  | Empty -> Empty
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter p l in
    let pv = p v in
    let r' = filter p r in
    if pv then internal_join l' v r' else internal_concat l' r'


let rec partition p = function
  | Empty -> (Empty, Empty)
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition p l in
    let pv = p v in
    let (rt, rf) = partition p r in
    if pv
    then (internal_join lt v rt, internal_concat lf rf)
    else (internal_concat lt rt, internal_join lf v rf)

let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node (Empty, x0, Empty, 1), l
    | 2, x0 :: x1 :: l -> Node (Node(Empty, x0, Empty, 1), x1, Empty, 2), l
    | 3, x0 :: x1 :: x2 :: l ->
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2),l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)

let of_sorted_array l =   
  let rec sub start n l  =
    if n = 0 then Empty else 
    if n = 1 then 
      let x0 = Array.unsafe_get l start in
      Node (Empty, x0, Empty, 1)
    else if n = 2 then     
      let x0 = Array.unsafe_get l start in 
      let x1 = Array.unsafe_get l (start + 1) in 
      Node (Node(Empty, x0, Empty, 1), x1, Empty, 2) else
    if n = 3 then 
      let x0 = Array.unsafe_get l start in 
      let x1 = Array.unsafe_get l (start + 1) in
      let x2 = Array.unsafe_get l (start + 2) in
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2)
    else 
      let nl = n / 2 in
      let left = sub start nl l in
      let mid = start + nl in 
      let v = Array.unsafe_get l mid in 
      let right = sub (mid + 1) (n - nl - 1) l in        
      create left v right
  in
  sub 0 (Array.length l) l 



(******************************************************************)
(* Functions below require ordering *)

(* Splitting.  
    split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x.
*)

let rec split x = function
  | Empty ->
    (Empty, false, Empty)
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, internal_join rl v r)
    else
      let (lr, pres, rr) = split x r in (internal_join l v lr, pres, rr)

let rec add x = function
    Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = Pervasives.compare x v in
    if c = 0 then t else
    if c < 0 then internal_bal (add x l) v r else internal_bal l v (add x r)

let rec union s1 s2 =
  match (s1, s2) with
  | (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
    if h1 >= h2 then
      if h2 = 1 then add v2 s1 else begin
        let (l2, _, r2) = split v1 s2 in
        internal_join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add v1 s2 else begin
      let (l1, _, r1) = split v2 s1 in
      internal_join (union l1 l2) v2 (union r1 r2)
    end    

let rec inter s1 s2 =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        internal_concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        internal_join (inter l1 l2) v1 (inter r1 r2)
    end 

let rec diff s1 s2 =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        internal_join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        internal_concat (diff l1 l2) (diff r1 r2)    
    end


let rec mem x = function
    Empty -> false
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x = function
    Empty -> Empty
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
    if c = 0 then internal_merge l r else
    if c < 0 then internal_bal (remove x l) v r else internal_bal l v (remove x r)


let rec compare_aux e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    let c = Pervasives.compare v1 v2 in
    if c <> 0
    then c
    else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

let compare s1 s2 =
  compare_aux (cons_enum s1 End) (cons_enum s2 End)

let equal s1 s2 =
  compare s1 s2 = 0

let rec subset s1 s2 =
  match (s1, s2) with
    Empty, _ ->
    true
  | _, Empty ->
    false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
    let c = Pervasives.compare v1 v2 in
    if c = 0 then
      subset l1 l2 && subset r1 r2
    else if c < 0 then
      subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
    else
      subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2




let rec find x = function
    Empty -> raise Not_found
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
    if c = 0 then v
    else find x (if c < 0 then l else r)



let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add x1 (singleton x0)
  | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
  | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq Pervasives.compare l)

let of_array l = 
  Array.fold_left (fun  acc x -> add x acc) empty l
let is_ordered cmp tree =
  let rec is_ordered_min_max tree =
    match tree with
    | Empty -> `Empty
    | Node(l,v,r,_) -> 
      begin match is_ordered_min_max l with
        | `No -> `No 
        | `Empty ->
          begin match is_ordered_min_max r with
            | `No  -> `No
            | `Empty -> `V (v,v)
            | `V(l,r) ->
              if cmp v l < 0 then
                `V(v,r)
              else
                `No
          end
        | `V(min_v,max_v)->
          begin match is_ordered_min_max r with
            | `No -> `No
            | `Empty -> 
              if Pervasives.compare max_v v < 0 then 
                `V(min_v,v)
              else
                `No 
            | `V(min_v_r, max_v_r) ->
              if Pervasives.compare max_v min_v_r < 0 then
                `V(min_v,max_v_r)
              else `No
          end
      end  in 
  is_ordered_min_max tree <> `No 


(* also check order *)
let invariant t =
  check t; 
  is_ordered Pervasives.compare t

module Make ( S : Set.OrderedType) = struct
  type elt = S.t
  type nonrec t = elt t 
  let empty = empty 
  let is_empty = is_empty
  let iter = iter 
  let fold = fold 
  let for_all = for_all 
  let exists = exists 
  let singleton = singleton 
  let cardinal = cardinal
  let elements = elements
  let min_elt = min_elt
  let max_elt = max_elt
  let choose = choose 
  let of_sorted_list = of_sorted_list
  let of_sorted_array = of_sorted_array
  let partition = partition 
  let filter = filter 
  let of_sorted_list = of_sorted_list
  let of_sorted_array = of_sorted_array
  let rec split x = function
    | Empty ->
      (Empty, false, Empty)
    | Node(l, v, r, _) ->
      let c = S.compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in (ll, pres, internal_join rl v r)
      else
        let (lr, pres, rr) = split x r in (internal_join l v lr, pres, rr)
  let rec add x = function
      Empty -> Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
      let c = S.compare x v in
      if c = 0 then t else
      if c < 0 then internal_bal (add x l) v r else internal_bal l v (add x r)

  let rec union s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then add v2 s1 else begin
          let (l2, _, r2) = split v1 s2 in
          internal_join (union l1 l2) v1 (union r1 r2)
        end
      else
      if h1 = 1 then add v1 s2 else begin
        let (l1, _, r1) = split v2 s1 in
        internal_join (union l1 l2) v2 (union r1 r2)
      end    

  let rec inter s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> Empty
    | (t1, Empty) -> Empty
    | (Node(l1, v1, r1, _), t2) ->
      begin match split v1 t2 with
        | (l2, false, r2) ->
          internal_concat (inter l1 l2) (inter r1 r2)
        | (l2, true, r2) ->
          internal_join (inter l1 l2) v1 (inter r1 r2)
      end 

  let rec diff s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> Empty
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, _), t2) ->
      begin match split v1 t2 with
        | (l2, false, r2) ->
          internal_join (diff l1 l2) v1 (diff r1 r2)
        | (l2, true, r2) ->
          internal_concat (diff l1 l2) (diff r1 r2)    
      end


  let rec mem x = function
      Empty -> false
    | Node(l, v, r, _) ->
      let c = S.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec remove x = function
      Empty -> Empty
    | Node(l, v, r, _) ->
      let c = S.compare x v in
      if c = 0 then internal_merge l r else
      if c < 0 then internal_bal (remove x l) v r else internal_bal l v (remove x r)


  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, r1, e1), More(v2, r2, e2)) ->
      let c = S.compare v1 v2 in
      if c <> 0
      then c
      else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

  let compare s1 s2 =
    compare_aux (cons_enum s1 End) (cons_enum s2 End)

  let equal s1 s2 =
    compare s1 s2 = 0

  let rec subset s1 s2 =
    match (s1, s2) with
      Empty, _ ->
      true
    | _, Empty ->
      false
    | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
      let c = S.compare v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
      else
        subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2




  let rec find x = function
      Empty -> raise Not_found
    | Node(l, v, r, _) ->
      let c = S.compare x v in
      if c = 0 then v
      else find x (if c < 0 then l else r)



  let of_list l =
    match l with
    | [] -> empty
    | [x0] -> singleton x0
    | [x0; x1] -> add x1 (singleton x0)
    | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
    | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
    | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
    | _ -> of_sorted_list (List.sort_uniq S.compare l)

  let of_array l = 
    Array.fold_left (fun  acc x -> add x acc) empty l

  (* also check order *)
  let invariant t =
    check t ;
    is_ordered S.compare t          
end 
