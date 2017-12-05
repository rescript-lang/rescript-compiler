

type ('elt, 'id) t0 = Empty | Node of ('elt, 'id) t0 * 'elt * ('elt, 'id) t0 * int

type ('elt, 'id) t = {
  cmp : ('elt,'id) Bs_Cmp.t ; 
  data : ('elt,'id) t0
}
(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)

let height = function
    Empty -> 0
  | Node(_, _, _, h) -> h

(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> assert false
    | Node(ll, lv, lr, _) ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else begin
        match lr with
          Empty -> assert false
        | Node(lrl, lrv, lrr, _)->
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

(* Insertion of one element *)

let rec add0 ~cmp x  = function
    Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then t else
    if c < 0 then bal (add0 ~cmp x l) v r else bal l v (add0 ~cmp x r)

let singleton0 x = Node(Empty, x, Empty, 1)

(* Beware: those two functions assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree; it
   does not test for equality with the current min (or max) element.
   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_element v = function
  | Empty -> singleton0 v
  | Node (l, x, r, h) ->
    bal (add_min_element v l) x r

let rec add_max_element v = function
  | Empty -> singleton0 v
  | Node (l, x, r, h) ->
    bal l x (add_max_element v r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
    if lh > rh + 2 then bal ll lv (join lr v r) else
    if rh > lh + 2 then bal (join l v rl) rv rr else
      create l v r

(* Smallest and greatest element of a set *)

let rec min0 = function
    Empty -> None
  | Node(Empty, v, r, _) -> Some v
  | Node(l, v, r, _) -> min0 l

let rec max0 = function
    Empty -> None
  | Node(l, v, Empty, _) -> Some v
  | Node(l, v, r, _) -> max0 r

(* Remove the smallest element of the given set *)

(* Input is non empty data *)
let rec min_eltAssert0 = function
    Empty -> assert false
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> min_eltAssert0 l
(* Input is non empty data *)
let rec remove_min_elt = function
    Empty -> assert false
  | Node(Empty, v, r, _) -> r
  | Node(l, v, r, _) -> bal (remove_min_elt l) v r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, Node _) -> bal t1 (min_eltAssert0 t2) (remove_min_elt t2)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, Node _) -> join t1 (min_eltAssert0 t2) (remove_min_elt t2)

(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec split0 ~cmp x = function
    Empty ->
    (Empty, false, Empty)
  | Node(l, v, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split0 ~cmp x l in (ll, pres, join rl v r)
    else
      let (lr, pres, rr) = split0 ~cmp x r in (join l v lr, pres, rr)

(* Implementation of the set operations *)

let empty0 = Empty

let isEmpty0 = function Empty -> true | _ -> false

let rec mem0 ~cmp x = function
    Empty -> false
  | Node(l, v, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp x (if c < 0 then l else r)

let rec remove0 ~cmp x = function
    Empty -> Empty
  | Node(l, v, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then merge l r else
    if c < 0 then bal (remove0 ~cmp x l) v r else bal l v (remove0 ~cmp x r)

let rec union0 ~cmp s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
    if h1 >= h2 then
      if h2 = 1 then add0 ~cmp v2 s1 else begin
        let (l2, _, r2) = split0 ~cmp v1 s2 in
        join (union0 ~cmp l1 l2) v1 (union0 ~cmp r1 r2)
      end
    else
    if h1 = 1 then add0 ~cmp v1 s2 else begin
      let (l1, _, r1) = split0 ~cmp v2 s1 in
      join (union0 ~cmp l1 l2) v2 (union0 ~cmp r1 r2)
    end

let rec inter0 ~cmp s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
    match split0 ~cmp v1 t2 with
      (l2, false, r2) ->
      concat (inter0 ~cmp l1 l2) (inter0 ~cmp r1 r2)
    | (l2, true, r2) ->
      join (inter0 ~cmp l1 l2) v1 (inter0 ~cmp r1 r2)

let rec diff0 ~cmp s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
    match split0 ~cmp v1 t2 with
      (l2, false, r2) ->
      join (diff0 ~cmp l1 l2) v1 (diff0 ~cmp r1 r2)
    | (l2, true, r2) ->
      concat (diff0 ~cmp l1 l2) (diff0 ~cmp r1 r2)

type ('elt, 'id)enumeration = End | More of 'elt * ('elt, 'id) t0 * ('elt, 'id) enumeration

let rec cons_enum s e =
  match s with
    Empty -> e
  | Node(l, v, r, _) -> cons_enum l (More(v, r, e))

let rec compare_aux ~cmp e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c <> 0
    then c
    else compare_aux ~cmp (cons_enum r1 e1) (cons_enum r2 e2)

let cmp0 ~cmp s1 s2 =
  compare_aux ~cmp (cons_enum s1 End) (cons_enum s2 End)

let eq0 ~cmp s1 s2 =
  cmp0 ~cmp s1 s2 = 0

let rec subset0 ~cmp s1 s2 =
  match (s1, s2) with
    Empty, _ ->
    true
  | _, Empty ->
    false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c = 0 then
      subset0 ~cmp l1 l2 && subset0 ~cmp r1 r2
    else if c < 0 then
      subset0 ~cmp (Node (l1, v1, Empty, 0)) l2 && subset0 ~cmp r1 t2
    else
      subset0 ~cmp (Node (Empty, v1, r1, 0)) r2 && subset0 ~cmp l1 t2

let rec iter0 f = function
    Empty -> ()
  | Node(l, v, r, _) -> iter0 f l; f v [@bs]; iter0 f r

let rec fold0 f s accu =
  match s with
    Empty -> accu
  | Node(l, v, r, _) -> fold0 f r (f v (fold0 f l accu) [@bs])

let rec forAll0 p = function
    Empty -> true
  | Node(l, v, r, _) -> p v [@bs] && forAll0 p l && forAll0 p r

let rec exists0 p = function
    Empty -> false
  | Node(l, v, r, _) -> p v [@bs] || exists0 p l || exists0 p r

let rec filter0 p = function
    Empty -> Empty
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter0 p l in
    let pv = p v [@bs] in
    let r' = filter0 p r in
    if pv then join l' v r' else concat l' r'

let rec partition0 p = function
    Empty -> (Empty, Empty)
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition0 p l in
    let pv = p v [@bs] in
    let (rt, rf) = partition0 p r in
    if pv
    then (join lt v rt, concat lf rf)
    else (concat lt rt, join lf v rf)

let rec cardinal0 = function
    Empty -> 0
  | Node(l, v, r, _) -> cardinal0 l + 1 + cardinal0 r

let rec elements_aux accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements0 s =
  elements_aux [] s


let rec findOpt0 ~cmp x = function
    Empty -> None
  | Node(l, v, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some v
    else findOpt0 ~cmp x (if c < 0 then l else r)

let rec findAssert0 ~cmp x = function
    Empty -> [%assert "Not_found"]
  | Node(l, v, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then  v
    else findAssert0 ~cmp x (if c < 0 then l else r)

    
let empty cmp = {
  cmp ;
  data = empty0
}  

let isEmpty m = isEmpty0 m.data

let mem (type elt) (type id) e (m : (elt,id) t) = 
  let module M = (val m.cmp) in 
  mem0 ~cmp:(M.cmp) e m.data

let add (type elt) (type id) e (m : (elt,id) t) =   
  let m_cmp = m.cmp in
  let module M = (val m_cmp) in 
  {data = add0 ~cmp:(M.cmp) e m.data ; cmp = m_cmp}

let singleton cmp e =     
  { cmp; 
    data = singleton0 e
  }

let remove (type elt) (type id) e (m : (elt,id) t) =      
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  { data = remove0 ~cmp:M.cmp e m.data ; 
    cmp = m_cmp
  }

let union (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  { data = union0 ~cmp:M.cmp m.data n.data ;
    cmp = m_cmp
  }

let inter (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  { data = inter0 ~cmp:M.cmp m.data n.data ;
    cmp = m_cmp
  }  

let diff (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  { data = diff0 ~cmp:M.cmp m.data n.data ;
    cmp = m_cmp
  }    

let cmp (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  cmp0 ~cmp:M.cmp m.data n.data

let eq (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  eq0 ~cmp:M.cmp m.data n.data  

let subset (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  subset0 ~cmp:M.cmp m.data n.data  

let iter f m = iter0 f m.data 

let fold f m acc = fold0 f m.data acc 

let forAll f m = forAll0 f m.data

let exists f m = exists0 f m.data   

let filter f m = {m with data = filter0 f m.data}

let partition f m = 
  let l,r = partition0 f m.data in 
  let cmp = m.cmp in 
  {data = l; cmp}, {data = r; cmp}

let cardinal m = cardinal0 m.data  

let elements m = elements0 m.data

let min m = min0 m.data

let max m = max0 m.data

let split (type elt) (type id) e (m : (elt,id) t) = 
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  let l, b, r = split0 ~cmp:M.cmp e m.data in 
  {cmp = m_cmp; data = l},
  b,
  {cmp = m_cmp ; data = r}

let findOpt (type elt) (type id) e (m : (elt,id) t) =   
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  findOpt0 ~cmp:M.cmp e m.data

let findAssert (type elt) (type id) e (m : (elt,id) t) =   
  let m_cmp = m.cmp in 
  let module M = (val m_cmp) in 
  findAssert0 ~cmp:M.cmp e m.data
  
