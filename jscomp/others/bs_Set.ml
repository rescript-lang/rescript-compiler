
module N = Bs_internalAVLset
type ('elt, 'id) t0 = ('elt, 'id) N.t0 

type ('elt, 'id)enumeration = 
    ('elt, 'id) N.enumeration0 
    =
    End 
    | More of 'elt * ('elt, 'id) t0 * ('elt, 'id) enumeration

type ('elt, 'id) t = {
  cmp : ('elt,'id) Bs_Cmp.t ; 
  data : ('elt,'id) t0
}

let empty0 = N.empty0      
let isEmpty0 = N.isEmpty0
let singleton0 = N.singleton0
let min0 = N.min0
let max0 = N.max0
let iter0 = N.iter0      
let fold0 = N.fold0
let forAll0 = N.forAll0
let exists0 = N.exists0    
let filter0 = N.filter0
let partition0 = N.partition0
let cardinal0 = N.cardinal0
let elements0 = N.elements0 

(* Insertion of one element *)

let rec add0 ~cmp x  (t : _ t0) : _ t0 =
  match N.toOpt t with 
    None -> N.(return @@ node ~left:empty ~right:empty ~value:x  ~h:1)
  | Some nt (* Node(l, v, r, _) as t *) ->
    let l,v,r = N.(left nt, value nt, right nt) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then t else
    if c < 0 then N.bal (add0 ~cmp x l) v r else N.bal l v (add0 ~cmp x r)


(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec split0 ~cmp x (t : _ t0) : _ t0 * bool * _ t0 =
  match N.toOpt t with 
    None ->
    N.(empty, false, empty)
  | Some n(* Node(l, v, r, _) *) ->
    let l,v,r = N.(left n, value n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split0 ~cmp x l in (ll, pres, N.join rl v r)
    else
      let (lr, pres, rr) = split0 ~cmp x r in (N.join l v lr, pres, rr)

let rec mem0 ~cmp x (t: _ t0) =
  match  N.toOpt t with 
  | None -> false
  | Some n (* Node(l, v, r, _) *) ->
    let l,v,r = N.(left n , value n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp x (if c < 0 then l else r)

let rec remove0 ~cmp x (t : _ t0) : _ t0 = 
  match N.toOpt t with 
    None -> t
  | Some n (* Node(l, v, r, _) *) ->
    let l,v,r = N.(left n , value n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then N.merge l r else
    if c < 0 then N.bal (remove0 ~cmp x l) v r else N.bal l v (remove0 ~cmp x r)

let rec union0 ~cmp (s1 : _ t0) (s2 : _ t0) : _ t0=
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s2
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) *) ->
    let h1, h2, v1,v2 = N.(h n1 , h n2, value n1, value n2) in                 
    if h1 >= h2 then
      if h2 = 1 then add0 ~cmp v2 s1 else begin
        let l1, r1 = N.(left n1, right n1) in      
        let (l2, _, r2) = split0 ~cmp v1 s2 in
        N.join (union0 ~cmp l1 l2) v1 (union0 ~cmp r1 r2)
      end
    else
    if h1 = 1 then add0 ~cmp v1 s2 else begin
      let l2, r2 = N.(left n2 , right n2) in 
      let (l1, _, r1) = split0 ~cmp v2 s1 in
      N.join (union0 ~cmp l1 l2) v2 (union0 ~cmp r1 r2)
    end

let rec inter0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s1
  | (_, None) -> s2
  | Some n1, Some _ (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, value n1, right n1) in  
    match split0 ~cmp v1 s2 with
      (l2, false, r2) ->
      N.concat (inter0 ~cmp l1 l2) (inter0 ~cmp r1 r2)
    | (l2, true, r2) ->
      N.join (inter0 ~cmp l1 l2) v1 (inter0 ~cmp r1 r2)

let rec diff0 ~cmp s1 s2 =
  match N.(toOpt s1, toOpt s2) with
    (None, _) 
  | (_, None) -> s1
  | Some n1, Some _ (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, value n1, right n1) in
    match split0 ~cmp v1 s2 with
      (l2, false, r2) ->
      N.join (diff0 ~cmp l1 l2) v1 (diff0 ~cmp r1 r2)
    | (l2, true, r2) ->
      N.concat (diff0 ~cmp l1 l2) (diff0 ~cmp r1 r2)



let rec compare_aux ~cmp e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c <> 0
    then c
    else compare_aux ~cmp (N.cons_enum r1 e1) (N.cons_enum r2 e2)

let cmp0 ~cmp s1 s2 =
  compare_aux ~cmp (N.cons_enum s1 End) (N.cons_enum s2 End)

let eq0 ~cmp s1 s2 =
  cmp0 ~cmp s1 s2 = 0

let rec subset0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match N.(toOpt s1, toOpt s2) with
    None, _ ->
    true
  | _, None ->
    false
  | Some t1 , Some t2 (* Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) *) ->
    let l1,v1,r1 = N.(left t1, value t1, right t1) in  
    let l2,v2,r2 = N.(left t2, value t2, right t2) in 
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c = 0 then
      subset0 ~cmp l1 l2 && subset0 ~cmp r1 r2
    else if c < 0 then
      subset0 ~cmp N.(return @@ node ~left:l1 ~value:v1 ~right:empty ~h:0) l2 && subset0 ~cmp r1 s2
    else
      subset0 ~cmp N.(return @@ node ~left:empty ~value:v1 ~right:r1 ~h:0) r2 && subset0 ~cmp l1 s2

let rec findOpt0 ~cmp x (n : _ t0) = 
  match N.toOpt n with 
    None -> None
  | Some t (* Node(l, v, r, _) *) ->
    let l,v,r = N.(left t , value t, right t) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some v
    else findOpt0 ~cmp x (if c < 0 then l else r)

let rec findAssert0 ~cmp x (n : _ t0) =
  match N.toOpt n with 
    None -> [%assert "Not_found"]
  | Some t (* Node(l, v, r, _) *) ->
    let l, v, r = N.(left t , value t, right t) in 
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

