# 1 "others/internal_map.cppo.ml"
[@@@bs.config {flags = [| "-bs-noassertfalse" |]}]
# 5 "others/internal_map.cppo.ml"
type key = int

# 10 "others/internal_map.cppo.ml"
module N = Belt_internalAVLtree
module A = Belt_Array 
module S = Belt_SortArray



type  'a t = (key,'a) N.t

let rec add  t (x : key) (data : _)  = 
  match t with
  | None -> 
    N.singleton x data 
  | Some n  ->
    let k = n.N.key in 
    if x = k then
      Some (N.updateValue n data)
    else
      let v = n.N.value  in 
      if x < k then
        N.bal (add n.N.left x data ) k v n.N.right
      else
        N.bal n.N.left k v (add n.N.right x data )

let rec get n (x : key)  =
  match n with 
    None -> None
  | Some n  ->
    let v = n.N.key in 
    if x = v then Some n.N.value
    else get (if x < v then n.N.left else n.N.right) x 

let rec getUndefined n (x : key) = 
  match n with 
  | None ->
    Js.undefined
  | Some n  ->
    let v = n.N.key in 
    if x = v then Js.Undefined.return n.N.value
    else getUndefined (if x < v then n.N.left else n.N.right) x 

let rec getExn n (x : key) =
  match n with 
  | None -> raise Not_found
  | Some n -> 
    let v = n.N.key in 
    if x = v then n.N.value
    else getExn (if x < v then n.N.left else n.N.right) x

let rec getWithDefault n (x : key) def =
  match n with 
  | None -> def    
  | Some n -> 
    let v = n.N.key in 
    if x = v then n.N.value
    else getWithDefault (if x < v then n.N.left else n.N.right) x def
    
let rec has n (x : key)= 
  match n with 
    None -> false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = n.N.key in 
    x = v || has (if x < v then n.N.left else n.N.right) x 

let rec remove n (x : key) = 
  match n with 
  |  None -> n    
  |  Some n ->
    let {N.left = l; key = v; right = r} = n in 
    if x = v then
      match l, r with
      | None, _ -> r 
      | _, None -> l 
      | _, Some rn -> 
        let kr, vr = ref rn.key, ref rn.value in 
        let r = N.removeMinAuxWithRef rn kr vr in 
        N.bal l kr.contents vr.contents r 
    else if x < v then
      N.(bal (remove l x ) v n.value r)
    else
      N.(bal l v n.value (remove r x ))

let rec splitAux (x : key) (n : _ N.node) : _ t * _ option  * _ t =  
  let {N.left = l; key = v; value = d; right = r} = n in  
  if x = v then (l, Some d, r)
  else     
  if x < v then
    match l with 
    | None -> 
      None, None, Some n
    | Some l -> 
      let (ll, pres, rl) = splitAux x l in (ll, pres, N.join rl v d r)
  else
    match r with 
    | None ->
      Some n, None, None
    | Some r -> 
      let (lr, pres, rr) = splitAux x r in (N.join l v d lr, pres, rr)


let split (x : key) n =
  match n with 
    None ->
    None, None, None
  | Some n -> 
    splitAux x n 

let rec mergeU s1 s2 f =
  match s1, s2 with
    (None, None) -> None
  | Some n (* (Node (l1, v1, d1, r1, h1), _)*), _ 
    when (n.N.height >= (match s2 with None -> 0 | Some n -> n.N.height)) ->
    let {N.left = l1; key = v1; value = d1; right = r1} = n in 
    let (l2, d2, r2) = split v1 s2 in
    N.concatOrJoin (mergeU l1 l2 f) v1 (f v1 (Some d1) d2 [@bs]) (mergeU r1 r2 f)
  | (_, Some n) (* Node (l2, v2, d2, r2, h2) *)  ->
    let {N.left = l2; key = v2; value = d2; right = r2} = n in 
    let (l1, d1, r1) = split v2 s1 in
    N.concatOrJoin (mergeU l1 l2 f) v2 (f v2 d1 (Some d2) [@bs]) (mergeU r1 r2 f)
  | _ ->
    assert false

let merge s1 s2 f = mergeU s1 s2 (fun [@bs] a b c -> f a b c)
    
let rec compareAux e1 e2 vcmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    let c = Pervasives.compare (h1.N.key : key) h2.N.key  in 
    if c = 0 then 
      let cx = vcmp h1.N.value h2.N.value [@bs] in 
      if cx = 0 then
        compareAux 
          (N.stackAllLeft  h1.N.right t1 ) 
          (N.stackAllLeft h2.N.right t2)
          vcmp 
      else  cx
    else c 
  | _, _ -> 0    

let cmpU s1 s2 cmp = 
  let len1, len2 = N.size s1, N.size s2 in 
  if len1 = len2 then 
    compareAux 
      (N.stackAllLeft s1 []) 
      (N.stackAllLeft s2 []) 
      cmp 
  else if len1 < len2 then -1 
  else 1 

let cmp s1 s2 f = cmpU s1 s2 (fun[@bs] a b -> f a b)

let rec eqAux e1 e2  eq =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    if (h1.N.key : key) =  h2.N.key  && 
       eq h1.N.value h2.N.value [@bs] then
      eqAux (
        N.stackAllLeft  h1.N.right t1 ) 
        (N.stackAllLeft h2.N.right t2)
        eq
    else  false    
  | _, _ -> true (*end *)  

let eqU s1 s2 eq =      
  let len1,len2 = N.size s1, N.size s2 in 
  if len1 = len2 then 
    eqAux 
      (N.stackAllLeft s1 [])
      (N.stackAllLeft s2 []) eq 
  else false  

let eq s1 s2 f = eqU s1 s2 (fun[@bs] a b -> f a b)
    
let rec addMutate  (t : _ t) x data : _ t =   
  match t with 
  | None -> N.singleton x data
  | Some nt -> 
    let k = nt.N.key in 
    (* let  c = (Belt_Cmp.getCmpInternal cmp) x k [@bs] in   *)
    if x = k then begin     
      nt.N.key <- x;
      nt.value <- data;
      Some nt
    end      
    else
      let l, r = (nt.N.left, nt.N.right) in 
      (if x < k then                   
         let ll = addMutate  l x data in
         nt.left <- ll
       else   
         nt.right <- (addMutate r x data);
      );
      Some (N.balMutate nt)  

let fromArray (xs : (key * _) array) =   
  let len = A.length xs in 
  if len = 0 then None
  else
    let next = 
        ref (S.strictlySortedLengthU xs 
        (fun[@bs] (x0,_) (y0,_) -> 
            x0 < y0
        ))
      in 
    let result  = ref (
      if next.contents >= 0 then 
        N.fromSortedArrayAux xs 0 next.contents 
      else begin   
        next .contents<- - next.contents; 
        N.fromSortedArrayRevAux xs (next.contents - 1) (next.contents)
      end  
    ) in 
    for i = next.contents to len - 1 do 
      let k, v = (A.getUnsafe xs i)  in 
      result .contents<- addMutate  result.contents k v 
    done ;
    result.contents         





