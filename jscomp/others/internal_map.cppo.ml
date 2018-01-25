#ifdef TYPE_STRING
type key = string
#elif defined TYPE_INT
type key = int
#else 
  [%error "unknown type"]
#endif 

module N = Bs_internalAVLtree
module A = Bs_Array 
module S = Bs_Sort
type ('key, 'a, 'id) t0 = ('key,'a) N.t0 

type  'a t = (key,'a) N.t0 

let rec add  t (x : key) (data : _)  = 
  match N.toOpt t with
  | None -> 
    N.singleton0 x data 
  | Some n  ->
    let k = N.key n in 
    if x = k then
      N.return (N.updateValue n data)
    else
      let v = N.value n in 
      if x < k then
        N.bal (add (N.left n) x data ) k v (N.right n)
      else
        N.bal (N.left n) k v (add (N.right n) x data )

let rec findOpt n (x : key)  =
  match N.toOpt n with 
    None -> None
  | Some n  ->
    let v = N.key n in 
    if x = v then Some (N.value n)
    else findOpt (if x < v then N.left n else N.right n) x 

let rec findNull n (x : key) = 
  match N.toOpt n with 
  | None ->
    Js.null
  | Some n  ->
    let v = N.key n in 
    if x = v then N.return (N.value n)
    else findNull (if x < v then (N.left n) else (N.right n)) x 

let rec findExn n (x : key) =
  match N.toOpt n with 
  | None -> [%assert "findExn"]
  | Some n -> 
    let v = N.key n in 
    if x = v then (N.value n)
    else findExn (if x < v then (N.left n) else (N.right n)) x

let rec findWithDefault n (x : key) def =
  match N.toOpt n with 
  | None -> def    
  | Some n -> 
    let v = N.key n in 
    if x = v then (N.value n)
    else findWithDefault (if x < v then (N.left n) else (N.right n)) x def
    
let rec mem n (x : key)= 
  match N.toOpt n with 
    None -> false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = N.key n in 
    x = v || mem (if x < v then N.left n else N.right n) x 

let rec remove n (x : key) = 
  match N.toOpt n with 
  |  None -> n    
  |  Some n ->
    let l,v,r = N.(left n, key n, right n) in 
    if x = v then
      match N.toOpt l, N.toOpt r with
      | None, _ -> r 
      | _, None -> l 
      | _, Some rn -> 
        let kr, vr = ref (N.key rn), ref (N.value rn) in 
        let r = N.removeMinAuxWithRef rn kr vr in 
        N.bal l !kr !vr r 
    else if x < v then
      N.(bal (remove l x ) v (value n) r)
    else
      N.(bal l v (value n) (remove r x ))

let rec splitAux (x : key) (n : _ N.node) : _ t0 * _ option  * _ t0 =  
  let l,v,d,r = N.(left n , key n, value n, right n) in  
  if x = v then (l, Some d, r)
  else     
  if x < v then
    match N.toOpt l with 
    | None -> 
      N.(empty , None, return n)
    | Some l -> 
      let (ll, pres, rl) = splitAux x l in (ll, pres, N.join rl v d r)
  else
    match N.toOpt r with 
    | None ->
      N.(return n, None, empty)
    | Some r -> 
      let (lr, pres, rr) = splitAux x r in (N.join l v d lr, pres, rr)


let rec split (x : key) n =
  match N.toOpt n with 
    None ->
    N.(empty, None, empty)
  | Some n -> 
    splitAux x n 

let rec merge s1 s2 f =
  match N.(toOpt s1, toOpt s2) with
    (None, None) -> N.empty
  | Some n (* (Node (l1, v1, d1, r1, h1), _)*), _ 
    when N.(h n >= (match N.toOpt s2 with None -> 0 | Some n -> N.h n)) ->
    let (l1,v1,d1,r1) = N.(left n, key n, value n, right n ) in 
    let (l2, d2, r2) = split v1 s2 in
    N.concatOrJoin (merge l1 l2 f) v1 (f v1 (Some d1) d2 [@bs]) (merge r1 r2 f)
  | (_, Some n) (* Node (l2, v2, d2, r2, h2) *)  ->
    let (l2, v2, d2, r2) = N.(left n, key n, value n, right n) in 
    let (l1, d1, r1) = split v2 s1 in
    N.concatOrJoin (merge l1 l2 f) v2 (f v2 d1 (Some d2) [@bs]) (merge r1 r2 f)
  | _ ->
    assert false

let rec compareAux e1 e2 vcmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    let c = Pervasives.compare (N.key h1 : key) (N.key h2)  in 
    if c = 0 then 
      let cx = vcmp (N.value h1) (N.value h2) [@bs] in 
      if cx = 0 then
        compareAux 
          (N.stackAllLeft  (N.right h1) t1 ) 
          (N.stackAllLeft (N.right h2) t2)
          vcmp 
      else  cx
    else c 
  | _, _ -> 0    

let cmp s1 s2 cmp = 
  let len1, len2 = N.length0 s1, N.length0 s2 in 
  if len1 = len2 then 
    compareAux 
      (N.stackAllLeft s1 []) 
      (N.stackAllLeft s2 []) 
      cmp 
  else if len1 < len2 then -1 
  else 1 


let rec eqAux e1 e2  eq =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    if (N.key h1 : key) =  (N.key h2)  && 
       eq (N.value h1) (N.value h2) [@bs] then
      eqAux (
        N.stackAllLeft  (N.right h1) t1 ) 
        (N.stackAllLeft (N.right h2) t2)
        eq
    else  false    
  | _, _ -> true (*end *)  

let eq s1 s2 eq =      
  let len1,len2 = N.length0 s1, N.length0 s2 in 
  if len1 = len2 then 
    eqAux 
      (N.stackAllLeft s1 [])
      (N.stackAllLeft s2 []) eq 
  else false  

let rec addMutate  (t : _ t) x data : _ t =   
  match N.toOpt t with 
  | None -> N.singleton0 x data
  | Some nt -> 
    let k = N.key nt in 
    (* let  c = (Bs_Cmp.getCmp cmp) x k [@bs] in   *)
    if x = k then begin     
      N.keySet nt x;
      N.valueSet nt data;
      N.return nt
    end      
    else
      let l, r = (N.left nt, N.right nt) in 
      (if x < k then                   
         let ll = addMutate  l x data in
         N.leftSet nt ll
       else   
         N.rightSet nt (addMutate r x data);
      );
      N.return (N.balMutate nt)  

let ofArray  (xs : (key * _) array) =   
  let len = A.length xs in 
  if len = 0 then N.empty0
  else
    let next = 
        ref (S.strictlySortedLength xs 
        (fun[@bs] (x0,_) (y0,_) -> 
            x0 < y0
        ))
      in 
    let result  = ref (
      if !next >= 0 then 
        N.ofSortedArrayAux xs 0 !next 
      else begin   
        next := - !next; 
        N.ofSortedArrayRevAux xs (!next - 1) (!next)
      end  
    ) in 
    for i = !next to len - 1 do 
      let k, v = (A.getUnsafe xs i)  in 
      result := addMutate  !result k v 
    done ;
    !result         





