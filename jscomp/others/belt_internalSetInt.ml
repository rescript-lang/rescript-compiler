# 5 "others/internal_set.cppo.ml"
type value = int
module S = Belt_SortArrayInt


# 12 "others/internal_set.cppo.ml"
module N = Belt_internalAVLset
module A = Belt_Array 


type t = value N.t

let rec has (t : t) (x : value)  =
  match t with 
  | None -> false
  | Some n  ->                
    let v = n.value in 
    x = v || has (if x < v then n.left else n.right) x


let rec compareAux e1 e2  =
    match e1,e2 with 
    | h1::t1, h2::t2 ->
        let (k1 : value) ,k2 = h1.N.value , h2.N.value  in 
        if k1 = k2 then  
          compareAux 
            (N.stackAllLeft h1.right t1 ) 
            (N.stackAllLeft h2.right t2)
        else if k1  < k2 then -1    
        else 1
    | _, _ -> 0   


let cmp s1 s2 =
  let len1, len2 = N.size s1, N.size s2 in   
  if len1 = len2 then 
    compareAux (N.stackAllLeft s1 []) (N.stackAllLeft s2 [])
  else if len1 < len2 then -1 else 1 

let eq (s1 : t) s2 = 
  cmp s1 s2 = 0





(* This algorithm applies to BST, it does not need to be balanced tree *)  
let rec subset (s1 : t) (s2 : t) =
  match s1, s2 with
    None, _ ->
    true
  | _, None ->
    false
  | Some t1, Some t2 (* Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) *) ->
    let {N.left = l1; value = v1; right = r1 } = t1 in  
    let {N.left = l2; value = v2; right = r2 } = t2 in 
    if v1 = v2 then
      subset l1 l2 && subset r1 r2
    else if v1 < v2 then
      subset (N.create l1 v1 None) l2 && subset r1 s2
    else
      subset (N.create None v1 r1 ) r2 && subset l1 s2


let rec get (n :t) (x : value) = 
  match n with 
  | None -> None
  | Some t  ->    
    let v = t.value in     
    if x = v then Some v
    else get (if x < v then t.left else t.right) x



let rec getUndefined (n :t) (x : value)   = 
  match n with 
  | None -> Js.undefined
  | Some t  ->    
    let v = t.value in     
    if x = v then Js.Undefined.return v
    else getUndefined  (if x < v then t.left else t.right) x

let rec getExn  (n :t) (x : value) = 
  match n with 
  | None -> raise Not_found
  | Some t  ->    
    let v = t.value in     
    if x = v then  v
    else getExn (if x < v then t.left else t.right) x

(****************************************************************************)
let rec addMutate  t  (x : value)=   
  match t with 
  | None -> N.singleton x
  | Some nt -> 
    let k = nt.N.value in 
    if x = k then t 
    else
      let {N.left = l;  right = r} = nt in 
      (if x < k then                   
         nt.left <- addMutate l x       
       else   
         nt.right <- addMutate r x;
      );
      Some (N.balMutate nt)



let fromArray (xs : value array) =   
  let len = A.length xs in 
  if len = 0 then None
  else
    let next =  ref (S.strictlySortedLength xs ) in 
    let result  = 
        ref (
          if next.contents >= 0 then 
            N.fromSortedArrayAux xs 0 next.contents
          else begin 
            next .contents<- - next.contents ;  
            N.fromSortedArrayRevAux xs (next.contents - 1) next.contents
          end
          ) in 
    for i = next.contents to len - 1 do 
      result .contents<- addMutate result.contents (A.getUnsafe xs i) 
    done ;
    result.contents 



