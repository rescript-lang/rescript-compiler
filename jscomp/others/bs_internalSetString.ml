# 2 "internal_set.cppo.ml"
type elt = string
module S = Bs_SortString


# 12
module N = Bs_internalAVLset
module A = Bs_Array 


type t = elt N.t0


let rec mem (t : t) (x : elt)  =
  match N.toOpt t with 
  | None -> false
  | Some n  ->                
    let v = N.key n in 
    x = v || mem (if x < v then N.left n else N.right n) x


let rec compareAux e1 e2  =
    match e1,e2 with 
    | h1::t1, h2::t2 ->
        let (k1 : elt) ,k2 = N.key h1, N.key h2 in 
        if k1 = k2 then  
          compareAux 
            (N.stackAllLeft (N.right h1) t1 ) 
            (N.stackAllLeft (N.right h2) t2)
        else if k1  < k2 then -1    
        else 1
    | _, _ -> 0   


let cmp s1 s2 =
  let len1, len2 = N.length0 s1, N.length0 s2 in   
  if len1 = len2 then 
    compareAux (N.stackAllLeft s1 []) (N.stackAllLeft s2 [])
  else if len1 < len2 then -1 else 1 

let eq (s1 : t) s2 = 
  cmp s1 s2 = 0





(* This algorithm applies to BST, it does not need to be balanced tree *)  
let rec subset (s1 : t) (s2 : t) =
  match N.(toOpt s1, toOpt s2) with
    None, _ ->
    true
  | _, None ->
    false
  | Some t1, Some t2 (* Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) *) ->
    let l1,v1,r1 = N.(left t1, key t1, right t1) in  
    let l2,v2,r2 = N.(left t2, key t2, right t2) in 
    if v1 = v2 then
      subset l1 l2 && subset r1 r2
    else if v1 < v2 then
      subset N.(create l1 v1 empty ) l2 && subset r1 s2
    else
      subset N.(create empty v1 r1 ) r2 && subset l1 s2


let rec findOpt  (n :t) (x : elt) = 
  match N.toOpt n with 
  | None -> None
  | Some t  ->    
    let v = N.key t in     
    if x = v then Some v
    else findOpt (if x < v then N.left t else N.right t) x



let rec findNull (n :t) (x : elt)   = 
  match N.toOpt n with 
  | None -> Js.null
  | Some t  ->    
    let v = N.key t in     
    if x = v then N.return v
    else findNull  (if x < v then N.left t else N.right t) x

let rec findExn  (n :t) (x : elt) = 
  match N.toOpt n with 
  | None -> [%assert "findExn"]
  | Some t  ->    
    let v = N.key t in     
    if x = v then  v
    else findExn (if x < v then N.left t else N.right t) x

(****************************************************************************)
let rec addMutate  t  (x : elt)=   
  match N.toOpt t with 
  | None -> N.singleton0 x
  | Some nt -> 
    let k = N.key nt in 
    if x = k then t 
    else
      let l, r = N.(left nt, right nt) in 
      (if x < k then                   
         N.leftSet nt (addMutate l x)       
       else   
         N.rightSet nt (addMutate r x);
      );
      N.return (N.balMutate nt)



let ofArray (xs : elt array) =   
  let len = A.length xs in 
  if len = 0 then N.empty
  else
    let next =  ref (S.strictlySortedLength xs ) in 
    let result  = 
        ref (
          if !next >= 0 then 
            N.ofSortedArrayAux xs 0 !next
          else begin 
            next := - !next ;  
            N.ofSortedArrayRevAux xs (!next - 1) !next
          end
          ) in 
    for i = !next to len - 1 do 
      result := addMutate !result (A.unsafe_get xs i) 
    done ;
    !result 



