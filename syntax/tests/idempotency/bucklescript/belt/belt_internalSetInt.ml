# 5 "others/internal_set.cppo.ml"
type value = int
module S = Belt_SortArrayInt


# 12 "others/internal_set.cppo.ml"
module N = Belt_internalAVLset
module A = Belt_Array 


type t = value N.t

let rec has (t : t) (x : value)  =
  match N.toOpt t with 
  | None -> false
  | Some n  ->                
    let v = N.valueGet n in 
    x = v || has (if x < v then N.leftGet n else N.rightGet n) x


let rec compareAux e1 e2  =
    match e1,e2 with 
    | h1::t1, h2::t2 ->
        let (k1 : value) ,k2 = N.valueGet h1, N.valueGet h2 in 
        if k1 = k2 then  
          compareAux 
            (N.stackAllLeft (N.rightGet h1) t1 ) 
            (N.stackAllLeft (N.rightGet h2) t2)
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
  match N.(toOpt s1, toOpt s2) with
    None, _ ->
    true
  | _, None ->
    false
  | Some t1, Some t2 (* Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) *) ->
    let l1,v1,r1 = N.(leftGet t1, valueGet t1, rightGet t1) in  
    let l2,v2,r2 = N.(leftGet t2, valueGet t2, rightGet t2) in 
    if v1 = v2 then
      subset l1 l2 && subset r1 r2
    else if v1 < v2 then
      subset N.(create l1 v1 empty ) l2 && subset r1 s2
    else
      subset N.(create empty v1 r1 ) r2 && subset l1 s2


let rec get (n :t) (x : value) = 
  match N.toOpt n with 
  | None -> None
  | Some t  ->    
    let v = N.valueGet t in     
    if x = v then Some v
    else get (if x < v then N.leftGet t else N.rightGet t) x



let rec getUndefined (n :t) (x : value)   = 
  match N.toOpt n with 
  | None -> Js.undefined
  | Some t  ->    
    let v = N.valueGet t in     
    if x = v then Js.Undefined.return v
    else getUndefined  (if x < v then N.leftGet t else N.rightGet t) x

let rec getExn  (n :t) (x : value) = 
  match N.toOpt n with 
  | None -> [%assert "getExn"]
  | Some t  ->    
    let v = N.valueGet t in     
    if x = v then  v
    else getExn (if x < v then N.leftGet t else N.rightGet t) x

(****************************************************************************)
let rec addMutate  t  (x : value)=   
  match N.toOpt t with 
  | None -> N.singleton x
  | Some nt -> 
    let k = N.valueGet nt in 
    if x = k then t 
    else
      let l, r = N.(leftGet nt, rightGet nt) in 
      (if x < k then                   
         N.leftSet nt (addMutate l x)       
       else   
         N.rightSet nt (addMutate r x);
      );
      N.return (N.balMutate nt)



let fromArray (xs : value array) =   
  let len = A.length xs in 
  if len = 0 then N.empty
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



