
module N = Bs_internalAVLset
module B =  Bs_Bag
module A = Bs_Array
type ('elt, 'id) t0 = ('elt, 'id) N.t0 


(* here we relies on reference transparence
   address equality means everything equal across time
   no need to call [bal] again
*)  
let rec add0 ~cmp (t : _ t0) x  : _ t0 =
  match N.toOpt t with 
    None -> N.(return @@ node ~left:empty ~right:empty ~key:x  ~h:1)
  | Some nt ->
    let k = N.key nt in 
    let c = (Bs_Cmp.getCmp cmp) x k [@bs] in
    if c = 0 then t else
      let l,r = N.(left nt, right nt) in 
      if c < 0 then 
        let ll = add0 ~cmp l x in 
        if ll == l then t 
        else N.bal ll k r 
      else 
        let rr = add0 ~cmp r x in 
        if rr == r then t 
        else N.bal l k rr 



let rec mem0 ~cmp  (t: _ t0) x =
  match  N.toOpt t with 
  | None -> false
  | Some n ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp (if c < 0 then N.left n else N.right n) x

let rec remove0 ~cmp (t : _ t0) x : _ t0 = 
  match N.toOpt t with 
    None -> t
  | Some n  ->
    let l,v,r = N.(left n , key n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then 
      match N.toOpt l, N.toOpt r with 
      | (None, _) -> r 
      | (_, None) -> l 
      | (_, Some rn) -> 
        let v = ref (N.key rn) in 
        let r = N.removeMinAuxWithRef rn v in 
        N.bal l !v r 
    else
    if c < 0 then 
      let ll = remove0 ~cmp  l x in 
      if ll == l then t
      else N.bal ll v r 
    else
      let rr = remove0 ~cmp  r x in 
      if rr == r then t  
      else N.bal l v rr

let addArray0 ~cmp  h arr =   
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.unsafe_get arr i in 
    v := add0 !v  ~cmp key 
  done ;
  !v 

let removeArray0 h arr ~cmp = 
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key = A.unsafe_get arr i in 
    v := remove0 !v  ~cmp key 
  done ;
  !v 


let rec compareAux e1 e2 ~cmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    let c = (Bs_Cmp.getCmp cmp) (N.key h1) (N.key h2) [@bs] in 
    if c = 0 then
      compareAux ~cmp 
        (N.stackAllLeft  (N.right h1) t1)
        (N.stackAllLeft (N.right h2) t2)
    else c 
  | _, _ -> 0   

let cmp0 s1 s2 ~cmp = 
  let len1,len2 = N.length0 s1, N.length0 s2 in    
  if len1 = len2 then
   compareAux ~cmp (N.stackAllLeft s1 []) (N.stackAllLeft s2 [])
  else if len1 < len2 then -1 else 1 


let eq0 ~cmp s1 s2 =
  cmp0 ~cmp s1 s2 = 0

let rec splitAuxNoPivot ~cmp (n : _ N.node) x : _ *  _ =   
  let l,v,r = N.(left n , key n, right n) in  
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
  if c = 0 then l,r
  else 
  if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.empty ,  N.return n
    | Some l -> 
      let (ll,  rl) = splitAuxNoPivot ~cmp  l x in 
      ll,  N.join rl v r
  else
    match N.toOpt r with 
    | None ->
      N.return n,  N.empty
    | Some r -> 
      let lr,  rr = splitAuxNoPivot ~cmp  r x in 
      N.join l v lr, rr

let rec splitAuxPivot ~cmp (n : _ N.node) x pres : _ *  _ =   
  let l,v,r = N.(left n , key n, right n) in  
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
  if c = 0 then 
    begin
      pres := true;
      l, r
    end
  else 
  if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.empty , N.return n
    | Some l -> 
      let (ll, rl) = splitAuxPivot ~cmp  l x pres in 
      ll,  N.join rl v r
  else
    match N.toOpt r with 
    | None ->
      N.return n,  N.empty
    | Some r -> 
      let lr, rr = splitAuxPivot ~cmp  r x pres in 
      N.join l v lr,  rr

let  split0 ~cmp  (t : _ t0) x : _ t0 * bool * _ t0 =
  match N.toOpt t with 
    None ->
    N.empty, false, N.empty
  | Some n ->
    let pres = ref false in 
    let l, r = splitAuxPivot ~cmp n x  pres in 
    l, !pres, r 

(* [union0 s1 s2]
   Use the pivot to split the smaller collection
*)      
let rec union0 ~cmp (s1 : _ t0) (s2 : _ t0) : _ t0=
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s2
  | (_, None) -> s1
  | Some n1, Some n2 ->
    let h1, h2 = N.(h n1 , h n2) in                 
    if h1 >= h2 then
      if h2 = 1 then add0 ~cmp s1 (N.key n2)  
      else begin
        let l1, v1, r1 = N.(left n1, key n1, right n1) in      
        let l2, r2 = splitAuxNoPivot ~cmp n2 v1 in
        N.join (union0 ~cmp l1 l2) v1 (union0 ~cmp r1 r2)
      end
    else
    if h1 = 1 then add0 s2 ~cmp (N.key n1) 
    else begin
      let l2, v2, r2 = N.(left n2 , key n2, right n2) in 
      let l1, r1 = splitAuxNoPivot ~cmp n1 v2  in
      N.join (union0 ~cmp l1 l2) v2 (union0 ~cmp r1 r2)
    end

let rec inter0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match N.(toOpt s1, toOpt s2) with
  | None, _ 
  | _, None -> N.empty
  | Some n1, Some n2  ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in  
    let pres = ref false in 
    let l2,r2 = splitAuxPivot ~cmp n2 v1 pres in 
    let ll = inter0 ~cmp l1 l2 in 
    let rr = inter0 ~cmp r1 r2 in 
    if !pres then N.join ll v1 rr 
    else N.concat ll rr 

let rec diff0 ~cmp s1 s2 =
  match N.(toOpt s1, toOpt s2) with
    (None, _) 
  | (_, None) -> s1
  | Some n1, Some n2  ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in
    let pres = ref false in 
    let l2, r2 = splitAuxPivot ~cmp n2 v1 pres in 
    let ll = diff0 ~cmp l1 l2 in 
    let rr = diff0 ~cmp r1 r2 in 
    if !pres then N.concat ll rr 
    else N.join ll v1 rr 


let rec subset0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match N.(toOpt s1, toOpt s2) with
  | None, _ -> true
  | _, None -> false
  | Some t1 , Some t2  ->
    let l1,v1,r1 = N.(left t1, key t1, right t1) in  
    let l2,v2,r2 = N.(left t2, key t2, right t2) in 
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c = 0 then
      subset0 ~cmp l1 l2 && subset0 ~cmp r1 r2
    else if c < 0 then
      subset0 ~cmp N.(return @@ node ~left:l1 ~key:v1 ~right:empty ~h:0) l2 && 
      subset0 ~cmp r1 s2
    else
      subset0 ~cmp N.(return @@ node ~left:empty ~key:v1 ~right:r1 ~h:0) r2 && 
      subset0 ~cmp l1 s2
(* and subsetAuxLeft s1 v s2 ~cmp = 
   mem0 ~cmp s2 v &&
   subset0 ~cmp s1 s2  *)

let rec findOpt0 ~cmp (n : _ t0) x = 
  match N.toOpt n with 
    None -> None
  | Some t (* Node(l, v, r, _) *) ->
    let v = N.key t in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some v
    else findOpt0 ~cmp  (if c < 0 then N.left t else N.right t) x


let rec findNull0 ~cmp (n : _ t0) x =
  match N.toOpt n with 
    None -> Js.null
  | Some t (* Node(l, v, r, _) *) ->
    let v = N.key t in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then  N.return v
    else findNull0 ~cmp  (if c < 0 then N.left t else N.right t) x 



let rec addMutate ~cmp (t : _ t0) x =   
  match N.toOpt t with 
  | None -> N.(return @@ node ~left:empty ~right:empty ~key:x ~h:1)
  | Some nt -> 
    let k = N.key nt in 
    let  c = (Bs_Cmp.getCmp cmp) x k [@bs] in  
    if c = 0 then t 
    else
      let l, r = N.(left nt, right nt) in 
      (if c < 0 then                   
         let ll = addMutate ~cmp l x in
         N.leftSet nt ll
       else   
         N.rightSet nt (addMutate ~cmp r x);
      );
      N.return (N.balMutate nt)

let rec removeMutateAux ~cmp nt x = 
  let k = N.key nt in 
  let c = (Bs_Cmp.getCmp cmp) x k [@bs] in 
  if c = 0 then 
    let l,r = N.(left nt, right nt) in       
    match N.(toOpt l, toOpt r) with 
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxMutateWithRoot nt nr);
      N.return (N.balMutate nt)
    | None, Some _ ->
      r  
    | (Some _ | None ), None ->  l 
  else 
    begin 
      if c < 0 then 
        match N.toOpt (N.left nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (removeMutateAux ~cmp l x );
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.right nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeMutateAux ~cmp r x);
          N.return (N.balMutate nt)
    end



let rec sortedLengthAux ~cmp (xs : _ array) prec acc len =    
  if  acc >= len then acc 
  else 
    let v = A.unsafe_get xs acc in 
    if (Bs_Cmp.getCmp cmp) v  prec [@bs] >= 0 then 
      sortedLengthAux ~cmp xs v (acc + 1) len 
    else acc    

let ofArray0 ~cmp (xs : _ array) =   
  let len = A.length xs in 
  if len = 0 then N.empty0
  else
    let next = sortedLengthAux ~cmp xs (A.unsafe_get xs 0) 1 len in 
    let result  = ref (N.ofSortedArrayAux  xs 0 next) in 
    for i = next to len - 1 do 
      result := addMutate ~cmp !result (A.unsafe_get xs i) 
    done ;
    !result     


let addArrayMutate (t : _ t0) xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    v := addMutate !v (A.unsafe_get xs i)  ~cmp
  done; 
  !v 


let rec addMutateCheckAux  (t : _ t0) x added ~cmp  =   
  match N.toOpt t with 
  | None -> 
    added := true;
    N.(return @@ node ~left:empty ~right:empty ~key:x ~h:1)
  | Some nt -> 
    let k = N.key nt in 
    let  c = (Bs_Cmp.getCmp cmp) x k [@bs] in  
    if c = 0 then t 
    else
      let l, r = N.(left nt, right nt) in 
      (if c < 0 then                   
         let ll = addMutateCheckAux ~cmp l x added in
         N.leftSet nt ll
       else   
         N.rightSet nt (addMutateCheckAux ~cmp r x added );
      );
      N.return (N.balMutate nt)


let rec removeArrayMutateAux t xs i len ~cmp  =  
  if i < len then 
    let ele = A.unsafe_get xs i in 
    let u = removeMutateAux t ele ~cmp in 
    match N.toOpt u with 
    | None -> N.empty0
    | Some t -> removeArrayMutateAux t xs (i+1) len ~cmp 
  else N.return t    

let removeArrayMutate (t : _ t0) xs ~cmp =
  match N.toOpt t with 
  | None -> t
  | Some nt -> 
    let len = A.length xs in 
    removeArrayMutateAux nt xs 0 len ~cmp 

let rec removeMutateCheckAux  nt x removed ~cmp= 
  let k = N.key nt in 
  let c = (Bs_Cmp.getCmp cmp) x k [@bs] in 
  if c = 0 then 
    let () = removed := true in  
    let l,r = N.(left nt, right nt) in       
    match N.(toOpt l, toOpt r) with 
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxMutateWithRoot nt nr);
      N.return (N.balMutate nt)
    | None, Some _ ->
      r  
    | (Some _ | None ), None ->  l 
  else 
    begin 
      if c < 0 then 
        match N.toOpt (N.left nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (removeMutateCheckAux ~cmp l x removed);
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.right nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeMutateCheckAux ~cmp r x removed);
          N.return (N.balMutate nt)
    end