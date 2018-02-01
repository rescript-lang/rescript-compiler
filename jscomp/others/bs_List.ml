(* Copyright (C) 2017 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


(*
   perf is not everything, there are better memory represenations
   {[
     type 'a cell = {
       mutable head : 'a;
       mutable tail : 'a opt_cell
     }

     and 'a opt_cell = 'a cell Js.null

     and 'a t = {
       length : int ;
       data : 'a opt_cell
     }
   ]}
   However,
   - people use List not because of its perf, but its
   convenencie, in that case, pattern match and compatibility seems
   more attractive, we could keep a mutable list
   - The built in types would indicate that
     its construtor is immutable, a better optimizer would break such code

   {[
     type 'a t = {
       head : 'a;
       mutable tail : 'a t | int 
     }
   ]}
   In the future, we could come up with a safer version
   {[
     type 'a t =
       | Nil
       | Cons of { hd : 'a ; mutable tail : 'a t }
   ]}
*)

type 'a t = 'a list

module A = Bs_Array

external mutableCell : 
  'a -> 'a t ->  'a t = "#makemutablelist"
(* 
    [mutableCell x []] == [x]
    but tell the compiler that is a mutable cell, so it wont
    be mis-inlined in the future
     dont inline a binding to mutable cell, it is mutable
*)
external unsafeMutateTail : 
  'a t -> 'a t -> unit = "#setfield1"    
(*
   - the cell is not empty   
   - it is mutated
*)  
external unsafeTail :   
  'a t -> 'a t = "%field1"
(*
   - the cell is not empty   
*)

let head x =
  match x with
  | [] -> None
  | x::_ -> Some x

let tail x =
  match x with
  | [] -> None
  | _::xs -> Some xs 

let add xs x  = x :: xs
                
(* Assume [n >=0] *)
let rec nthAux x n =
  match x with
  | h::t -> if n = 0 then Some h else nthAux t (n - 1)
  | _ -> None

let rec nthAuxAssert x n =
  match x with
  | h::t -> if n = 0 then h else nthAuxAssert t (n - 1)
  | _ -> [%assert "getExn"]

let get x n =
  if n < 0 then None
  else nthAux x n

let getExn x n =
  if n < 0 then [%assert "nthAssert"]
  else nthAuxAssert x n 

let rec partitionAux p cell precX precY =
  match cell with
  | [] -> ()
  | h::t ->
    let next = mutableCell h [] in
    if p h [@bs] then     
      begin 
        unsafeMutateTail precX next ;
        partitionAux p t next precY
      end 
    else 
      begin 
        unsafeMutateTail precY next ;
        partitionAux p t precX next 
      end 

let rec splitAux cell precX precY = 
  match cell with 
  | [] -> () 
  | (a,b)::t -> 
    let nextA = mutableCell a [] in 
    let nextB = mutableCell b [] in 
    unsafeMutateTail precX nextA;  
    unsafeMutateTail precY nextB; 
    splitAux t nextA nextB

(* return the tail pointer so it can continue copy other 
   list
*)  
let rec copyAuxCont cellX prec =
  match cellX with
  | [] -> prec
  | h::t ->
    let next = mutableCell h [] in
    unsafeMutateTail prec next ; 
    copyAuxCont t next

let rec copyAuxWitFilter f cellX prec =
  match cellX with
  | [] -> 
    ()
  | h::t ->
    if f h [@bs] then 
      begin 
        let next = mutableCell h [] in
        unsafeMutateTail prec next ; 
        copyAuxWitFilter f t next
      end
    else copyAuxWitFilter f t prec 

let rec copyAuxWitFilterMap f cellX prec =
  match cellX with
  | [] -> 
    ()
  | h::t ->
    match f h [@bs] with
    | Some h -> 
      begin 
        let next = mutableCell h [] in
        unsafeMutateTail prec next ; 
        copyAuxWitFilterMap f t next
      end
    | None ->  copyAuxWitFilterMap f t prec 

let rec removeAssocAuxWithMap  cellX x  prec f =  
  match cellX with 
  | [] -> ()
  | ((a,_) as h):: t -> 
    if f a x [@bs] then 
      unsafeMutateTail prec t 
    else  
      let next = mutableCell h [] in 
      unsafeMutateTail prec next ; 
      removeAssocAuxWithMap t x next f 

let rec removeAssocAuxByReference  cellX x  prec =  
  match cellX with 
  | [] -> ()
  | ((a,_) as h):: t -> 
    if  a == x  then 
      unsafeMutateTail prec t 
    else  
      let next = mutableCell h [] in 
      unsafeMutateTail prec next ; 
      removeAssocAuxByReference t x next 
      
let rec copyAuxWithMap cellX prec f =
  match cellX with
  | [] -> 
    ()
  | h::t ->
    let next = mutableCell (f h [@bs]) [] in
    unsafeMutateTail prec next ; 
    copyAuxWithMap t next f


let rec zipAux  cellX cellY prec =
  match cellX, cellY with
  | h1::t1, h2::t2 -> 
    let next = mutableCell ( h1, h2) [] in
    unsafeMutateTail prec next ; 
    zipAux  t1 t2 next
  | [],_ | _,[] -> 
    ()

let rec copyAuxWithMap2 f cellX cellY prec =
  match cellX, cellY with
  | h1::t1, h2::t2 -> 
    let next = mutableCell (f h1 h2 [@bs]) [] in
    unsafeMutateTail prec next ; 
    copyAuxWithMap2 f t1 t2 next
  | [],_ | _,[] -> 
    ()

let rec copyAuxWithMapI f i cellX prec =
  match cellX with
  | h::t ->
    let next = mutableCell (f i h [@bs]) [] in
    unsafeMutateTail prec next ; 
    copyAuxWithMapI f (i + 1) t next
  | [] -> 
    ()

let rec takeAux n cell prec = 
  if n = 0 then true
  else 
    match cell with 
    | [] -> false 
    | x::xs -> 
      let cell = mutableCell x [] in 
      unsafeMutateTail prec cell; 
      takeAux (n - 1) xs cell 

let rec splitAtAux n cell prec = 
  if n = 0 then Some cell 
  else 
    match cell with 
    | [] -> None 
    | x::xs -> 
      let cell = mutableCell x [] in 
      unsafeMutateTail prec cell;  
      splitAtAux (n - 1) xs cell

(* invarint [n >= 0] *)    
let  take lst n = 
  if n < 0 then None
  else 
  if n = 0 then Some []
  else 
    match lst with
    | [] -> None 
    | x::xs -> 
      let cell = mutableCell x [] in 
      let has = takeAux (n-1) xs cell in
      if has then Some cell
      else None
(* invariant [n >= 0 ] *)
let rec dropAux l n = 
  if n = 0 then Some l
  else 
    match l with 
    | _::tl ->  dropAux tl (n -1)
    | [] -> None 

let drop lst n =       
  if n < 0 then None 
  else 
    dropAux lst n 

let splitAt lst n =     
  if n < 0 then None 
  else 
  if n = 0 then Some ([],lst) 
  else 
    match lst with 
    | [] ->  None 
    | x::xs -> 
      let cell = mutableCell x [] in 
      let rest = splitAtAux (n - 1) xs cell in 
      match rest with 
      | Some rest -> Some (cell, rest)
      | None -> None

let concat xs ys =
  match xs with
  | [] -> ys
  | h::t ->
    let cell = mutableCell h [] in       
    unsafeMutateTail (copyAuxCont t cell) ys; 
    cell

let map xs f =
  match xs with
  | [] -> []
  | h::t ->
    let cell = mutableCell (f h [@bs]) [] in
    copyAuxWithMap t cell f;
    cell

let zipBy l1 l2 f =
  match (l1, l2) with
  | (a1::l1, a2::l2) -> 
    let cell = mutableCell (f a1 a2 [@bs]) []  in
    copyAuxWithMap2 f l1 l2 cell; 
    cell 
  | [], _ | _, [] -> []

let mapWithIndex  xs f  = 
  match xs with 
    [] -> []
  | h::t -> 
    let cell = mutableCell (f 0 h [@bs]) [] in 
    copyAuxWithMapI f 1 t cell;
    cell 




let makeBy n f =
  if n <= 0 then []
  else
    let headX = mutableCell (f 0 [@bs]) [] in
    let cur = ref headX in
    let i = ref 1 in
    while !i < n do
      let v = mutableCell (f !i [@bs]) [] in
      unsafeMutateTail !cur v ; 
      cur := v ;
      incr i ;
    done
    ;
    headX

let make n v =
  if n <= 0 then []
  else
    let headX = mutableCell v [] in
    let cur = ref headX in
    let i = ref 1 in
    while !i < n do
      let v = mutableCell v [] in
      unsafeMutateTail !cur v ; 
      cur := v ;
      incr i ;
    done
    ;
    headX

let rec lengthAux x acc =
  match x with
  | [] -> acc
  | _::t -> lengthAux t (acc + 1)

let length xs = lengthAux xs 0
let size = length
  
let rec fillAux arr i x =
  match x with
  | [] -> ()
  | h::t ->
    A.setUnsafe arr i h ;
    fillAux arr (i + 1) t

let toArray ( x : _ t) =
  let len = length x in
  let arr = Bs_Array.makeUninitializedUnsafe len in
  fillAux arr 0 x;
  arr

let rec fillAuxMap arr i x f =
  match x with
  | [] -> ()
  | h::t ->
    A.setUnsafe arr i (f h [@bs]) ;
    fillAuxMap arr (i + 1) t f  

(* module J = Js_json *)
(* type json = J.t  *)
(* let toJson x f =    *)
(*   let len = length x in *)
(*   let arr = Bs_Array.makeUninitializedUnsafe len in *)
(*   fillAuxMap arr 0 x f; *)
(*   J.array arr *)

(* TODO: best practice about raising excpetion 
   1. raise OCaml exception, no stacktrace 
   2. raise JS exception, how to pattern match
*)

(* let fromJson j f =    *)
(*   match J.decodeArray j with  *)
(*   | Some arr ->      *)
(*     let len = Bs_Array.length arr in  *)
(*     if len = 0 then [] *)
(*     else  *)
(*       let head = (mutableCell (f (A.getUnsafe arr 0) [@bs]) []) in  *)
(*       let cell = ref head in    *)
(*       for i = 1 to len - 1 do    *)
(*         let next = mutableCell (f (A.getUnsafe arr i) [@bs]) [] in  *)
(*         unsafeMutateTail !cell next ; *)
(*         cell := next  *)
(*       done ; *)
(*       head *)
(*   | None -> *)
(*     [%assert "Not array when decoding list"] *)
    
let rec reverseConcat l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> reverseConcat l (a :: l2)

let reverse l = reverseConcat l []

let rec flattenAux prec xs =
  match xs with
  | [] -> unsafeMutateTail prec [] 
  | h::r -> flattenAux (copyAuxCont h prec) r


let rec flatten xs =     
  match xs with 
  | [] -> []
  | []::xs -> flatten xs
  | (h::t):: r ->  
    let cell = mutableCell h [] in 
    flattenAux (copyAuxCont t cell) r ;
    cell 

let concatMany xs =
  match xs with
  | [||] -> []
  | [|x|] -> x
  | _ ->
    let len = A.length xs in
    let v = ref (A.getUnsafe xs (len - 1)) in
    for i = len - 2 downto 0 do
      v := concat (A.getUnsafe xs i) !v
    done ;
    !v
    
let rec mapRevAux f accu xs = 
  match xs with 
  | [] -> accu
  | a::l -> mapRevAux f (f a [@bs] :: accu) l

let mapReverse l f  =
  mapRevAux f [] l


let rec forEach xs f  = 
  match xs with 
    [] -> ()
  | a::l -> f a [@bs]; forEach l f 

let rec iteri xs i f  = 
  match xs with 
    [] -> ()
  | a::l -> f i a [@bs]; iteri l (i + 1) f 

let forEachWithIndex l f  = iteri l 0 f 

let rec reduce l accu f   =
  match l with
    [] -> accu
  | a::l -> reduce l (f accu a [@bs]) f 

let rec reduceReverse l accu f  =
  match l with
    [] -> accu
  | a::l -> f a (reduceReverse l accu f) [@bs]


let rec mapRevAux2 l1 l2 accu f =
  match (l1, l2) with  
  | (a1::l1, a2::l2) -> mapRevAux2  l1 l2  (f a1 a2 [@bs]:: accu) f
  | _, [] | [], _ -> accu 

let mapReverse2 l1 l2 f =
  mapRevAux2 l1 l2  [] f

let rec forEach2  l1 l2 f =
  match (l1, l2) with
  | (a1::l1, a2::l2) -> f a1 a2 [@bs]; forEach2  l1 l2 f
  | [],_ | _, [] -> ()

let rec reduce2 l1 l2 accu f =
  match (l1, l2) with
  | (a1::l1, a2::l2) -> 
    reduce2 l1 l2 (f accu a1 a2 [@bs]) f 
  | [], _ | _, [] -> accu

let rec reduceReverse2 l1 l2 accu f  =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> f a1 a2 (reduceReverse2 l1 l2 accu f) [@bs]
  | _, [] | [], _ -> accu

let rec every xs p = 
  match xs with 
    [] -> true
  | a::l -> p a [@bs] && every l p

let rec some xs p = 
  match xs with 
    [] -> false
  | a::l -> p a [@bs] || some l p 

let rec every2 l1 l2  p =
  match (l1, l2) with
    (_, []) | [],_ -> true
  | (a1::l1, a2::l2) -> p a1 a2 [@bs] && every2 l1 l2 p

let rec cmp l1 l2  p =
  match (l1, l2) with
  | [], [] -> 0
  | _ , [] -> 1
  | [], _ -> -1
  | (a1::l1, a2::l2) ->
    let c = p a1 a2 [@bs] in
    if c = 0 then
      cmp l1 l2 p
    else c
      
let rec eq l1 l2  p =
  match (l1, l2) with
  | [], [] -> true
  | _ , [] 
  | [], _ -> false
  | (a1::l1, a2::l2) ->
    if p a1 a2 [@bs]  then
      eq l1 l2 p
    else false

let rec some2 l1 l2 p =
  match (l1, l2) with
    [], _ | _, [] -> false
  | (a1::l1, a2::l2) -> p a1 a2 [@bs] || some2 l1 l2 p 


let rec has xs x eq  = 
  match xs with 
    [] -> false
  | a::l -> eq a x [@bs] || has l x eq 

let rec hasByReference xs x = 
  match xs with 
    [] -> false
  | a::l -> a == x || hasByReference l x 

let rec assoc  xs x eq = 
  match xs with 
  | [] -> None
  | (a,b)::l -> 
    if eq a x [@bs] then Some b 
    else assoc l x eq 

let rec assocByReference xs x = 
  match xs with 
    [] -> None
  | (a,b)::l -> if a == x then Some b else assocByReference l x 

let rec hasAssoc xs x eq = 
  match xs with 
  | [] -> false
  | (a, b) :: l -> eq a x [@bs] || hasAssoc l x eq 

let rec hasAssocByReference xs x  = 
  match xs with 
  | [] -> false
  | (a, b) :: l -> a == x || hasAssocByReference l x 


(* remove the first pair *)  
let  removeAssoc xs x eq  = 
  match xs with 
  | [] -> []
  | (a, _ as pair) :: l ->
    if eq a x [@bs] then l 
    else 
      let cell = mutableCell pair [] in 
      removeAssocAuxWithMap l x cell eq ;
      cell 

let  removeAssocByReference xs x = 
  match xs with 
  | [] -> []
  | (a, b as pair) :: l -> 
    if a == x then l 
    else 
      let cell = mutableCell pair [] in 
      removeAssocAuxByReference l x cell;  
      cell 

let rec getBy xs p = 
  match xs with 
  | [] -> None
  | x :: l -> 
    if p x [@bs] then Some x
    else getBy l p 


let rec keep xs p  = 
  match xs with 
  | [] -> []
  | h::t -> 
    if p h [@bs] then 
      begin 
        let cell = mutableCell h [] in 
        copyAuxWitFilter p t cell ;
        cell 
      end
    else 
      keep t p
        

let rec keepMap xs p  = 
  match xs with 
  | [] -> []
  | h::t -> 
    match p h [@bs] with
    | Some h ->
      begin 
        let cell = mutableCell h [] in 
        copyAuxWitFilterMap p t cell ;
        cell 
      end
    | None -> 
      keepMap t p


let partition l p  =    
  match l with 
  | [] -> [],[]
  | h::t -> 
    let nextX = mutableCell h [] in 
    let nextY = mutableCell h [] in 
    let b = p h [@bs]  in 
    partitionAux p t nextX nextY;
    if b then 
      nextX, unsafeTail nextY  
    else       
      unsafeTail nextX, nextY 


let rec unzip xs = 
  match xs with 
  | [] -> ([], [])
  | (x,y)::l ->
    let cellX = mutableCell x [] in
    let cellY = mutableCell y [] in 
    splitAux l cellX cellY ; 
    cellX, cellY


let rec zip l1 l2 =
  match (l1, l2) with
    _, [] | [], _ -> []
  | (a1::l1, a2::l2) -> 
    let cell = mutableCell (a1,a2) [] in 
    zipAux l1 l2 cell; 
    cell

