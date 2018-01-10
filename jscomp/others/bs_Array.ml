(***********************************************************************)
(*                                                                     *)
(*                           OCaml                                     *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Array operations *)

external length : 'a array -> int = "%array_length"
external get: 'a array -> int -> 'a = "%array_safe_get"
external set: 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external makeUninitialized : int -> 'a Js.undefined array = "Array" [@@bs.new]
external makeUninitializedUnsafe : int -> 'a  array = "Array" [@@bs.new]

external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external concat : 'a array list -> 'a array = "caml_array_concat"
external blitUnsafe :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"


(*DOC: when l < 0 raise RangeError js excpetion *)
(* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... *)
let init l f =
  [%assert l >= 0];
  let res = makeUninitializedUnsafe l in 
  for i = 0 to  l - 1 do
    unsafe_set res i (f i [@bs])
  done;
  res

let swapUnsafe xs i j =    
  let tmp = unsafe_get xs i in 
  unsafe_set xs i (unsafe_get xs j) ;
  unsafe_set xs j tmp


let shuffleInPlace xs =     
  let len = length xs in 
  for i = 0 to len - 1 do
    swapUnsafe xs i (Js_math.random_int i len) (* [i,len)*)
  done 

let makeMatrix sx sy init =
  [%assert sx >=0 && sy >=0 ];
  let res = makeUninitializedUnsafe sx in 
  for x = 0 to  sx - 1 do
    let initY = makeUninitializedUnsafe sy in 
    for y = 0 to sy - 1 do 
      unsafe_set initY y init 
    done ;
    unsafe_set res x initY
  done;
  res



let copy a =
  let l = length a in 
  let v = makeUninitializedUnsafe l in 
  for i = 0 to l - 1 do 
    unsafe_set v i (unsafe_get a i)
  done ;
  v


let zip xs ys = 
  let lenx, leny = length xs, length ys in 
  let len = Pervasives.min lenx leny  in 
  let s = makeUninitializedUnsafe len in 
  for i = 0 to len - 1 do 
    unsafe_set s i (unsafe_get xs i, unsafe_get ys i)
  done ; 
  s 

let append a1 a2 =
  let l1 = length a1 in
  if l1 = 0 then copy a2
  else if length a2 = 0 then unsafe_sub a1 0 l1
  else append_prim a1 a2

let sub a ofs len =
  if len < 0 || ofs > length a - len
  then 
    (* invalid_arg  *)
    [%assert "Array.sub"]
  else unsafe_sub a ofs len

let fill a ofs len v =
  if ofs < 0 || len < 0 || ofs > length a - len
  then 
    (* invalid_arg  *)
    [%assert "Array.fill"]
  else for i = ofs to ofs + len - 1 do unsafe_set a i v done

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
     || ofs2 < 0 || ofs2 > length a2 - len
  then 
    (* invalid_arg  *)
    [%assert "Array.blit"]
  else blitUnsafe a1 ofs1 a2 ofs2 len

let iter a f =
  for i = 0 to length a - 1 do f(unsafe_get a i) [@bs] done

let map a f =
  let l = length a in
  let r = makeUninitializedUnsafe l in 
  for i = 0 to l - 1 do
    unsafe_set r i (f(unsafe_get a i) [@bs])
  done;
  r


let iteri a f=
  for i = 0 to length a - 1 do f i (unsafe_get a i) [@bs] done

let mapi  a f =
  let l = length a in
  let r = makeUninitializedUnsafe l in 
  for i = 0 to l - 1 do
    unsafe_set r i (f i (unsafe_get a i) [@bs])
  done;
  r


let toList a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

(* Cannot use List.length here because the List module depends on Array. *)
let rec list_length accu = function
  | [] -> accu
  | h::t -> list_length (succ accu) t


let rec fillAUx arr i xs =
  match xs with 
  |  [] -> ()
  | hd::tl -> unsafe_set arr i hd; fillAUx arr (i+1) tl 

let ofList xs = 
  let len = list_length 0 xs in  
  let a = makeUninitializedUnsafe len in 
  fillAUx a 0 xs;
  a

let foldLeft a x f =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i) [@bs]
  done;
  !r

let foldRight a x f =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f  !r (unsafe_get a i) [@bs]
  done;
  !r





let rec forAllAux arr i b len =   
  if i = len then true 
  else if b (unsafe_get arr i) [@bs] then 
    forAllAux arr (i + 1) b len
  else false    

let forAll arr b =   
  let len = length arr in 
  forAllAux arr 0 b len 

let rec forAllAux2 arr1 arr2 i b len =   
  if i = len then true 
  else if b (unsafe_get arr1 i) (unsafe_get arr2 i) [@bs] then 
    forAllAux2 arr1 arr2 (i + 1) b len
  else false      

let forAll2  a b p =   
  let lena = length a in  
  let lenb = length b in 
  if lena <> lenb then false
  else 
    forAllAux2  a b 0 p lena