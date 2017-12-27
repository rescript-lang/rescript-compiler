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
external make: int -> 'a -> 'a array = "caml_make_vect"
external create: int -> 'a -> 'a array = "caml_make_vect"
external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external concat : 'a array list -> 'a array = "caml_array_concat"
external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"
external makeFloat: int -> float array = "caml_make_float_vect"

(*DOC: when l < 0 raise RangeError js excpetion *)
let init l f =
  if l = 0 then [||] else
  (* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... *)
  (* if l < 0 then invalid_arg "Array.init"
  
  else *)
   let res = create l (f 0 [@bs]) in
   for i = 1 to pred l do
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
  let res = create sx [||] in
  for x = 0 to pred sx do
    unsafe_set res x (create sy init)
  done;
  res



let copy a =
  let l = length a in if l = 0 then [||] else unsafe_sub a 0 l

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
  else unsafe_blit a1 ofs1 a2 ofs2 len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) [@bs] done

let map f a =
  let l = length a in
  if l = 0 then [||] else begin
    let r = create l (f(unsafe_get a 0) [@bs]) in
    for i = 1 to l - 1 do
      unsafe_set r i (f(unsafe_get a i) [@bs])
    done;
    r
  end

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) [@bs] done

let mapi f a =
  let l = length a in
  if l = 0 then [||] else begin
    let r = create l (f 0 (unsafe_get a 0) [@bs]) in
    for i = 1 to l - 1 do
      unsafe_set r i (f i (unsafe_get a i) [@bs])
    done;
    r
  end

let toList a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

(* Cannot use List.length here because the List module depends on Array. *)
let rec list_length accu = function
  | [] -> accu
  | h::t -> list_length (succ accu) t
;;

let ofList = function
    [] -> [||]
  | hd::tl as l ->
      let a = create (list_length 0 l) hd in
      let rec fill i = function
          [] -> a
        | hd::tl -> unsafe_set a i hd; fill (i+1) tl in
      fill 1 tl

let foldLeft f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i) [@bs]
  done;
  !r

let foldRight f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r [@bs]
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
