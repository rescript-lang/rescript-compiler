(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type 'a t = 'a array

external length : 'a array -> int = "%array_length"
external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external make: int -> 'a -> 'a array = "caml_make_vect"
external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"

(** **param** a array

    **param** p predicate
*)
let filterInPlace p a =
  let  i = ref 0 in
  let j = ref 0 in
  while i.contents < Js_array2.length a do
    let v = Js_array2.unsafe_get a i.contents in
    if p v   [@bs] then
      begin
        Js_array2.unsafe_set a j.contents v ;
        j.contents <- j.contents + 1
      end;
    i.contents <- i.contents + 1
  done;
  Js_array2.removeFromInPlace a ~pos:j.contents |. ignore

let empty a  =
  Js_array2.removeFromInPlace a ~pos:0 |. ignore

let pushBack x xs =
  Js_array2.push xs x |. ignore

(** Find by JS (===)  equality *)
let memByRef x xs =
  Js_array2.indexOf xs x >= 0

let iter f  xs =
  for i = 0 to Js_array2.length xs - 1 do
    f (Js_array2.unsafe_get xs i) [@bs]
  done

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) [@bs] done



external createUnsafe : int -> 'a t = "Array" [@@bs.new]

(* let ofList xs =  *)
(*   match xs with  *)
(*   | [] -> [||] *)
(*   | l -> *)
(*     let a = createUnsafe (Js_list.length l) in  *)
(*     let rec fill i = function *)
(*         | [] -> a *)
(*         | hd::tl -> Array.unsafe_set a i hd; fill (i+1) tl in *)
(*     fill 0 l *)

let toList a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []


let init n f  =
  let v = createUnsafe n in
  for i = 0 to n - 1 do
    unsafe_set v i (f i [@bs])
  done ;
  v

let copy x =
  let len = length x in
  let b = createUnsafe len in
  for i = 0 to len - 1 do
    unsafe_set b i (unsafe_get x i)
  done  ;
  b

let map f a =
  let l = Js_array2.length a in
  let r = createUnsafe l in
  for i = 0 to l - 1 do
      unsafe_set r i (f(unsafe_get a i) [@bs])
  done;
  r


let foldLeft f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r.contents<- f r.contents (unsafe_get a i) [@bs]
  done;
  r.contents


let foldRight f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r.contents<- f (unsafe_get a i) r.contents [@bs]
  done;
  r.contents


let mapi f a =
  let l = length a in
  if l = 0 then [||] else begin
    let r= createUnsafe l in
    for i = 0 to l - 1 do
      unsafe_set r i (f i (unsafe_get a i) [@bs])
    done;
    r
  end

let append x a =
  Js_array2.concat a [|x|]

(* TODO: add `append` *)
