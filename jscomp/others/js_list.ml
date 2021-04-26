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

(** WIP: List operations *)
type 'a t = 'a list



let rec lengthAux len = function
  | [] -> len
  | _::l -> lengthAux (len + 1) l

let length l = lengthAux 0 l

let cons  x xs = x :: xs

let isEmpty x =  x = []

let hd = function
  | [] -> None
  | a::_ -> Some a

let tl = function
  | [] -> None
  | _::l -> Some l

let nth l n =
  if n < 0 then None else
    let rec nth_aux l n =
      match l with
      | [] -> None
      | a::l -> if n = 0 then Some a else nth_aux l (n-1)
    in nth_aux l n



let rec revAppend l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> revAppend l (a :: l2)

let rev l = revAppend l []


let rec mapRevAux f acc ls =
  match ls with
  | [] ->  acc
  | a::l ->
    mapRevAux f (f a [@bs] :: acc) l

let mapRev f ls = mapRevAux f [] ls

let map f ls = rev (mapRevAux f [] ls )

let rec iter f = function
    [] -> ()
  | a::l -> f a [@bs]; iter f l

let rec iteri i f = function
    [] -> ()
  | a::l -> f i a [@bs]; iteri (i + 1) f l

let iteri f l = iteri 0 f l

let rec foldLeft f accu l =
  match l with
    [] -> accu
  | a::l -> foldLeft f (f accu a [@bs]) l

let foldRightMaxStack = 1000


let rec tailLoop f  acc = function
  | [] -> acc
  | h :: t -> tailLoop f (f h acc [@bs]) t

let foldRight f l init =
  let rec loop n = function
    | [] -> init
    | h :: t ->
      if n < foldRightMaxStack then
        f h (loop (n+1) t) [@bs]
      else
        f h (tailLoop f init (rev t)) [@bs]
  in
  loop 0 l


let rec flattenAux  acc  lx =
  match lx with
  | [] -> rev acc
  | y::ys -> flattenAux  (revAppend  y   acc )  ys

let flatten  lx =
  flattenAux  [] lx


let rec filterRevAux f acc xs =
  match xs with
  | [] ->  acc
  | y :: ys ->
    begin match f y [@bs] with
      | false ->   filterRevAux f acc ys
      | true -> filterRevAux f  (y::acc) ys
    end

let filter f xs  =  rev (filterRevAux f [] xs )

let rec filterMapRevAux (f:  'a -> 'b option [@bs]) acc xs =
  match xs with
  | [] ->  acc
  | y :: ys ->
    begin match f y [@bs] with
      | None ->   filterMapRevAux f acc ys
      | Some z -> filterMapRevAux f  (z::acc) ys
    end

let filterMap f xs =
  rev (filterMapRevAux f [] xs)


let rec countByAux f acc xs =
  match xs with
  | [] -> acc
  | y::ys ->
    countByAux f (if f y [@bs] then acc + 1 else acc) ys

let countBy f xs = countByAux f 0 xs

let init n f =
  Js_vector.toList (Js_vector.init n f )

external createUnsafe : int -> 'a Js_vector.t =
  "Array" [@@bs.new]

let toVector xs =
  match xs with
  | [] -> [||]
  | l ->
    let a = createUnsafe (length l) in
    let rec fill i = function
      | [] -> a
      | hd::tl -> Js_array2.unsafe_set a i hd; fill (i+1) tl in
    fill 0 l

let rec equal cmp xs ys =
  match xs,ys with
  | [], [] -> true
  | x::xs, y::ys ->
    if cmp x y [@bs] then equal cmp xs ys
    else false
  | _, _ -> false
