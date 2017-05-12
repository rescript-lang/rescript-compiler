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
  | a::l -> lengthAux (len + 1) l

let length l = lengthAux 0 l

let cons  x xs = x :: xs 

let isEmpty x =  x = []

let hd = function
  | [] -> None
  | a::l -> Some a

let tl = function
  | [] -> None
  | a::l -> Some l

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

let rec map f ls = rev @@ mapRevAux f [] ls 


