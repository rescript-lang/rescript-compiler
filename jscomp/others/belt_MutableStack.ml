(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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


type  'a t = { mutable root : 'a opt_cell}
and 'a opt_cell = 'a cell option
and 'a cell = {
  head : 'a ; 
  tail : 'a opt_cell
} 


let make () = {root = None}

let clear s =  s.root <- None

let copy (s : _ t) : _ t = {root = s.root}

let push s x = 
  s.root <- Some {head = x; tail = s.root}

let topUndefined (s : 'a t) = 
   match s.root with  
   | None -> Js.undefined
   | Some x -> Js.Undefined.return x.head 

let top s = 
  match s.root with 
  | None -> None 
  | Some x -> Some x.head

let isEmpty s = s.root = None

let popUndefined s =   
  match s.root with 
  | None -> Js.undefined
  | Some x -> 
    s.root <- x.tail;    
    Js.Undefined.return x.head

let pop s =     
    match s.root with 
  | None -> None
  | Some x -> 
    s.root <- x.tail;
    Some x.head



let rec lengthAux (x : _ cell) acc = 
  match x.tail with 
  | None -> acc + 1 
  | Some x -> lengthAux x (acc + 1)

let size s =   
  match s.root with 
  | None -> 0 
  | Some x -> lengthAux x 0

let rec iterAux (s : _ opt_cell) f =  
  match s with 
  | None -> ()
  | Some x -> 
    f x.head [@bs];
    iterAux x.tail f 

let forEachU s f =   
  iterAux s.root f 

let forEach s f = forEachU s (fun [@bs] x -> f x)


let rec dynamicPopIterU s  f = 
  match s.root with 
  | Some {tail; head }-> 
    s.root <- tail;
    f head [@bs] ;
    dynamicPopIterU s  f (* using root, `f` may change it*)
 | None -> ()   



let dynamicPopIter s f = dynamicPopIterU s (fun [@bs] x -> f x)


