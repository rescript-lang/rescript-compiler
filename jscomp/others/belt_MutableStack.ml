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


type  'a t = { mutable root : 'a opt_cell}
and 'a opt_cell = 'a cell Js.null
and 'a cell = {
  head : 'a ; 
  tail : 'a opt_cell
} [@@bs.deriving abstract]


let make () = t ~root:Js.null

let clear s = rootSet s Js.null

let copy (s : _ t) : _ t = t ~root:(root s)

let push s x = 
  rootSet s (Js.Null.return @@ cell ~head:x ~tail:(root s))

let topUndefined (s : 'a t) = 
   match Js.nullToOption (root s) with  
   | None -> Js.undefined
   | Some x -> Js.Undefined.return (head x) 

let top s = 
  match Js.nullToOption (root s) with 
  | None -> None 
  | Some x -> Some (head x)

let isEmpty s = root s = Js.null

let popUndefined s =   
  match Js.nullToOption (root s) with 
  | None -> Js.undefined
  | Some x -> 
    rootSet s (tail x);    
    Js.Undefined.return (head x)

let pop s =     
    match Js.nullToOption (root s) with 
  | None -> None
  | Some x -> 
    rootSet s (tail x);
    Some (head x)



let rec lengthAux (x : _ cell) acc = 
  match Js.nullToOption (tail x ) with 
  | None -> acc + 1 
  | Some x -> lengthAux x (acc + 1)

let size s =   
  match Js.nullToOption (root s) with 
  | None -> 0 
  | Some x -> lengthAux x 0

let rec iterAux (s : _ opt_cell) f =  
  match Js.nullToOption s with 
  | None -> ()
  | Some x -> 
    f (head x) [@bs];
    iterAux (tail x) f 

let forEachU s f =   
  iterAux (root s) f 

let forEach s f = forEachU s (fun [@bs] x -> f x)
    
let dynamicPopIterU s f =    
  let cursor = ref (root s) in 
  while !cursor != Js.null do 
    let v = Js.Null.getUnsafe !cursor in 
    rootSet s (tail v);
    f (head v) [@bs];
    cursor := root s (* using root, [f] may change it*)
  done

let dynamicPopIter s f = dynamicPopIterU s (fun [@bs] x -> f x)


