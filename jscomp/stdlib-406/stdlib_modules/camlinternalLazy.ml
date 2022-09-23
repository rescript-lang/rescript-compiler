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

 [@@@bs.config { flags = [|"-bs-no-cross-module-opt" |]}]
 
(* Internals of forcing lazy values. *)
type 'a t = {
  mutable tag : bool [@bs.as "LAZY_DONE"] ; 
  (* Invariant: name  *)
  mutable value : 'a [@bs.as "VAL"]
  (* its type is ['a] or [unit -> 'a ] *)
}


external%private fnToVal : (unit -> 'a [@bs]) -> 'a = "%identity"
external%private valToFn :  'a -> (unit -> 'a [@bs])  = "%identity"
external%private castToConcrete : 'a lazy_t -> 'a t   = "%identity"

let is_val (type a ) (l : a lazy_t) : bool = 
  (castToConcrete l ).tag  



exception Undefined

let%private forward_with_closure (type a ) (blk : a t) (closure : unit -> a [@bs]) : a = 
  let result = closure () [@bs] in
  (* do set_field BEFORE set_tag *)
  blk.value <- result;
  blk.tag<- true;
  result


let%private raise_undefined =  (fun [@bs] () -> raise Undefined)

(* Assume [blk] is a block with tag lazy *)
let%private force_lazy_block (type a ) (blk : a t) : a  =
  let closure = valToFn blk.value in
  blk.value <- fnToVal raise_undefined;
  try
    forward_with_closure blk closure
  with e ->
    blk.value <- fnToVal (fun [@bs] () -> raise e);
    raise e


(* Assume [blk] is a block with tag lazy *)
let%private force_val_lazy_block (type a ) (blk : a t) : a  =
  let closure  = valToFn blk.value  in
  blk.value <-  fnToVal raise_undefined;
  forward_with_closure blk closure



let force (type a ) (lzv : a lazy_t) : a =
    let lzv = (castToConcrete lzv : _ t) in 
    if lzv.tag  then lzv.value else
      force_lazy_block lzv




let force_val (type a) (lzv : a lazy_t) : a =
  let lzv : _ t = castToConcrete lzv in 
  if lzv.tag then lzv.value  else
    force_val_lazy_block lzv


