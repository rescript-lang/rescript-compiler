(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Internals of forcing lazy values. *)

let lazy_tag = 246
let forward_tag = 250

type 'a t = {
  mutable tag : int ; (* Invariant: name  *)
  mutable _0 : 'a
}

external cast_from_lazy : 'a lazy_t -> 'b = "%identity"
external cast_to_lazy : 'b -> 'a lazy_t = "%identity"

(* external new_block : int -> int -> 'a lazy_t = "caml_obj_block" *)
let set_tag : 'a lazy_t  -> int -> unit = fun x tag -> 
  (x |. cast_from_lazy).tag<-tag
let tag : 'a lazy_t -> int = fun x -> 
  (x |. cast_from_lazy). tag 

let set_field (blk : 'arg lazy_t) (result : 'a) : unit = 
  (* Obj.set_field (Obj.repr blk) 0 (Obj.repr result) *)
  (blk |. cast_from_lazy)._0<-result
let get_field (blk : 'arg lazy_t ) : 'a = 
    (* Obj.obj (Obj.field (Obj.repr blk) 0) *)
    (blk |. cast_from_lazy)._0

let new_block_with_tag tag (value : 'a)  : 'arg lazy_t =
  ({tag ; _0 = value}  |. cast_to_lazy)
  (* let x = new_block tag 1 in 
  set_field  x  value; 
  x  *)

let from_fun (f : unit -> 'arg ) = 
  new_block_with_tag lazy_tag f 


let from_val (v : 'arg) =
  let t = tag (cast_to_lazy v) in
  if t = forward_tag || t = lazy_tag  then begin
    new_block_with_tag forward_tag v
  end else begin
    (cast_to_lazy v : 'arg lazy_t)
  end    


let forward_with_closure (blk : 'arg lazy_t) closure = 
  let result = closure () in
  (* do set_field BEFORE set_tag *)
  set_field blk result;
  set_tag blk forward_tag;
  result


exception Undefined

let raise_undefined =  (fun () -> raise Undefined)

(* Assume [blk] is a block with tag lazy *)
let force_lazy_block (blk : 'arg lazy_t) =
  let closure : unit -> 'arg = get_field blk in
  set_field blk raise_undefined;
  try
    forward_with_closure blk closure
  with e ->
    set_field blk (fun () -> raise e);
    raise e


(* Assume [blk] is a block with tag lazy *)
let force_val_lazy_block (blk : 'arg lazy_t) =
  let closure : unit -> 'arg = get_field blk  in
  set_field blk  raise_undefined;
  forward_with_closure blk closure

(* [force] is not used, since [Lazy.force] is declared as a primitive
   whose code inlines the tag tests of its argument.  This function is
   here for the sake of completeness, and for debugging purpose. *)

let force (lzv : 'arg lazy_t) : 'arg =
  let t = tag lzv in
  if t = forward_tag then get_field lzv  else
  if t <> lazy_tag then cast_from_lazy lzv
  else force_lazy_block lzv


let force_val (lzv : 'arg lazy_t) : 'arg =
  let t = tag lzv in
  if t = forward_tag then get_field lzv  else
  if t <> lazy_tag then cast_from_lazy lzv
  else force_val_lazy_block lzv





let is_val (l : 'arg lazy_t) = tag l <> lazy_tag
