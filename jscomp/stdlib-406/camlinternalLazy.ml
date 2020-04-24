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

external new_block : int -> int -> Obj.t = "caml_obj_block"

let set_field (blk : 'arg lazy_t) result = 
  Obj.set_field (Obj.repr blk) 0 (Obj.repr result)

let new_block_with_tag tag (value : 'a)  : 'arg lazy_t =
  let x = new_block tag 1 in 
  set_field (Obj.obj x)  (Obj.repr value); 
  (Obj.obj x : 'arg lazy_t)

let from_fun (f : unit -> 'arg ) = 
  new_block_with_tag lazy_tag f 


let from_val (v : 'arg) =
  let t = Obj.tag (Obj.repr v) in
  if t = forward_tag || t = lazy_tag || false (* t = Obj.double_tag *) then begin
    new_block_with_tag forward_tag v
  end else begin
    (Obj.magic v : 'arg lazy_t)
  end    
external set_tag : Obj.t -> int -> unit = "caml_obj_set_tag"

let forward_with_closure (blk : 'arg lazy_t) closure = 
  let result = closure () in
  (* do set_field BEFORE set_tag *)
  set_field blk result;
  set_tag (Obj.repr blk) forward_tag;
  result


exception Undefined

let raise_undefined = Obj.repr (fun () -> raise Undefined)

(* Assume [blk] is a block with tag lazy *)
let force_lazy_block (blk : 'arg lazy_t) =
  let closure = (Obj.obj (Obj.field (Obj.repr blk) 0) : unit -> 'arg) in
  set_field blk raise_undefined;
  try
    forward_with_closure blk closure
  with e ->
    set_field blk (Obj.repr (fun () -> raise e));
    raise e


(* Assume [blk] is a block with tag lazy *)
let force_val_lazy_block (blk : 'arg lazy_t) =
  let closure = (Obj.obj (Obj.field (Obj.repr blk) 0) : unit -> 'arg) in
  set_field blk  raise_undefined;
  forward_with_closure blk closure

(* [force] is not used, since [Lazy.force] is declared as a primitive
   whose code inlines the tag tests of its argument.  This function is
   here for the sake of completeness, and for debugging purpose. *)

let force (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> lazy_tag then (Obj.obj x : 'arg)
  else force_lazy_block lzv


let force_val (lzv : 'arg lazy_t) =
  let x = Obj.repr lzv in
  let t = Obj.tag x in
  if t = forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
  if t <> lazy_tag then (Obj.obj x : 'arg)
  else force_val_lazy_block lzv





let is_val (l : 'arg lazy_t) = Obj.tag (Obj.repr l) <> lazy_tag
