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
type 'a t = {
  mutable tag : int [@bs.as "tag"] ; 
  (* Invariant: name  *)
  mutable value : 'a (* [@bs.as "val"] *)
  (* its type is ['a] or [unit -> 'a ] *)
}


let%private lazy_tag = 246
let%private forward_tag = 250
external%private magic : 'a -> 'b = "%identity"

let%private lazy_boxed (type a) (l : a ) : bool  = 
  if Js.testAny l then false 
  else   
    let t = (magic l : _ t  ).tag in 
    t = forward_tag || t = lazy_tag  

let is_val (type a ) (l : a lazy_t) : bool = 
  Js.testAny l || ((magic l : _ t ).tag  <> lazy_tag)

let from_fun (type arg ) (f : unit -> arg ) : arg lazy_t = 
  (magic {tag = lazy_tag; value = f} : arg lazy_t) 


let from_val (type arg ) (v : arg) : arg lazy_t=
  if lazy_boxed v  then begin
    (magic {tag = forward_tag ; value = v} : arg lazy_t )
  end else begin
    (magic v : arg lazy_t)
  end    

exception Undefined

let%private forward_with_closure (type a ) (blk : a t) (closure : unit -> a) : a = 
  let result = closure () in
  (* do set_field BEFORE set_tag *)
  blk.value <- result;
  blk.tag<- forward_tag;
  result


let%private raise_undefined =  (fun () -> raise Undefined)

(* Assume [blk] is a block with tag lazy *)
let%private force_lazy_block (type a ) (blk : a t) : a  =
  let closure : unit -> a = magic blk.value in
  blk.value <- magic raise_undefined;
  try
    forward_with_closure blk closure
  with e ->
    blk.value <- magic (fun () -> raise e);
    raise e


(* Assume [blk] is a block with tag lazy *)
let%private force_val_lazy_block (type a ) (blk : a t) : a  =
  let closure : unit -> a = magic blk.value  in
  blk.value <-  magic raise_undefined;
  forward_with_closure blk closure



let force (type a ) (lzv : a lazy_t) : a =
  if lazy_boxed  lzv then 
    if is_val lzv then (magic lzv : _ t).value else
      force_lazy_block (magic lzv : _ t)
  else  magic lzv



let force_val (lzv : 'arg lazy_t) : 'arg =
  if lazy_boxed  lzv then 
    if is_val lzv then (magic lzv : _ t).value  else
      force_val_lazy_block (magic lzv : _ t)
  else  magic lzv

