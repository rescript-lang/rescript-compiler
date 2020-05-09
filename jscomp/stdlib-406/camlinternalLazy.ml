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
  mutable tag : string [@bs.as "RE_LAZY"] ; 
  (* Invariant: name  *)
  mutable value : 'a (* its type is ['a] or [unit -> 'a ] *)
}


let%private status_todo = "todo" (* used to be lazy tag in native *)
let%private status_done = "done" (* used to be forward_tag in native *)
external%private magic : 'a -> 'b = "%identity"
external%private fnToVal : (unit -> 'a [@bs]) -> 'a = "%identity"
external%private valToFn :  'a -> (unit -> 'a [@bs])  = "%identity"
external%private castToLazy : 'a t ->  'a lazy_t  = "%identity"
external%private castToConcrete : 'a lazy_t -> 'a t   = "%identity"
external%private lazyBox : 'a -> 'a lazy_t = "%identity"
external%private lazyUnBox : 'a lazy_t  -> 'a  = "%identity"

let%private lazy_boxed (type a) (l : a ) : bool  = 
  if Js.testAny l then false 
  else   
    let t = (magic l : _ t  ).tag in 
    t = status_done || t = status_todo  

let is_val (type a ) (l : a lazy_t) : bool = 
  Js.testAny l || ((castToConcrete l ).tag  <> status_todo)

let from_fun (type arg ) f : arg lazy_t = 
  castToLazy {tag = status_todo; value = fnToVal f}


let from_val (type arg ) (v : arg) : arg lazy_t=
  if lazy_boxed v  then begin
    castToLazy {tag = status_done ; value = v} 
  end else begin
    lazyBox v 
  end    

exception Undefined

let%private forward_with_closure (type a ) (blk : a t) (closure : unit -> a [@bs]) : a = 
  let result = closure () [@bs] in
  (* do set_field BEFORE set_tag *)
  blk.value <- result;
  blk.tag<- status_done;
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
  if lazy_boxed  lzv then 
    if is_val lzv then (castToConcrete lzv : _ t).value else
      force_lazy_block (castToConcrete lzv : _ t)
  else  lazyUnBox lzv



let force_val (lzv : 'arg lazy_t) : 'arg =
  if lazy_boxed  lzv then 
    if is_val lzv then (castToConcrete lzv : _ t).value  else
      force_val_lazy_block (castToConcrete lzv : _ t)
  else  lazyUnBox lzv

