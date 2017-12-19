(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Francois Pottier, projet Cristal, INRIA Rocquencourt           *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* exception Empty *)

type 'a node =
  { content: 'a; mutable next: 'a cell }
and 'a cell = 'a node Js.null
[@@bs.deriving abstract ]

type 'a t = {
  mutable length: int;
  mutable first: 'a cell;
  mutable last: 'a cell
}
let null  = Js.null 
let return = Js.Null.return 
let create () = {
  length = 0;
  first = null;
  last = null
}

let clear q =
  q.length <- 0;
  q.first <- null;
  q.last <- null

let add x (q : _ t) =
  let cell = return @@ node 
    ~content:x
    ~next:null
   in
  match Js.nullToOption q.last with
  | None ->
    q.length <- 1;
    q.first <- cell;
    q.last <- cell
  | Some last ->
    q.length <- q.length + 1;
    nextSet last  cell;
    q.last <- cell

let push =
  add

let peekOpt q =
  match Js.nullToOption q.first with
  | None -> None
  | Some v -> Some (content v)


let popOpt q =
  match Js.nullToOption q.first with
  | None -> None
  | Some x  ->

    let next = next x in 
    if Js.Null.test next then 
    begin 
      clear q;
      Some (content x)
    end
    else begin 
    q.length <- q.length - 1;
    q.first <- next;
    Some(content x) 
    end


let copy =
  let rec copy q_res prev cell =
    match Js.nullToOption cell with
    | None -> q_res.last <- prev; q_res
    | Some x  ->
    (* Cons { content; next } *)
      let content = content x in 
      let res = return @@ node ~content ~next:null in
      begin match Js.nullToOption prev with
      | None -> q_res.first <- res
      | Some p -> nextSet p  res
      end;
      copy q_res res (next x)
  in
  fun q -> copy { length = q.length; first = null; last = null } null q.first

let isEmpty q =
  q.length = 0

let length q =
  q.length

let iter =
  let rec iter f cell =
    match Js.nullToOption cell with
    | None -> ()
    | Some x  ->
      f (content x) [@bs];
      iter f (next x)
  in
  fun f q -> iter f q.first

let fold =
  let rec fold f accu cell =
    match Js.nullToOption cell with
    | None -> accu
    | Some x  ->
      let accu = f accu (content x) [@bs] in
      fold f accu (next x)
  in
  fun f accu q -> fold f accu q.first

let transfer q1 q2 =
  if q1.length > 0 then
    match Js.nullToOption q2.last with
    | None ->
      q2.length <- q1.length;
      q2.first <- q1.first;
      q2.last <- q1.last;
      clear q1
    | Some last ->
      q2.length <- q2.length + q1.length;
      nextSet last q1.first;
      q2.last <- q1.last;
      clear q1
