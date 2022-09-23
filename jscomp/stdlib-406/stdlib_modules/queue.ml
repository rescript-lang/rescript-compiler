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

exception Empty

type 'a cell =
  | Nil
  | Cons of { content: 'a; mutable next: 'a cell }

type 'a t = {
  mutable length: int;
  mutable first: 'a cell;
  mutable last: 'a cell
}

let create () = {
  length = 0;
  first = Nil;
  last = Nil
}

let clear q =
  q.length <- 0;
  q.first <- Nil;
  q.last <- Nil

let add x q =
  let cell = Cons {
    content = x;
    next = Nil
  } in
  match q.last with
  | Nil ->
    q.length <- 1;
    q.first <- cell;
    q.last <- cell
  | Cons last ->
    q.length <- q.length + 1;
    last.next <- cell;
    q.last <- cell

let push =
  add

let peek q =
  match q.first with
  | Nil -> raise Empty
  | Cons { content } -> content

let top =
  peek

let take q =
  match q.first with
  | Nil -> raise Empty
  | Cons { content; next = Nil } ->
    clear q;
    content
  | Cons { content; next } ->
    q.length <- q.length - 1;
    q.first <- next;
    content

let pop =
  take

let copy =
  let rec copy q_res prev cell =
    match cell with
    | Nil -> q_res.last <- prev; q_res
    | Cons { content; next } ->
      let res = Cons { content; next = Nil } in
      begin match prev with
      | Nil -> q_res.first <- res
      | Cons p -> p.next <- res
      end;
      copy q_res res next
  in
  fun q -> copy { length = q.length; first = Nil; last = Nil } Nil q.first

let is_empty q =
  q.length = 0

let length q =
  q.length

let iter =
  let rec iter f cell =
    match cell with
    | Nil -> ()
    | Cons { content; next } ->
      f content;
      iter f next
  in
  fun f q -> iter f q.first

let fold =
  let rec fold f accu cell =
    match cell with
    | Nil -> accu
    | Cons { content; next } ->
      let accu = f accu content in
      fold f accu next
  in
  fun f accu q -> fold f accu q.first

let transfer q1 q2 =
  if q1.length > 0 then
    match q2.last with
    | Nil ->
      q2.length <- q1.length;
      q2.first <- q1.first;
      q2.last <- q1.last;
      clear q1
    | Cons last ->
      q2.length <- q2.length + q1.length;
      last.next <- q1.first;
      q2.last <- q1.last;
      clear q1
