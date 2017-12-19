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

type 'a node =
  { content: 'a; mutable next: 'a cell }
and 'a cell = 'a node Js.null
and 'a t = {
  mutable length: int;
  mutable first: 'a cell;
  mutable last: 'a cell
} [@@bs.deriving abstract]
let null  = Js.null 
let return = Js.Null.return 

let create () = 
  t 
  ~length: 0
  ~first:null
  ~last:null


let clear q =
  lengthSet q  0;
  firstSet q  null;
  lastSet q  null

let add x (q : _ t) =
  let cell = return @@ node 
    ~content:x
    ~next:null
   in
  match Js.nullToOption (last q )with
  | None ->
    lengthSet  q 1;
    firstSet q cell;
    lastSet q cell
  | Some last ->
    lengthSet q (length q + 1);
    nextSet last  cell;
    lastSet q  cell

let push =
  add

let peekOpt q =
  match Js.nullToOption (first q ) with
  | None -> None
  | Some v -> Some (content v)


let popOpt q =
  match Js.nullToOption (first q ) with
  | None -> None
  | Some x  ->

    let next = next x in 
    if Js.Null.test next then 
    begin 
      clear q;
      Some (content x)
    end
    else begin 
    lengthSet q (length q - 1);
    firstSet q next;
    Some(content x) 
    end


let copy =
  let rec copy q_res prev cell =
    match Js.nullToOption cell with
    | None -> lastSet q_res  prev; q_res
    | Some x  ->
    (* Cons { content; next } *)
      let content = content x in 
      let res = return @@ node ~content ~next:null in
      begin match Js.nullToOption prev with
      | None -> firstSet q_res res
      | Some p -> nextSet p  res
      end;
      copy q_res res (next x)
  in
  fun q -> copy (t  ~length:(length q) ~first:null ~last:null)  null (first q)

let isEmpty q =
  length q = 0

let length q =
  length q

let iter =
  let rec iter f cell =
    match Js.nullToOption cell with
    | None -> ()
    | Some x  ->
      f (content x) [@bs];
      iter f (next x)
  in
  fun f q -> iter f (first q)

let fold =
  let rec fold f accu cell =
    match Js.nullToOption cell with
    | None -> accu
    | Some x  ->
      let accu = f accu (content x) [@bs] in
      fold f accu (next x)
  in
  fun f accu q -> fold f accu (first q)

let transfer q1 q2 =
  if length q1 > 0 then
    match Js.nullToOption (last q2) with
    | None ->
      lengthSet q2 (length q1);
      firstSet q2 (first q1);
      lastSet q2 (last q1);
      clear q1
    | Some l ->
      lengthSet q2  (length q2 + length q1);
      nextSet l (first q1);
      lastSet q2 (last  q1);
      clear q1
