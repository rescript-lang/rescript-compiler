(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Francois Pottier, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

exception Empty

(* OCaml currently does not allow the components of a sum type to be
   mutable. Yet, for optimal space efficiency, we must have cons cells
   whose [next] field is mutable. This leads us to define a type of
   cyclic lists, so as to eliminate the [Nil] case and the sum
   type. *)

type 'a cell = {
    content: 'a;
    mutable next: 'a cell
  }

(* A queue is a reference to either nothing or some cell of a cyclic
   list. By convention, that cell is to be viewed as the last cell in
   the queue. The first cell in the queue is then found in constant
   time: it is the next cell in the cyclic list. The queue's length is
   also recorded, so as to make [length] a constant-time operation.

   The [tail] field should really be of type ['a cell option], but
   then it would be [None] when [length] is 0 and [Some] otherwise,
   leading to redundant memory allocation and accesses. We avoid this
   overhead by filling [tail] with a dummy value when [length] is 0.
   Of course, this requires bending the type system's arm slightly,
   because it does not have dependent sums. *)

type 'a t = {
    mutable length: int;
    mutable tail: 'a cell
  }

let create () = {
  length = 0;
  tail = Obj.magic None
}

let clear q =
  q.length <- 0;
  q.tail <- Obj.magic None

let add x q =
  if q.length = 0 then
    let rec cell = {
      content = x;
      next = cell
    } in
    q.length <- 1;
    q.tail <- cell
  else
    let tail = q.tail in
    let head = tail.next in
    let cell = {
      content = x;
      next = head
    } in
    q.length <- q.length + 1;
    tail.next <- cell;
    q.tail <- cell

let push =
  add

let peek q =
  if q.length = 0 then
    raise Empty
  else
    q.tail.next.content

let top =
  peek

let take q =
  if q.length = 0 then raise Empty;
  q.length <- q.length - 1;
  let tail = q.tail in
  let head = tail.next in
  if head == tail then
    q.tail <- Obj.magic None
  else
    tail.next <- head.next;
  head.content

let pop =
  take

let copy q =
  if q.length = 0 then
    create()
  else
    let tail = q.tail in

    let rec tail' = {
      content = tail.content;
      next = tail'
    } in

    let rec copy prev cell =
      if cell != tail
      then let res = {
        content = cell.content;
        next = tail'
      } in prev.next <- res;
      copy res cell.next in

    copy tail' tail.next;
    {
      length = q.length;
      tail = tail'
    }

let is_empty q =
  q.length = 0

let length q =
  q.length

let iter f q =
  if q.length > 0 then
    let tail = q.tail in
    let rec iter cell =
      f cell.content;
      if cell != tail then
        iter cell.next in
    iter tail.next

let fold f accu q =
  if q.length = 0 then
    accu
  else
    let tail = q.tail in
    let rec fold accu cell =
      let accu = f accu cell.content in
      if cell == tail then
        accu
      else
        fold accu cell.next in
    fold accu tail.next

let transfer q1 q2 =
  let length1 = q1.length in
  if length1 > 0 then
    let tail1 = q1.tail in
    clear q1;
    if q2.length > 0 then begin
      let tail2 = q2.tail in
      let head1 = tail1.next in
      let head2 = tail2.next in
      tail1.next <- head2;
      tail2.next <- head1
    end;
    q2.length <- q2.length + length1;
    q2.tail <- tail1
