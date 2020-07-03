(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



exception Empty

type 'a t = {
  mutable prev : 'a t;
  mutable next : 'a t;
}

type 'a node = {
  mutable node_prev : 'a t;
  mutable node_next : 'a t;
  mutable node_data : 'a;
  mutable node_active : bool;
}

external seq_of_node : 'a node -> 'a t = "%identity"
external node_of_seq : 'a t -> 'a node = "%identity"

(* +-----------------------------------------------------------------+
   | Operations on nodes                                             |
   +-----------------------------------------------------------------+ *)

let get node =
  node.node_data

let set node data =
  node.node_data <- data

let remove node =
  if node.node_active then begin
    node.node_active <- false;
    let seq = seq_of_node node in
    seq.prev.next <- seq.next;
    seq.next.prev <- seq.prev
  end

(* +-----------------------------------------------------------------+
   | Operations on sequences                                         |
   +-----------------------------------------------------------------+ *)

let create () =
  let rec seq = { prev = seq; next = seq } in
  seq

let is_empty seq = seq.next == seq

let length seq =
  let rec loop curr len =
    if curr == seq then
      len
    else
      let node = node_of_seq curr in loop node.node_next (len + 1)
  in
  loop seq.next 0

let add_l data seq =
  let node = { node_prev = seq; node_next = seq.next; node_data = data; node_active = true } in
  seq.next.prev <- seq_of_node node;
  seq.next <- seq_of_node node;
  node

let add_r data seq =
  let node = { node_prev = seq.prev; node_next = seq; node_data = data; node_active = true } in
  seq.prev.next <- seq_of_node node;
  seq.prev <- seq_of_node node;
  node

let take_l seq =
  if is_empty seq then
    raise Empty
  else begin
    let node = node_of_seq seq.next in
    remove node;
    node.node_data
  end

let take_r seq =
  if is_empty seq then
    raise Empty
  else begin
    let node = node_of_seq seq.prev in
    remove node;
    node.node_data
  end

let take_opt_l seq =
  if is_empty seq then
    None
  else begin
    let node = node_of_seq seq.next in
    remove node;
    Some node.node_data
  end

let take_opt_r seq =
  if is_empty seq then
    None
  else begin
    let node = node_of_seq seq.prev in
    remove node;
    Some node.node_data
  end

let transfer_l s1 s2 =
  s2.next.prev <- s1.prev;
  s1.prev.next <- s2.next;
  s2.next <- s1.next;
  s1.next.prev <- s2;
  s1.prev <- s1;
  s1.next <- s1

let transfer_r s1 s2 =
  s2.prev.next <- s1.next;
  s1.next.prev <- s2.prev;
  s2.prev <- s1.prev;
  s1.prev.next <- s2;
  s1.prev <- s1;
  s1.next <- s1

let iter_l f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node.node_data;
      loop node.node_next
    end
  in
  loop seq.next

let iter_r f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node.node_data;
      loop node.node_prev
    end
  in
  loop seq.prev

let iter_node_l f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node;
      loop node.node_next
    end
  in
  loop seq.next

let iter_node_r f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node;
      loop node.node_prev
    end
  in
  loop seq.prev

let fold_l f seq acc =
  let rec loop curr acc =
    if curr == seq then
      acc
    else
      let node = node_of_seq curr in
      if node.node_active then
        loop node.node_next (f node.node_data acc)
      else
        loop node.node_next acc
  in
  loop seq.next acc

let fold_r f seq acc =
  let rec loop curr acc =
    if curr == seq then
      acc
    else
      let node = node_of_seq curr in
      if node.node_active then
        loop node.node_prev (f node.node_data acc)
      else
        loop node.node_prev acc
  in
  loop seq.prev acc

let find_node_l f seq =
  let rec loop curr =
    if curr != seq then
      let node = node_of_seq curr in
      if node.node_active then
        if f node.node_data then
          node
        else
          loop node.node_next
      else
        loop node.node_next
    else
      raise Not_found
  in
  loop seq.next

let find_node_r f seq =
  let rec loop curr =
    if curr != seq then
      let node = node_of_seq curr in
      if node.node_active then
        if f node.node_data then
          node
        else
          loop node.node_prev
      else
        loop node.node_prev
    else
      raise Not_found
  in
  loop seq.prev

let find_node_opt_l f seq =
  try Some (find_node_l f seq) with Not_found -> None

let find_node_opt_r f seq =
  try Some (find_node_r f seq) with Not_found -> None
