(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(* A survey and measurements of more optimized implementations can be found at:

    https://jsthomas.github.io/map-comparison.html

   See discussion in https://github.com/ocsigen/lwt/pull/347. *)
let tail_recursive_map f l =
  List.rev (List.rev_map f l)

let tail_recursive_mapi f l =
  let rec inner acc i = function
    | [] -> List.rev acc
    | hd::tl -> (inner [@ocaml.tailcall]) ((f i hd)::acc) (i + 1) tl
  in
  inner [] 0 l

open Lwt.Infix

let rec iter_s f l =
  match l with
  | [] ->
    Lwt.return_unit
  | x :: l ->
    Lwt.apply f x >>= fun () ->
    iter_s f l

let iter_p f l =
  let ts = tail_recursive_map (Lwt.apply f) l in
  Lwt.join ts

let rec iteri_s i f l =
  match l with
  | [] ->
    Lwt.return_unit
  | x :: l ->
    Lwt.apply (f i) x >>= fun () ->
    iteri_s (i + 1) f l

let iteri_s f l = iteri_s 0 f l

let iteri_p f l =
  let f' i = Lwt.apply (f i) in
  let ts = tail_recursive_mapi f' l in
  Lwt.join ts

let map_s f l =
  let rec inner acc = function
    | [] -> List.rev acc |> Lwt.return
    | hd::tl ->
      Lwt.apply f hd >>= fun r ->
      (inner [@ocaml.tailcall]) (r::acc) tl
  in
  inner [] l

let rec _collect acc = function
  | [] ->
    List.rev acc |> Lwt.return
  | t::ts ->
    t >>= fun i ->
    (_collect [@ocaml.tailcall]) (i::acc) ts

let map_p f l =
  let ts = tail_recursive_map (Lwt.apply f) l in
  _collect [] ts

let filter_map_s f l =
  let rec inner acc = function
    | []     -> List.rev acc |> Lwt.return
    | hd::tl ->
      Lwt.apply f hd >>= function
      | Some v -> (inner [@ocaml.tailcall]) (v::acc) tl
      | None -> (inner [@ocaml.tailcall]) acc tl
  in
  inner [] l

let filter_map_p f l =
  let rec _collect_optional acc = function
  | []    -> List.rev acc |> Lwt.return
  | t::ts ->
    t >>= function
    | Some v -> (_collect_optional [@ocaml.tailcall]) (v::acc) ts
    | None -> (_collect_optional [@ocaml.tailcall]) acc ts
  in
  let ts = tail_recursive_map (Lwt.apply f) l in
  _collect_optional [] ts

let mapi_s f l =
  let rec inner acc i = function
    | []     -> List.rev acc |> Lwt.return
    | hd::tl ->
      Lwt.apply (f i) hd >>= fun v ->
      (inner [@ocaml.tailcall]) (v::acc) (i+1) tl
  in
  inner [] 0 l

let mapi_p f l =
  let f' i = Lwt.apply (f i) in
  let ts = tail_recursive_mapi f' l in
  _collect [] ts

let rec rev_map_append_s acc f l =
  match l with
  | [] ->
    Lwt.return acc
  | x :: l ->
    Lwt.apply f x >>= fun x ->
    rev_map_append_s (x :: acc) f l

let rev_map_s f l =
  rev_map_append_s [] f l

let rec rev_map_append_p acc f l =
  match l with
  | [] ->
    acc
  | x :: l ->
    rev_map_append_p
      (Lwt.apply f x >>= fun x ->
       acc >|= fun l ->
       x :: l) f l

let rev_map_p f l =
  rev_map_append_p Lwt.return_nil f l

let rec fold_left_s f acc l =
  match l with
  | [] ->
    Lwt.return acc
  | x :: l ->
    Lwt.apply (f acc) x >>= fun acc ->
    (fold_left_s [@ocaml.tailcall]) f acc l

let fold_right_s f l acc =
  let rec inner f a = function
    | []     -> Lwt.return a
    | hd::tl -> (Lwt.apply (f hd) a) >>= fun a' ->
      (inner [@ocaml.tailcall]) f a' tl
  in
  inner f acc (List.rev l)

let rec for_all_s f l =
  match l with
  | [] ->
    Lwt.return_true
  | x :: l ->
    Lwt.apply f x >>= function
    | true ->
      (for_all_s [@ocaml.tailcall]) f l
    | false ->
      Lwt.return_false

let for_all_p f l =
  map_p f l >>= fun bl -> List.for_all (fun x -> x) bl |> Lwt.return

let rec exists_s f l =
  match l with
  | [] ->
    Lwt.return_false
  | x :: l ->
    Lwt.apply f x >>= function
    | true ->
      Lwt.return_true
    | false ->
      (exists_s [@ocaml.tailcall]) f l

let exists_p f l =
  map_p f l >>= fun bl -> List.exists (fun x -> x) bl |> Lwt.return

let rec find_s f l =
  match l with
  | [] ->
    Lwt.fail Not_found
  | x :: l ->
    Lwt.apply f x >>= function
    | true ->
      Lwt.return x
    | false ->
      (find_s [@ocaml.tailcall]) f l

let _optionalize f x =
  f x >>= fun b -> if b then Lwt.return (Some x) else Lwt.return None

let filter_s f l =
  filter_map_s (_optionalize f) l

let filter_p f l =
   filter_map_p (_optionalize f) l

let partition_s f l =
  let rec inner acc1 acc2 = function
    | []     -> Lwt.return (List.rev acc1, List.rev acc2)
    | hd::tl -> Lwt.apply f hd >>= fun b ->
        if b then
          inner (hd::acc1) acc2 tl
        else
          inner acc1 (hd::acc2) tl
  in
  inner [] [] l

let partition_p f l =
  let g x = Lwt.apply f x >>= fun b -> Lwt.return (b, x) in
  map_p g l >>= fun tl ->
  let group1 = tail_recursive_map snd @@ List.filter fst tl in
  let group2 =
    tail_recursive_map snd @@ List.filter (fun x -> not @@ fst x) tl in
  Lwt.return (group1, group2)
