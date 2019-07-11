(* Copyright (c) 2013, Simon Cruanes All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. Redistributions in binary
   form must reproduce the above copyright notice, this list of conditions and
   the following disclaimer in the documentation and/or other materials
   provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE. *)

(** {1 Simple S-expression parsing/printing} *)

type t = [`Atom of string | `List of t list]

let equal a b = a = b
let compare a b = Pervasives.compare a b
let hash a = Hashtbl.hash a
let of_int x = `Atom (string_of_int x)
let of_float x = `Atom (string_of_float x)
let of_bool x = `Atom (string_of_bool x)
let atom x = `Atom x
let of_unit = `List []
let of_list l = `List l
let of_rev_list l = `List (List.rev l)
let of_pair (x, y) = `List [x; y]
let of_triple (x, y, z) = `List [x; y; z]
let of_quad (x, y, z, u) = `List [x; y; z; u]
let of_variant name args = `List (`Atom name :: args)
let of_field name t = `List [`Atom name; t]
let of_record l = `List (List.map (fun (n, x) -> of_field n x) l)

(** {6 Traversal of S-exp} *)

module Traverse = struct
  type 'a conv = t -> 'a option

  let return x = Some x
  let ( >|= ) e f = match e with None -> None | Some x -> Some (f x)
  let ( >>= ) e f = match e with None -> None | Some x -> f x

  let map_opt f l =
    let rec recurse acc l =
      match l with
      | [] -> Some (List.rev acc)
      | x :: l' -> (
        match f x with None -> None | Some y -> recurse (y :: acc) l' ) in
    recurse [] l

  let rec _list_any f l =
    match l with
    | [] -> None
    | x :: tl -> (
      match f x with Some _ as res -> res | None -> _list_any f tl )

  let list_any f e = match e with `Atom _ -> None | `List l -> _list_any f l

  let rec _list_all f acc l =
    match l with
    | [] -> List.rev acc
    | x :: tl -> (
      match f x with
      | Some y -> _list_all f (y :: acc) tl
      | None -> _list_all f acc tl )

  let list_all f e = match e with `Atom _ -> [] | `List l -> _list_all f [] l

  let _try_atom e f =
    match e with
    | `List _ -> None
    | `Atom x -> ( try Some (f x) with _ -> None )

  let to_int e = _try_atom e int_of_string
  let to_bool e = _try_atom e bool_of_string
  let to_float e = _try_atom e float_of_string
  let to_string e = _try_atom e (fun x -> x)
  let to_pair e = match e with `List [x; y] -> Some (x, y) | _ -> None

  let to_pair_with f1 f2 e =
    to_pair e
    >>= fun (x, y) -> f1 x >>= fun x -> f2 y >>= fun y -> return (x, y)

  let to_triple e =
    match e with `List [x; y; z] -> Some (x, y, z) | _ -> None

  let to_triple_with f1 f2 f3 e =
    to_triple e
    >>= fun (x, y, z) ->
    f1 x >>= fun x -> f2 y >>= fun y -> f3 z >>= fun z -> return (x, y, z)

  let to_list e = match e with `List l -> Some l | `Atom _ -> None

  let to_list_with f (e : t) =
    match e with `List l -> map_opt f l | `Atom _ -> None

  let rec _get_field name l =
    match l with
    | `List [`Atom n; x] :: _ when name = n -> Some x
    | _ :: tl -> _get_field name tl
    | [] -> None

  let get_field name e =
    match e with `List l -> _get_field name l | `Atom _ -> None

  let field name f e = get_field name e >>= f

  let rec _get_field_list name l =
    match l with
    | `List (`Atom n :: tl) :: _ when name = n -> Some tl
    | _ :: tl -> _get_field_list name tl
    | [] -> None

  let field_list name f e =
    match e with `List l -> _get_field_list name l >>= f | `Atom _ -> None

  let rec _get_variant s args l =
    match l with
    | [] -> None
    | (s', f) :: _ when s = s' -> f args
    | _ :: tl -> _get_variant s args tl

  let get_variant l e =
    match e with
    | `List (`Atom s :: args) -> _get_variant s args l
    | `List _ -> None
    | `Atom s -> _get_variant s [] l

  let get_exn e =
    match e with None -> failwith "CCSexp.Traverse.get_exn" | Some x -> x
end
