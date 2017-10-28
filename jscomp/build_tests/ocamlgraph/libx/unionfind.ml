(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

module type HashedOrderedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val init : elt list -> t
  val find : elt -> t -> elt
  val union : elt -> elt -> t -> unit
end

module Make(X:HashedOrderedType) = struct

  type elt = X.t

  module H = Hashtbl.Make(X)

  type cell = {
    mutable c : int;
    data : elt;
    mutable father : cell
  }

  type t = cell H.t (* a forest *)

  let init l =
    let h = H.create 997 in
    List.iter
      (fun x ->
         let rec cell = { c = 0; data = x; father = cell } in
	 H.add h x cell)
      l;
    h

  let rec find_aux cell =
    if cell.father == cell then
      cell
    else
      let r = find_aux cell.father in
      cell.father <- r;
      r

  let find x h = (find_aux (H.find h x)).data

  let union x y h =
    let rx = find_aux (H.find h x) in
    let ry = find_aux (H.find h y) in
    if rx != ry then begin
      if rx.c > ry.c then
        ry.father <- rx
      else if rx.c < ry.c then
        rx.father <- ry
      else begin
        rx.c <- rx.c + 1;
        ry.father <- rx
      end
    end
end

(*** test ***)
(***

module M = Make (struct
        type t = int let
        hash = Hashtbl.hash
        let compare = compare
        let equal = (=)
    end)

open Printf

let saisir s  =
        printf "%s = " s; flush stdout;
        let x = read_int () in
        x

let h = M.init [0;1;2;3;4;5;6;7;8;9]
let () = if not !Sys.interactive then
    while true do
        printf "1) find\n2) union\n";
        match read_int () with
            1 -> begin
                let x = saisir "x" in
                printf "%d\n" (M.find x h)
            end
          | 2 -> begin
                let x, y = saisir "x", saisir "y" in
                M.union x y h
            end
          | _ -> ()
    done

***)
