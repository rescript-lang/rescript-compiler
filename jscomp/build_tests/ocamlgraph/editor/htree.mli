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

module type TREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label
end ;;

module type HTREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label

  type coord = float * float

  type driver = {
    rlimit : float ;
    moveto : coord -> unit ;
    lineto : coord -> unit ;
    curveto : coord -> coord -> coord -> unit ;
    draw_label : label -> coord -> float -> unit ;
    init_edge_pass : unit -> unit ;
    init_label_pass : unit -> unit ;
    finalize : unit -> unit ;
  }

  val shrink_factor : coord -> float
  val drag_origin : coord -> coord -> coord -> coord

  val draw_linear_tree : driver -> t -> coord -> float -> unit
  val draw_curved_tree : driver -> t -> coord -> float -> unit
end ;;

module Make(T : TREE) :
  HTREE with type t = T.t and type label = T.label ;;
