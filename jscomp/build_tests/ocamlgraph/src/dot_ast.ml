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

(* $Id:$ *)

(** AST for DOT file format. *)

type id =
  | Ident of string
  | Number of string
  | String of string
  | Html of string

type attr = (id * id option) list

type compass_pt = N | Ne | E | Se | S | Sw | W | Nw

type port =
  | PortId of id * compass_pt option
  | PortC of compass_pt

type node_id = id * port option

type subgraph =
  | SubgraphId of id
  | SubgraphDef of id option * stmt list

and node =
  | NodeId of node_id
  | NodeSub of subgraph

and stmt =
  | Node_stmt of node_id * attr list
  | Edge_stmt of node * node list * attr list
  | Attr_graph of attr list
  | Attr_node of attr list
  | Attr_edge of attr list
  | Equal of id * id
  | Subgraph of subgraph

type file =
  { strict : bool;
    digraph : bool;
    id : id option;
    stmts : stmt list }
