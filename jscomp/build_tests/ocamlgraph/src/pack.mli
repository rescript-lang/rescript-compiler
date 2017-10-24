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

(* $Id: pack.mli,v 1.1 2004-02-04 11:52:02 filliatr Exp $ *)

(** {b Immediate access to the library}: provides implementation of imperative
    graphs labeled with integer as well as algorithms on such graphs.

    So if you bother functors, you can use this module. *)

(** Directed imperative graphs with edges and vertices labeled with integer. *)
module Digraph : Sig_pack.S

(** Undirected imperative graphs with edges and vertices labeled with
    integer. *)
module Graph : Sig_pack.S

