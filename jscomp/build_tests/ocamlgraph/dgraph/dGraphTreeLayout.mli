(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009-2010                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

open Graph

type cluster = string

module Make
    (Tree: Graphviz.GraphWithDotAttrs)
    (TreeManipulation: sig val is_ghost_node: Tree.V.t -> bool end):
sig

  val from_tree:
    [> `widget] Gtk.obj -> Tree.t -> Tree.V.t -> XDot.Make(Tree).graph_layout

end

module MakeFromDotModel
    (Tree : Sig.G with type V.label = DGraphModel.DotG.V.t
                   and type E.label = unit)
    (TreeManipulation: sig val is_ghost_node: Tree.V.t -> bool end):
sig

  module Tree: Graphviz.GraphWithDotAttrs with module V = Tree.V
                                           and module E = Tree.E
                                           and type t = Tree.t

  val from_model:
    Tree.t -> Tree.V.t -> DGraphModel.dotg_model -> XDot.Make(Tree).graph_layout

end
