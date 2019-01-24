(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** *)

class type doc_generator =
  object method generate : Odoc_module.t_module list -> unit end;;

module type Base = sig
    class generator : doc_generator
  end;;

module Base_generator : Base = struct
  class generator : doc_generator = object method generate _ = () end
  end;;

module type Base_functor = functor (G: Base) -> Base
module type Html_functor = functor (G: Odoc_html.Html_generator) -> Odoc_html.Html_generator
module type Latex_functor = functor (G: Odoc_latex.Latex_generator) -> Odoc_latex.Latex_generator
module type Texi_functor = functor (G: Odoc_texi.Texi_generator) -> Odoc_texi.Texi_generator
module type Man_functor = functor (G: Odoc_man.Man_generator) -> Odoc_man.Man_generator
module type Dot_functor = functor (G: Odoc_dot.Dot_generator) -> Odoc_dot.Dot_generator

type generator =
  | Html of (module Odoc_html.Html_generator)
  | Latex of (module Odoc_latex.Latex_generator)
  | Texi of (module Odoc_texi.Texi_generator)
  | Man of (module Odoc_man.Man_generator)
  | Dot of (module Odoc_dot.Dot_generator)
  | Base of (module Base)
;;

let get_minimal_generator = function
  Html m ->
    let module M = (val m : Odoc_html.Html_generator) in
    (new M.html :> doc_generator)
| Latex m ->
    let module M = (val m : Odoc_latex.Latex_generator) in
    (new M.latex :> doc_generator)
| Man m ->
    let module M = (val m : Odoc_man.Man_generator) in
    (new M.man :> doc_generator)
| Texi m ->
    let module M = (val m : Odoc_texi.Texi_generator) in
    (new M.texi :> doc_generator)
| Dot m ->
    let module M = (val m : Odoc_dot.Dot_generator) in
    (new M.dot :> doc_generator)
| Base m ->
    let module M = (val m : Base) in
    new M.generator
    ;;
