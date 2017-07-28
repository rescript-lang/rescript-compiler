(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Gallium, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2010 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The types of generators. *)

(** The minimal class type of documentation generators. *)
class type doc_generator =
  object method generate : Odoc_module.t_module list -> unit end;;

(** The module type of minimal generators. *)
module type Base = sig
    class generator : doc_generator
  end;;

module Base_generator : Base

module type Base_functor = functor (P: Base) -> Base
module type Html_functor = functor (G: Odoc_html.Html_generator) -> Odoc_html.Html_generator
module type Latex_functor = functor (G: Odoc_latex.Latex_generator) -> Odoc_latex.Latex_generator
module type Texi_functor = functor (G: Odoc_texi.Texi_generator) -> Odoc_texi.Texi_generator
module type Man_functor = functor (G: Odoc_man.Man_generator) -> Odoc_man.Man_generator
module type Dot_functor = functor (G: Odoc_dot.Dot_generator) -> Odoc_dot.Dot_generator

(** Various ways to create a generator. *)
type generator =
  | Html of (module Odoc_html.Html_generator)
  | Latex of (module Odoc_latex.Latex_generator)
  | Texi of (module Odoc_texi.Texi_generator)
  | Man of (module Odoc_man.Man_generator)
  | Dot of (module Odoc_dot.Dot_generator)
  | Base of (module Base)
;;

val get_minimal_generator : generator -> doc_generator
