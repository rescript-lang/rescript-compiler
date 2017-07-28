(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Analysis of the command line arguments. *)

(** The current module defining the generator to use. *)
val current_generator : Odoc_gen.generator option ref

(** To set the documentation generator. *)
val set_generator : Odoc_gen.generator -> unit

(** Extend current HTML generator.
  @raise Failure if another kind of generator is already set.*)
val extend_html_generator : (module Odoc_gen.Html_functor) -> unit

(** Extend current LaTeX generator.
  @raise Failure if another kind of generator is already set.*)
val extend_latex_generator : (module Odoc_gen.Latex_functor) -> unit

(** Extend current Texi generator.
  @raise Failure if another kind of generator is already set.*)
val extend_texi_generator : (module Odoc_gen.Texi_functor) -> unit

(** Extend current man generator.
  @raise Failure if another kind of generator is already set.*)
val extend_man_generator : (module Odoc_gen.Man_functor) -> unit

(** Extend current dot generator.
  @raise Failure if another kind of generator is already set.*)
val extend_dot_generator : (module Odoc_gen.Dot_functor) -> unit

(** Extend current base generator.
  @raise Failure if another kind of generator is already set.*)
val extend_base_generator : (module Odoc_gen.Base_functor) -> unit

(** Add an option specification. *)
val add_option : string * Arg.spec * string -> unit

(** Parse the args.
   [byte] indicate if we are in bytecode mode (default is [true]).*)
val parse : unit -> unit
