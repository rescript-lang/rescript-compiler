(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Environments and auxiliary structures used during closure conversion. *)

(** Used to remember which [Variable.t] values correspond to which
    [Ident.t] values during closure conversion, and similarly for
     static exception identifiers. *)
module Env : sig
  type t

  val empty : t

  val add_var : t -> Ident.t -> Variable.t -> t
  val add_vars : t -> Ident.t list -> Variable.t list -> t

  val find_var : t -> Ident.t -> Variable.t
  val find_var_exn : t -> Ident.t -> Variable.t

  val add_mutable_var : t -> Ident.t -> Mutable_variable.t -> t
  val find_mutable_var_exn : t -> Ident.t -> Mutable_variable.t

  val add_static_exception : t -> int -> Static_exception.t -> t
  val find_static_exception : t -> int -> Static_exception.t

  val add_global : t -> int -> Symbol.t -> t
  val find_global : t -> int -> Symbol.t

  val at_toplevel : t -> bool
  val not_at_toplevel : t -> t
end

(** Used to represent information about a set of function declarations
    during closure conversion.  (The only case in which such a set may
    contain more than one declaration is when processing "let rec".) *)
module Function_decls : sig
  module Function_decl : sig
    type t

    val create
       : let_rec_ident:Ident.t option
      -> closure_bound_var:Variable.t
      -> kind:Lambda.function_kind
      -> params:Ident.t list
      -> body:Lambda.lambda
      -> attr:Lambda.function_attribute
      -> loc:Location.t
      -> t

    val let_rec_ident : t -> Ident.t
    val closure_bound_var : t -> Variable.t
    val kind : t -> Lambda.function_kind
    val params : t -> Ident.t list
    val body : t -> Lambda.lambda
    val inline : t -> Lambda.inline_attribute
    val specialise : t -> Lambda.specialise_attribute
    val is_a_functor : t -> bool
    val stub : t -> bool
    val loc : t -> Location.t

    (* Like [all_free_idents], but for just one function. *)
    val free_idents : t -> Lambda.IdentSet.t
  end

  type t

  val create : Function_decl.t list -> t
  val to_list : t -> Function_decl.t list

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t -> Lambda.IdentSet.t

  (* A map from identifiers to their corresponding [Variable.t]s whose domain
     is the set of all identifiers free in the bodies of the declarations that
     are not bound as parameters.
     It also contains the globals bindings of the provided environment. *)
  val closure_env_without_parameters : Env.t -> t -> Env.t
end
