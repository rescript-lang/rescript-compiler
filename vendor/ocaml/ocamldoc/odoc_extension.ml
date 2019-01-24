(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation and manipulation of type extensions. *)

module Name = Odoc_name

type private_flag = Asttypes.private_flag =
    Private | Public

type extension_alias = {
    xa_name : Name.t ;
    mutable xa_xt : t_extension_constructor option ;
  }

and t_extension_constructor = {
    xt_name : Name.t ;
    xt_args: Odoc_type.constructor_args;
    xt_ret: Types.type_expr option ; (** the optional return type of the extension *)
    xt_type_extension: t_type_extension ; (** the type extension containing this constructor *)
    xt_alias: extension_alias option ;
    mutable xt_loc: Odoc_types.location ;
    mutable xt_text: Odoc_types.info option ; (** optional user description *)
  }

and t_type_extension = {
    mutable te_info : Odoc_types.info option ; (** optional user information *)
    te_type_name : Name.t;
    te_type_parameters : Types.type_expr list;
    te_private : private_flag;
    mutable te_constructors: t_extension_constructor list;
    mutable te_loc : Odoc_types.location ;
    mutable te_code : string option ;
  }

let extension_constructors te = te.te_constructors
