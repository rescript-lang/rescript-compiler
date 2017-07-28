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

(** Representation and manipulation of a type, but not class nor module type.*)

module Name = Odoc_name

type private_flag = Asttypes.private_flag =
    Private | Public

(** Description of a variant type constructor. *)
type variant_constructor = {
    vc_name : string ;
    vc_args : Types.type_expr list ; (** arguments of the constructor *)
    vc_ret : Types.type_expr option ;
    mutable vc_text : Odoc_types.info option ; (** optional user description *)
  }

(** Description of a record type field. *)
type record_field = {
    rf_name : string ;
    rf_mutable : bool ; (** true if mutable *)
    rf_type : Types.type_expr ;
    mutable rf_text : Odoc_types.info option ; (** optional user description *)
  }

(** The various kinds of type. *)
type type_kind =
    Type_abstract
  | Type_variant of variant_constructor list
                   (** constructors *)
  | Type_record of record_field list
                   (** fields *)
  | Type_open

type object_field = {
  of_name : string ;
  of_type : Types.type_expr ;
  mutable of_text : Odoc_types.info option ; (** optional user description *)
}

type type_manifest =
  | Other of Types.type_expr (** Type manifest directly taken from Typedtre. *)
  | Object_type of object_field list

(** Representation of a type. *)
type t_type = {
    ty_name : Name.t ;
    mutable ty_info : Odoc_types.info option ; (** optional user information *)
    ty_parameters : (Types.type_expr * bool * bool) list ;
                    (** type parameters: (type, covariant, contravariant) *)
    ty_kind : type_kind ;
    ty_private : private_flag;
    ty_manifest : type_manifest option;
    mutable ty_loc : Odoc_types.location ;
    mutable ty_code : string option;
  }
