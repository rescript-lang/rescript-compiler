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

(** Representation and manipulation of method / function / class parameters. *)

let print_DEBUG s = print_string s ; print_newline ()

(** Types *)

(** Representation of a simple parameter name *)
type simple_name = {
    sn_name : string ;
    sn_type : Types.type_expr ;
    mutable sn_text : Odoc_types.text option ;
  }

(** Representation of parameter names. We need it to represent parameter names in tuples.
   The value [Tuple ([], t)] stands for an anonymous parameter.*)
type param_info =
  | Simple_name of simple_name
  | Tuple of param_info list * Types.type_expr

(** A parameter is just a param_info.*)
type parameter = param_info

(** Functions *)

(** acces to the name as a string. For tuples, parenthesis and commas are added. *)
let complete_name p =
  let rec iter pi =
    match pi with
      Simple_name sn ->
        sn.sn_name
    | Tuple ([], _) -> (* anonymous parameter *)
        "??"
    | Tuple (pi_list, _) ->
        "("^(String.concat "," (List.map iter pi_list))^")"
  in
  iter p

(** access to the complete type *)
let typ pi =
  match pi with
    Simple_name sn -> sn.sn_type
  | Tuple (_, typ) -> typ

(** Update the text of a parameter using a function returning
   the optional text associated to a parameter name.*)
let update_parameter_text f p =
  let rec iter pi =
    match pi with
      Simple_name sn ->
        sn.sn_text <- f sn.sn_name
    | Tuple (l, _) ->
        List.iter iter l
  in
  iter p

(** access to the description of a specific name.
   @raise Not_found if no description is associated to the given name. *)
let desc_by_name pi name =
  let rec iter acc pi =
    match pi with
      Simple_name sn ->
        (sn.sn_name, sn.sn_text) :: acc
    | Tuple (pi_list, _) ->
        List.fold_left iter acc pi_list
      in
  let l = iter [] pi in
  List.assoc name l


(** acces to the list of names ; only one for a simple parameter, or
   a list for tuples. *)
let names pi =
  let rec iter acc pi =
    match pi with
      Simple_name sn ->
        sn.sn_name :: acc
    | Tuple (pi_list, _) ->
            List.fold_left iter acc pi_list
  in
  iter [] pi

(** access to the type of a specific name.
   @raise Not_found if no type is associated to the given name. *)
let type_by_name pi name =
  let rec iter acc pi =
    match pi with
      Simple_name sn ->
        (sn.sn_name, sn.sn_type) :: acc
    | Tuple (pi_list, _) ->
        List.fold_left iter acc pi_list
      in
  let l = iter [] pi in
  List.assoc name l

(** access to the optional description of a parameter name from an optional info structure.*)
let desc_from_info_opt info_opt s =
  print_DEBUG "desc_from_info_opt";
  match info_opt with
    None -> None
  | Some i ->
      match s with
        "" -> None
      | _ ->
          try
            Some (List.assoc s i.Odoc_types.i_params)
          with
            Not_found ->
              print_DEBUG ("desc_from_info_opt "^s^" not found in\n");
              List.iter (fun (s, _) -> print_DEBUG s) i.Odoc_types.i_params;
              None
