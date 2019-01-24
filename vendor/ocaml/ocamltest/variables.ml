(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of environment variabless *)

type t = {
  variable_name : string;
  variable_description : string
}

let compare v1 v2 = String.compare v1.variable_name v2.variable_name

exception Empty_variable_name

exception Variable_already_registered

let make (name, description) =
  if name="" then raise Empty_variable_name else {
    variable_name = name;
    variable_description = description
  }

let name_of_variable v = v.variable_name

let description_of_variable v = v.variable_description

let (variables : (string, t) Hashtbl.t) = Hashtbl.create 10

let register_variable variable =
  if Hashtbl.mem variables variable.variable_name
  then raise Variable_already_registered
  else Hashtbl.add variables variable.variable_name variable

let find_variable variable_name =
  try Some (Hashtbl.find variables variable_name)
  with Not_found -> None
