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

(* Definition of actions, basic blocks for tests *)

type result =
  | Pass of Environments.t
  | Fail of string
  | Skip of string

let string_of_reason prefix reason =
  if reason="" then prefix
  else prefix ^ " (" ^ reason ^ ")"

let string_of_result = function
  | Pass _ -> "Pass"
  | Fail reason -> string_of_reason "Fail" reason
  | Skip reason -> string_of_reason "Skip" reason

type body = out_channel -> Environments.t -> result

type t = {
  action_name : string;
  action_environment : Environments.t -> Environments.t;
  action_body : body
}

let compare a1 a2 = String.compare a1.action_name a2.action_name

let (actions : (string, t) Hashtbl.t) = Hashtbl.create 10

let register action =
  Hashtbl.add actions action.action_name action

let get_registered_actions () =
  let f _action_name action acc = action::acc in
  let unsorted_actions = Hashtbl.fold f actions [] in
  List.sort compare unsorted_actions

let lookup name =
  try Some (Hashtbl.find actions name)
  with Not_found -> None

let run log env action =
  action.action_body log env

module ActionSet = Set.Make
(struct
  type nonrec t = t
  let compare = compare
end)

let update_environment initial_env actions =
  let f act env = act.action_environment env in
  ActionSet.fold f actions initial_env
