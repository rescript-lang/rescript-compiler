(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 OCamlPro SAS                                    *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Decipher command line arguments of the form
        <value> | <key>=<value>[,...]
    (as used for example for the specification of inlining parameters
    varying by simplification round).
*)

module Make (S : sig
  module Key : sig
    type t

    (** The textual representation of a key must not contain '=' or ','. *)
    val of_string : string -> t

    module Map : Map.S with type key = t
  end

  module Value : sig
    type t

    (** The textual representation of a value must not contain ','. *)
    val of_string : string -> t
  end
end) : sig
  type parsed

  val default : S.Value.t -> parsed

  val set_base_default : S.Value.t -> parsed -> parsed

  val add_base_override : S.Key.t -> S.Value.t -> parsed -> parsed

  val reset_base_overrides : parsed -> parsed

  val set_user_default : S.Value.t -> parsed -> parsed

  val add_user_override : S.Key.t -> S.Value.t -> parsed -> parsed

  val parse : string -> string -> parsed ref -> unit

  type parse_result =
    | Ok
    | Parse_failed of exn

  val parse_no_error : string -> parsed ref -> parse_result

  val get : key:S.Key.t -> parsed -> S.Value.t
end
