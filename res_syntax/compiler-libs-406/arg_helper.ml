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

let fatal err =
  prerr_endline err;
  exit 2

module Make (S : sig
  module Key : sig
    type t
    val of_string : string -> t
    module Map : Map.S with type key = t
  end

  module Value : sig
    type t
    val of_string : string -> t
  end
end) = struct
  type parsed = {
    base_default : S.Value.t;
    base_override : S.Value.t S.Key.Map.t;
    user_default : S.Value.t option;
    user_override : S.Value.t S.Key.Map.t;
  }

  let default v =
    { base_default = v;
      base_override = S.Key.Map.empty;
      user_default = None;
      user_override = S.Key.Map.empty; }

  let set_base_default value t =
    { t with base_default = value }

  let add_base_override key value t =
    { t with base_override = S.Key.Map.add key value t.base_override }

  let reset_base_overrides t =
    { t with base_override = S.Key.Map.empty }

  let set_user_default value t =
    { t with user_default = Some value }

  let add_user_override key value t =
    { t with user_override = S.Key.Map.add key value t.user_override }

  exception Parse_failure of exn

  let parse_exn str ~update =
    (* Is the removal of empty chunks really relevant here? *)
    (* (It has been added to mimic the old Misc.String.split.) *)
    let values = String.split_on_char ',' str |> List.filter ((<>) "") in
    let parsed =
      List.fold_left (fun acc value ->
          match String.index value '=' with
          | exception Not_found ->
            begin match S.Value.of_string value with
            | value -> set_user_default value acc
            | exception exn -> raise (Parse_failure exn)
            end
          | equals ->
            let key_value_pair = value in
            let length = String.length key_value_pair in
            assert (equals >= 0 && equals < length);
            if equals = 0 then begin
              raise (Parse_failure (
                Failure "Missing key in argument specification"))
            end;
            let key =
              let key = String.sub key_value_pair 0 equals in
              try S.Key.of_string key
              with exn -> raise (Parse_failure exn)
            in
            let value =
              let value =
                String.sub key_value_pair (equals + 1) (length - equals - 1)
              in
              try S.Value.of_string value
              with exn -> raise (Parse_failure exn)
            in
            add_user_override key value acc)
        !update
        values
    in
    update := parsed

  let parse str help_text update =
    match parse_exn str ~update with
    | () -> ()
    | exception (Parse_failure exn) ->
      fatal (Printf.sprintf "%s: %s" (Printexc.to_string exn) help_text)

  type parse_result =
    | Ok
    | Parse_failed of exn

  let parse_no_error str update =
    match parse_exn str ~update with
    | () -> Ok
    | exception (Parse_failure exn) -> Parse_failed exn

  let get ~key parsed =
    match S.Key.Map.find key parsed.user_override with
    | value -> value
    | exception Not_found ->
      match parsed.user_default with
      | Some value -> value
      | None ->
        match S.Key.Map.find key parsed.base_override with
        | value -> value
        | exception Not_found -> parsed.base_default

end
