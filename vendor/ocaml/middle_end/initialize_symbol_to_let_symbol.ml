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

let constant_field (expr:Flambda.t)
  : Flambda.constant_defining_value_block_field option =
  match expr with
  | Let { var; defining_expr = Const c; body = Var var' ; _ } ->
    assert(Variable.equal var var');
    (* This must be true since var is the only variable in scope *)
    Some (Flambda.Const c)
  | Let { var; defining_expr = Symbol s; body = Var var' ; _ } ->
    assert(Variable.equal var var');
    Some (Flambda.Symbol s)
  | _ ->
    None

let rec loop (program : Flambda.program_body) : Flambda.program_body =
  match program with
  | Initialize_symbol (symbol, tag, fields, program) ->
    let constant_fields = List.map constant_field fields in
    begin
      match Misc.Stdlib.List.some_if_all_elements_are_some constant_fields
    with
    | None ->
      Initialize_symbol (symbol, tag, fields, loop program)
    | Some fields ->
      Let_symbol (symbol, Block (tag, fields), loop program)
    end
  | Let_symbol (symbol, const, program) ->
    Let_symbol (symbol, const, loop program)
  | Let_rec_symbol (defs, program) ->
    Let_rec_symbol (defs, loop program)
  | Effect (expr, program) ->
    Effect (expr, loop program)
  | End symbol ->
    End symbol

let run (program : Flambda.program) =
  { program with
    program_body = loop program.program_body;
  }
