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

type ('a, 'b) kind =
  | Initialisation of (Symbol.t * Tag.t * Flambda.t list)
  | Effect of 'b

let should_copy (named:Flambda.named) =
  match named with
  | Symbol _ | Read_symbol_field _ | Const _ -> true
  | _ -> false

type extracted =
  | Expr of Variable.t * Flambda.t
  | Exprs of Variable.t list * Flambda.t
  | Block of Variable.t * Tag.t * Variable.t list

type accumulated = {
  copied_lets : (Variable.t * Flambda.named) list;
  extracted_lets : extracted list;
  terminator : Flambda.expr;
}

let rec accumulate ~substitution ~copied_lets ~extracted_lets
      (expr : Flambda.t) =
  match expr with
  | Let { var; body = Var var'; _ } | Let_rec ([var, _], Var var')
    when Variable.equal var var' ->
    { copied_lets; extracted_lets;
      terminator = Flambda_utils.toplevel_substitution substitution expr;
    }
  (* If the pattern is what lifting let_rec generates, prevent it from being
     lifted again. *)
  | Let_rec (defs,
             Let { var; body = Var var';
                   defining_expr = Prim (Pmakeblock _, fields, _); })
    when
      Variable.equal var var'
      && List.for_all (fun field ->
          List.exists (fun (def_var, _) -> Variable.equal def_var field) defs)
      fields ->
    { copied_lets; extracted_lets;
      terminator = Flambda_utils.toplevel_substitution substitution expr;
    }
  | Let { var; defining_expr = Expr (Var alias); body; _ }
  | Let_rec ([var, Expr (Var alias)], body) ->
    let alias =
      match Variable.Map.find alias substitution with
      | exception Not_found -> alias
      | original_alias -> original_alias
    in
    accumulate
      ~substitution:(Variable.Map.add var alias substitution)
      ~copied_lets
      ~extracted_lets
      body
  | Let { var; defining_expr = named; body; _ }
  | Let_rec ([var, named], body)
    when should_copy named ->
      accumulate body
        ~substitution
        ~copied_lets:((var, named)::copied_lets)
        ~extracted_lets
  | Let { var; defining_expr = named; body; _ } ->
    let extracted =
      let renamed = Variable.rename var in
      match named with
      | Prim (Pmakeblock (tag, _, Asttypes.Immutable, _value_kind), args, _dbg) ->
        let tag = Tag.create_exn tag in
        let args =
          List.map (fun v ->
              try Variable.Map.find v substitution
              with Not_found -> v)
            args
        in
        Block (var, tag, args)
      | named ->
        let expr =
          Flambda_utils.toplevel_substitution substitution
            (Flambda.create_let renamed named (Var renamed))
        in
        Expr (var, expr)
    in
    accumulate body
      ~substitution
      ~copied_lets
      ~extracted_lets:(extracted::extracted_lets)
  | Let_rec ([var, named], body) ->
    let renamed = Variable.rename var in
    let def_substitution = Variable.Map.add var renamed substitution in
    let expr =
      Flambda_utils.toplevel_substitution def_substitution
        (Let_rec ([renamed, named], Var renamed))
    in
    let extracted = Expr (var, expr) in
    accumulate body
      ~substitution
      ~copied_lets
      ~extracted_lets:(extracted::extracted_lets)
  | Let_rec (defs, body) ->
    let renamed_defs, def_substitution =
      List.fold_right (fun (var, def) (acc, substitution) ->
          let new_var = Variable.rename var in
          (new_var, def) :: acc,
          Variable.Map.add var new_var substitution)
        defs ([], substitution)
    in
    let extracted =
      let expr =
        Flambda_utils.toplevel_substitution def_substitution
          (Let_rec (renamed_defs,
                    Flambda_utils.name_expr ~name:"lifted_let_rec_block"
                      (Prim (Pmakeblock (0, Lambda.default_tag_info, Immutable, None),
                             List.map fst renamed_defs,
                             Debuginfo.none))))
      in
      Exprs (List.map fst defs, expr)
    in
    accumulate body
      ~substitution
      ~copied_lets
      ~extracted_lets:(extracted::extracted_lets)
  | _ ->
  { copied_lets;
    extracted_lets;
    terminator = Flambda_utils.toplevel_substitution substitution expr;
  }

let rebuild_expr
      ~(extracted_definitions : (Symbol.t * int list) Variable.Map.t)
      ~(copied_definitions : Flambda.named Variable.Map.t)
      ~(substitute : bool)
      (expr : Flambda.t) =
  let expr_with_read_symbols =
    Flambda_utils.substitute_read_symbol_field_for_variables
      extracted_definitions expr
  in
  let free_variables = Flambda.free_variables expr_with_read_symbols in
  let substitution =
    if substitute then
      Variable.Map.of_set (fun x -> Variable.rename x) free_variables
    else
      Variable.Map.of_set (fun x -> x) free_variables
  in
  let expr_with_read_symbols =
    Flambda_utils.toplevel_substitution substitution
      expr_with_read_symbols
  in
  Variable.Map.fold (fun var declaration body ->
      let definition = Variable.Map.find var copied_definitions in
      Flambda.create_let declaration definition body)
    substitution expr_with_read_symbols

let rebuild (used_variables:Variable.Set.t) (accumulated:accumulated) =
  let copied_definitions = Variable.Map.of_list accumulated.copied_lets in
  let accumulated_extracted_lets =
    List.map (fun decl ->
        match decl with
        | Block (var, _, _) | Expr (var, _) ->
          Flambda_utils.make_variable_symbol var, decl
        | Exprs (vars, _) ->
          Flambda_utils.make_variables_symbol vars, decl)
      accumulated.extracted_lets
  in
  let extracted_definitions =
    (* Blocks are lifted to direct top-level Initialize_block:
         accessing the value be done directly through the symbol.
       Other let bound variables are initialized inside a size
       one static block:
         accessing the value is done directly through the field 0
         of the symbol.
       let rec of size more than one is represented as a block of
       all the bound variables allocated inside a size one static
       block:
         accessing the value is done directly through the right
         field of the field 0 of the symbol. *)
    List.fold_left (fun map (symbol, decl) ->
        match decl with
        | Block (var, _tag, _fields) ->
          Variable.Map.add var (symbol, []) map
        | Expr (var, _expr) ->
          Variable.Map.add var (symbol, [0]) map
        | Exprs (vars, _expr) ->
          let map, _ =
            List.fold_left (fun (map, field) var ->
                Variable.Map.add var (symbol, [field; 0]) map,
                field + 1)
              (map, 0) vars
          in
          map)
      Variable.Map.empty accumulated_extracted_lets
  in
  let extracted =
    List.map (fun (symbol, decl) ->
        match decl with
        | Expr (var, decl) ->
          let expr =
            rebuild_expr ~extracted_definitions ~copied_definitions
              ~substitute:true decl
          in
          if Variable.Set.mem var used_variables then
            Initialisation
              (symbol,
               Tag.create_exn 0,
               [expr])
          else
            Effect expr
        | Exprs (_vars, decl) ->
          let expr =
            rebuild_expr ~extracted_definitions ~copied_definitions
              ~substitute:true decl
          in
          Initialisation (symbol, Tag.create_exn 0, [expr])
        | Block (_var, tag, fields) ->
          let fields =
            List.map (fun var ->
                rebuild_expr ~extracted_definitions ~copied_definitions
                  ~substitute:true (Var var))
              fields
          in
          Initialisation (symbol, tag, fields))
      accumulated_extracted_lets
  in
  let terminator =
    (* We don't need to substitute the variables in the terminator, we
       suppose that we did for every other occurrence.  Avoiding this
       substitution allows this transformation to be idempotent. *)
    rebuild_expr ~extracted_definitions ~copied_definitions
      ~substitute:false accumulated.terminator
  in
  List.rev extracted, terminator

let introduce_symbols expr =
  let accumulated =
    accumulate expr
      ~substitution:Variable.Map.empty
      ~copied_lets:[] ~extracted_lets:[]
  in
  let used_variables = Flambda.used_variables expr in
  let extracted, terminator = rebuild used_variables accumulated in
  extracted, terminator

let add_extracted introduced program =
  List.fold_right (fun extracted program ->
      match extracted with
      | Initialisation (symbol, tag, def) ->
        Flambda.Initialize_symbol (symbol, tag, def, program)
      | Effect effect ->
        Flambda.Effect (effect, program))
    introduced program

let rec split_program (program : Flambda.program_body) : Flambda.program_body =
  match program with
  | End s -> End s
  | Let_symbol (s, def, program) ->
    Let_symbol (s, def, split_program program)
  | Let_rec_symbol (defs, program) ->
    Let_rec_symbol (defs, split_program program)
  | Effect (expr, program) ->
    let program = split_program program in
    let introduced, expr = introduce_symbols expr in
    add_extracted introduced (Flambda.Effect (expr, program))
  | Initialize_symbol (symbol, tag, ((_::_::_) as fields), program) ->
    (* CR-someday pchambart: currently the only initialize_symbol with more
       than 1 field is the module block. This could evolve, in that case
       this pattern should be handled properly. *)
    Initialize_symbol (symbol, tag, fields, split_program program)
  | Initialize_symbol (sym, tag, [], program) ->
    Let_symbol (sym, Block (tag, []), split_program program)
  | Initialize_symbol (symbol, tag, [field], program) ->
    let program = split_program program in
    let introduced, field = introduce_symbols field in
    add_extracted introduced
      (Flambda.Initialize_symbol (symbol, tag, [field], program))

let lift ~backend:_ (program : Flambda.program) =
  { program with
    program_body = split_program program.program_body;
  }
