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

type lifter = Flambda.program -> Flambda.program

let rebuild_let
    (defs : (Variable.t * Flambda.named Flambda.With_free_variables.t) list)
    (body : Flambda.t) =
  let module W = Flambda.With_free_variables in
  List.fold_left (fun body (var, def) ->
      W.create_let_reusing_defining_expr var def body)
    body defs

let rec extract_lets
    (acc:(Variable.t * Flambda.named Flambda.With_free_variables.t) list)
    (let_expr:Flambda.let_expr) :
  (Variable.t * Flambda.named Flambda.With_free_variables.t) list *
  Flambda.t Flambda.With_free_variables.t =
  let module W = Flambda.With_free_variables in
  match let_expr with
  | { var = v1; defining_expr = Expr (Let let2); _ } ->
    let acc, body2 = extract_lets acc let2 in
    let acc = (v1, W.expr body2) :: acc in
    let body = W.of_body_of_let let_expr in
    extract acc body
  | { var = v; _ } ->
    let acc = (v, W.of_defining_expr_of_let let_expr) :: acc in
    let body = W.of_body_of_let let_expr in
    extract acc body

and extract acc (expr : Flambda.t Flambda.With_free_variables.t) =
  let module W = Flambda.With_free_variables in
  match W.contents expr with
  | Let let_expr ->
    extract_lets acc let_expr
  | _ ->
    acc, expr

let rec lift_lets_expr (expr:Flambda.t) ~toplevel : Flambda.t =
  let module W = Flambda.With_free_variables in
  match expr with
  | Let let_expr ->
    let defs, body = extract_lets [] let_expr in
    let rev_defs =
      List.rev_map (lift_lets_named_with_free_variables ~toplevel) defs
    in
    let body = lift_lets_expr (W.contents body) ~toplevel in
    rebuild_let (List.rev rev_defs) body
  | e ->
    Flambda_iterators.map_subexpressions
      (lift_lets_expr ~toplevel)
      (lift_lets_named ~toplevel)
      e

and lift_lets_named_with_free_variables
    ((var, named):Variable.t * Flambda.named Flambda.With_free_variables.t)
      ~toplevel : Variable.t * Flambda.named Flambda.With_free_variables.t =
  let module W = Flambda.With_free_variables in
  match W.contents named with
  | Expr e ->
    var, W.expr (W.of_expr (lift_lets_expr e ~toplevel))
  | Set_of_closures set when not toplevel ->
    var,
    W.of_named
      (Set_of_closures
         (Flambda_iterators.map_function_bodies
            ~f:(lift_lets_expr ~toplevel) set))
  | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
  | Read_symbol_field (_, _) | Project_closure _ | Move_within_set_of_closures _
  | Project_var _ | Prim _ | Set_of_closures _ ->
    var, named

and lift_lets_named _var (named:Flambda.named) ~toplevel : Flambda.named =
  let module W = Flambda.With_free_variables in
  match named with
  | Expr e ->
    Expr (lift_lets_expr e ~toplevel)
  | Set_of_closures set when not toplevel ->
    Set_of_closures
      (Flambda_iterators.map_function_bodies ~f:(lift_lets_expr ~toplevel) set)
  | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
  | Read_symbol_field (_, _) | Project_closure _ | Move_within_set_of_closures _
  | Project_var _ | Prim _ | Set_of_closures _ ->
    named

module Sort_lets = Strongly_connected_components.Make (Variable)

let rebuild_let_rec (defs:(Variable.t * Flambda.named) list) body =
  let map = Variable.Map.of_list defs in
  let graph =
    Variable.Map.map
      (fun named ->
         Variable.Set.filter (fun v -> Variable.Map.mem v map)
           (Flambda.free_variables_named named))
      map
  in
  let components =
    Sort_lets.connected_components_sorted_from_roots_to_leaf graph
  in
  Array.fold_left (fun body (component:Sort_lets.component) ->
      match component with
      | No_loop v ->
          let def = Variable.Map.find v map in
          Flambda.create_let v def body
      | Has_loop l ->
          Flambda.Let_rec
            (List.map (fun v -> v, Variable.Map.find v map) l,
             body))
    body components

let lift_let_rec program =
  Flambda_iterators.map_exprs_at_toplevel_of_program program
    ~f:(Flambda_iterators.map_expr
          (fun expr -> match expr with
             | Let_rec (defs, body) ->
                 rebuild_let_rec defs body
             | expr -> expr))

let lift_lets program =
  let program = lift_let_rec program in
  Flambda_iterators.map_exprs_at_toplevel_of_program program
    ~f:(lift_lets_expr ~toplevel:false)

let lifting_helper exprs ~evaluation_order ~create_body ~name =
  let vars, lets =
    (* [vars] corresponds elementwise to [exprs]; the order is unchanged. *)
    List.fold_right (fun (flam : Flambda.t) (vars, lets) ->
        match flam with
        | Var v ->
          (* Note that [v] is (statically) always an immutable variable. *)
          v::vars, lets
        | expr ->
          let v =
            Variable.create name ~current_compilation_unit:
                (Compilation_unit.get_current_exn ())
          in
          v::vars, (v, expr)::lets)
      exprs ([], [])
  in
  let lets =
    match evaluation_order with
    | `Right_to_left -> lets
    | `Left_to_right -> List.rev lets
  in
  List.fold_left (fun body (v, expr) ->
      Flambda.create_let v (Expr expr) body)
    (create_body vars) lets
