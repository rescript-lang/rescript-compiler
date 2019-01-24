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

let rename_var var =
  Mutable_variable.create
    (Variable.unique_name var)
  (* Variable.rename var *)
  (*   ~current_compilation_unit:(Compilation_unit.get_current_exn ()) *)

let variables_not_used_as_local_reference (tree:Flambda.t) =
  let set = ref Variable.Set.empty in
  let rec loop_named (flam : Flambda.named) =
    match flam with
    (* Directly used block: does not prevent use as a variable *)
    | Prim(Pfield _, [_], _)
    | Prim(Poffsetref _, [_], _) -> ()
    | Prim(Psetfield _, [_block; v], _) ->
      (* block is not prevented to be used as a local reference, but v is *)
      set := Variable.Set.add v !set
    | Prim(_, _, _)
    | Symbol _ |Const _ | Allocated_const _ | Read_mutable _
    | Read_symbol_field _ | Project_closure _
    | Move_within_set_of_closures _ | Project_var _ ->
      set := Variable.Set.union !set (Flambda.free_variables_named flam)
    | Set_of_closures set_of_closures ->
      set := Variable.Set.union !set (Flambda.free_variables_named flam);
      Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
          loop function_decl.body)
        set_of_closures.function_decls.funs
    | Expr e ->
      loop e
  and loop (flam : Flambda.t) =
    match flam with
    | Let { defining_expr; body; _ } ->
      loop_named defining_expr;
      loop body
    | Let_rec (defs, body) ->
      List.iter (fun (_var, named) -> loop_named named) defs;
      loop body
    | Var v ->
      set := Variable.Set.add v !set
    | Let_mutable { initial_value = v; body } ->
      set := Variable.Set.add v !set;
      loop body
    | If_then_else (cond, ifso, ifnot) ->
      set := Variable.Set.add cond !set;
      loop ifso;
      loop ifnot
    | Switch (cond, { consts; blocks; failaction }) ->
      set := Variable.Set.add cond !set;
      List.iter (fun (_, branch) -> loop branch) consts;
      List.iter (fun (_, branch) -> loop branch) blocks;
      Misc.may loop failaction
    | String_switch (cond, branches, default) ->
      set := Variable.Set.add cond !set;
      List.iter (fun (_, branch) -> loop branch) branches;
      Misc.may loop default
    | Static_catch (_, _, body, handler) ->
      loop body;
      loop handler
    | Try_with (body, _, handler) ->
      loop body;
      loop handler
    | While (cond, body) ->
      loop cond;
      loop body
    | For { bound_var = _; from_value; to_value; direction = _; body; } ->
      set := Variable.Set.add from_value !set;
      set := Variable.Set.add to_value !set;
      loop body
    | Static_raise (_, args) ->
      set := Variable.Set.union (Variable.Set.of_list args) !set
    | Proved_unreachable | Apply _ | Send _ | Assign _ ->
      set := Variable.Set.union !set (Flambda.free_variables flam)
  in
  loop tree;
  !set

let variables_containing_ref (flam:Flambda.t) =
  let map = ref Variable.Map.empty in
  let aux (flam : Flambda.t) =
    match flam with
    | Let { var;
            defining_expr = Prim(Pmakeblock(0, _, Asttypes.Mutable, _), l, _);
          } ->
      map := Variable.Map.add var (List.length l) !map
    | _ -> ()
  in
  Flambda_iterators.iter aux (fun _ -> ()) flam;
  !map

let eliminate_ref_of_expr flam =
  let variables_not_used_as_local_reference =
    variables_not_used_as_local_reference flam
  in
  let convertible_variables =
    Variable.Map.filter
      (fun v _ ->
        not (Variable.Set.mem v variables_not_used_as_local_reference))
      (variables_containing_ref flam)
  in
  if Variable.Map.cardinal convertible_variables = 0 then flam
  else
    let convertible_variables =
      Variable.Map.mapi (fun v size ->
          Array.init size (fun _ -> rename_var v))
        convertible_variables
    in
    let convertible_variable v = Variable.Map.mem v convertible_variables in
    let get_variable v field =
      let arr = try Variable.Map.find v convertible_variables
        with Not_found -> assert false in
      if Array.length arr <= field
      then None (* This case could apply when inlining code containing GADTS *)
      else Some (arr.(field), Array.length arr)
    in
    let aux (flam : Flambda.t) : Flambda.t =
      match flam with
      | Let { var;
              defining_expr = Prim(Pmakeblock(0, _, Asttypes.Mutable, shape), l,_);
              body }
        when convertible_variable var ->
        let shape = match shape with
          | None -> List.map (fun _ -> Lambda.Pgenval) l
          | Some shape -> shape
        in
        let _, expr =
          List.fold_left2 (fun (field,body) init kind ->
              match get_variable var field with
              | None -> assert false
              | Some (field_var, _) ->
                field+1,
                (Let_mutable { var = field_var;
                               initial_value = init;
                               body;
                               contents_kind = kind } : Flambda.t))
            (0,body) l shape in
        expr
      | Let _ | Let_mutable _
      | Assign _ | Var _ | Apply _
      | Let_rec _ | Switch _ | String_switch _
      | Static_raise _ | Static_catch _
      | Try_with _ | If_then_else _
      | While _ | For _ | Send _ | Proved_unreachable ->
        flam
    and aux_named (named : Flambda.named) : Flambda.named =
      match named with
      | Prim(Pfield (field,_), [v], _)
        when convertible_variable v ->
        (match get_variable v field with
         | None -> Expr Proved_unreachable
         | Some (var,_) -> Read_mutable var)
      | Prim(Poffsetref delta, [v], dbg)
        when convertible_variable v ->
        (match get_variable v 0 with
         | None -> Expr Proved_unreachable
         | Some (var,size) ->
           if size = 1
           then begin
             let mut = Variable.create "read_mutable" in
             let new_value = Variable.create "offseted" in
             let expr =
               Flambda.create_let mut (Read_mutable var)
                 (Flambda.create_let new_value
                    (Prim(Poffsetint delta, [mut], dbg))
                    (Assign { being_assigned = var; new_value }))
             in
             Expr expr
           end
           else
             Expr Proved_unreachable)
      | Prim(Psetfield (field, _, _,_), [v; new_value], _)
        when convertible_variable v ->
        (match get_variable v field with
         | None -> Expr Proved_unreachable
         | Some (being_assigned,_) ->
           Expr (Assign { being_assigned; new_value }))
      | Prim _ | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field _ | Set_of_closures _ | Project_closure _
      | Move_within_set_of_closures _ | Project_var _ | Expr _ ->
        named
    in
    Flambda_iterators.map aux aux_named flam

let eliminate_ref (program:Flambda.program) =
  Flambda_iterators.map_exprs_at_toplevel_of_program program
    ~f:eliminate_ref_of_expr
