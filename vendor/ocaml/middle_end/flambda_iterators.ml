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

let apply_on_subexpressions f f_named (flam : Flambda.t) =
  match flam with
  | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable
  | Static_raise _ -> ()
  | Let { defining_expr; body; _ } ->
    f_named defining_expr;
    f body
  | Let_mutable { body; _ } ->
    f body
  | Let_rec (defs, body) ->
    List.iter (fun (_,l) -> f_named l) defs;
    f body
  | Switch (_, sw) ->
    List.iter (fun (_,l) -> f l) sw.consts;
    List.iter (fun (_,l) -> f l) sw.blocks;
    Misc.may f sw.failaction
  | String_switch (_, sw, def) ->
    List.iter (fun (_,l) -> f l) sw;
    Misc.may f def
  | Static_catch (_,_,f1,f2) ->
    f f1; f f2;
  | Try_with (f1,_,f2) ->
    f f1; f f2
  | If_then_else (_,f1, f2) ->
    f f1;f f2
  | While (f1,f2) ->
    f f1; f f2
  | For { body; _ } -> f body

let rec list_map_sharing f l =
  match l with
  | [] -> l
  | h :: t ->
    let new_t = list_map_sharing f t in
    let new_h = f h in
    if h == new_h && t == new_t then
      l
    else
      new_h :: new_t

let may_map_sharing f v =
  match v with
  | None -> v
  | Some s ->
    let new_s = f s in
    if s == new_s then
      v
    else
      Some new_s

let map_snd_sharing f ((a, b) as cpl) =
  let new_b = f a b in
  if b == new_b then
    cpl
  else
    (a, new_b)

let map_subexpressions f f_named (tree:Flambda.t) : Flambda.t =
  match tree with
  | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable
  | Static_raise _ -> tree
  | Let { var; defining_expr; body; _ } ->
    let new_named = f_named var defining_expr in
    let new_body = f body in
    if new_named == defining_expr && new_body == body then
      tree
    else
      Flambda.create_let var new_named new_body
  | Let_rec (defs, body) ->
    let new_defs =
      list_map_sharing (map_snd_sharing f_named) defs
    in
    let new_body = f body in
    if new_defs == defs && new_body == body then
      tree
    else
      Let_rec (new_defs, new_body)
  | Let_mutable mutable_let ->
    let new_body = f mutable_let.body in
    if new_body == mutable_let.body then
      tree
    else
      Let_mutable { mutable_let with body = new_body }
  | Switch (arg, sw) ->
    let aux = map_snd_sharing (fun _ v -> f v) in
    let new_consts = list_map_sharing aux sw.consts in
    let new_blocks = list_map_sharing aux sw.blocks in
    let new_failaction = may_map_sharing f sw.failaction in
    if sw.failaction == new_failaction &&
       new_consts == sw.consts &&
       new_blocks == sw.blocks then
      tree
    else
      let sw =
        { sw with
          failaction = new_failaction;
          consts = new_consts;
          blocks = new_blocks;
        }
      in
      Switch (arg, sw)
  | String_switch (arg, sw, def) ->
    let new_sw = list_map_sharing (map_snd_sharing (fun _ v -> f v)) sw in
    let new_def = may_map_sharing f def in
    if sw == new_sw && def == new_def then
      tree
    else
      String_switch(arg, new_sw, new_def)
  | Static_catch (i, vars, body, handler) ->
    let new_body = f body in
    let new_handler = f handler in
    if new_body == body && new_handler == handler then
      tree
    else
      Static_catch (i, vars, new_body, new_handler)
  | Try_with(body, id, handler) ->
    let new_body = f body in
    let new_handler = f handler in
    if body == new_body && handler == new_handler then
      tree
    else
      Try_with(new_body, id, new_handler)
  | If_then_else(arg, ifso, ifnot) ->
    let new_ifso = f ifso in
    let new_ifnot = f ifnot in
    if new_ifso == ifso && new_ifnot == ifnot then
      tree
    else
      If_then_else(arg, new_ifso, new_ifnot)
  | While(cond, body) ->
    let new_cond = f cond in
    let new_body = f body in
    if new_cond == cond && new_body == body then
      tree
    else
      While(new_cond, new_body)
  | For { bound_var; from_value; to_value; direction; body; } ->
    let new_body = f body in
    if new_body == body then
      tree
    else
      For { bound_var; from_value; to_value; direction; body = new_body; }

let iter_general = Flambda.iter_general

let iter f f_named t = iter_general ~toplevel:false f f_named (Is_expr t)
let iter_expr f t = iter f (fun _ -> ()) t
let iter_on_named f f_named t =
  iter_general ~toplevel:false f f_named (Is_named t)
let iter_named f_named t = iter (fun (_ : Flambda.t) -> ()) f_named t
let iter_named_on_named f_named named =
  iter_general ~toplevel:false (fun (_ : Flambda.t) -> ()) f_named
    (Is_named named)

let iter_toplevel f f_named t =
  iter_general ~toplevel:true f f_named (Is_expr t)
let iter_named_toplevel f f_named named =
  iter_general ~toplevel:true f f_named (Is_named named)

let iter_all_immutable_let_and_let_rec_bindings t ~f =
  iter_expr (function
      | Let { var; defining_expr; _ } -> f var defining_expr
      | Let_rec (defs, _) -> List.iter (fun (var, named) -> f var named) defs
      | _ -> ())
    t

let iter_all_toplevel_immutable_let_and_let_rec_bindings t ~f =
  iter_general ~toplevel:true
    (function
      | Let { var; defining_expr; _ } -> f var defining_expr
      | Let_rec (defs, _) -> List.iter (fun (var, named) -> f var named) defs
      | _ -> ())
    (fun _ -> ())
    (Is_expr t)

let iter_on_sets_of_closures f t =
  iter_named (function
      | Set_of_closures clos -> f clos
      | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _ -> ())
    t

let iter_exprs_at_toplevel_of_program (program : Flambda.program) ~f =
  let rec loop (program : Flambda.program_body) =
    match program with
    | Let_symbol (_, Set_of_closures set_of_closures, program) ->
      Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
          f function_decl.body)
        set_of_closures.function_decls.funs;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (function
          | (_, Flambda.Set_of_closures set_of_closures) ->
            Variable.Map.iter
              (fun _ (function_decl : Flambda.function_declaration) ->
                f function_decl.body)
              set_of_closures.function_decls.funs
          | _ -> ()) defs;
      loop program
    | Let_symbol (_, _, program) ->
      loop program
    | Initialize_symbol (_, _, fields, program) ->
      List.iter f fields;
      loop program
    | Effect (expr, program) ->
      f expr;
      loop program
    | End _ -> ()
  in
  loop program.program_body

let iter_named_of_program program ~f =
  iter_exprs_at_toplevel_of_program program ~f:(iter_named f)

let iter_on_set_of_closures_of_program (program : Flambda.program) ~f =
  let rec loop (program : Flambda.program_body) =
    match program with
    | Let_symbol (_, Set_of_closures set_of_closures, program) ->
      f ~constant:true set_of_closures;
      Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
          iter_on_sets_of_closures (f ~constant:false) function_decl.body)
        set_of_closures.function_decls.funs;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (function
          | (_, Flambda.Set_of_closures set_of_closures) ->
            f ~constant:true set_of_closures;
            Variable.Map.iter
              (fun _ (function_decl : Flambda.function_declaration) ->
                iter_on_sets_of_closures (f ~constant:false) function_decl.body)
              set_of_closures.function_decls.funs
          | _ -> ()) defs;
      loop program
    | Let_symbol (_, _, program) ->
      loop program
    | Initialize_symbol (_, _, fields, program) ->
      List.iter (iter_on_sets_of_closures (f ~constant:false)) fields;
      loop program
    | Effect (expr, program) ->
      iter_on_sets_of_closures (f ~constant:false) expr;
      loop program
    | End _ -> ()
  in
  loop program.program_body

let iter_constant_defining_values_on_program (program : Flambda.program) ~f =
  let rec loop (program : Flambda.program_body) =
    match program with
    | Let_symbol (_, const, program) ->
      f const;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (fun (_, const) -> f const) defs;
      loop program
    | Initialize_symbol (_, _, _, program) ->
      loop program
    | Effect (_, program) ->
      loop program
    | End _ -> ()
  in
  loop program.program_body

let map_general ~toplevel f f_named tree =
  let rec aux (tree : Flambda.t) =
    match tree with
    | Let _ ->
      Flambda.map_lets tree ~for_defining_expr:aux_named ~for_last_body:aux
        ~after_rebuild:f
    | _ ->
      let exp : Flambda.t =
        match tree with
        | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable
        | Static_raise _ -> tree
        | Let _ -> assert false
        | Let_mutable mutable_let ->
          let new_body = aux mutable_let.body in
          if new_body == mutable_let.body then
            tree
          else
            Let_mutable { mutable_let with body = new_body }
        | Let_rec (defs, body) ->
          let done_something = ref false in
          let defs =
            List.map (fun (id, lam) ->
                id, aux_named_done_something id lam done_something)
              defs
          in
          let body = aux_done_something body done_something in
          if not !done_something then
            tree
          else
            Let_rec (defs, body)
        | Switch (arg, sw) ->
          let done_something = ref false in
          let sw =
            { sw with
              failaction =
                begin match sw.failaction with
                | None -> None
                | Some failaction ->
                  Some (aux_done_something failaction done_something)
                end;
              consts =
                List.map (fun (i, v) ->
                    i, aux_done_something v done_something)
                  sw.consts;
              blocks =
                List.map (fun (i, v) ->
                    i, aux_done_something v done_something)
                  sw.blocks;
            }
          in
          if not !done_something then
            tree
          else
            Switch (arg, sw)
        | String_switch (arg, sw, def) ->
          let done_something = ref false in
          let sw =
            List.map (fun (i, v) -> i, aux_done_something v done_something) sw
          in
          let def =
            match def with
            | None -> None
            | Some def -> Some (aux_done_something def done_something)
          in
          if not !done_something then
            tree
          else
            String_switch(arg, sw, def)
        | Static_catch (i, vars, body, handler) ->
          let new_body = aux body in
          let new_handler = aux handler in
          if new_body == body && new_handler == handler then
            tree
          else
            Static_catch (i, vars, new_body, new_handler)
        | Try_with(body, id, handler) ->
          let new_body = aux body in
          let new_handler = aux handler in
          if new_body == body && new_handler == handler then
            tree
          else
            Try_with (new_body, id, new_handler)
        | If_then_else (arg, ifso, ifnot) ->
          let new_ifso = aux ifso in
          let new_ifnot = aux ifnot in
          if new_ifso == ifso && new_ifnot == ifnot then
            tree
          else
            If_then_else (arg, new_ifso, new_ifnot)
        | While (cond, body) ->
          let new_cond = aux cond in
          let new_body = aux body in
          if new_cond == cond && new_body == body then
            tree
          else
            While (new_cond, new_body)
        | For { bound_var; from_value; to_value; direction; body; } ->
          let new_body = aux body in
          if new_body == body then
            tree
          else
            For { bound_var; from_value; to_value; direction;
              body = new_body; }
      in
      f exp
  and aux_done_something expr done_something =
    let new_expr = aux expr in
    if not (new_expr == expr) then begin
      done_something := true
    end;
    new_expr
  and aux_named (id : Variable.t) (named : Flambda.named) =
    let named : Flambda.named =
      match named with
      | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Read_symbol_field _ -> named
      | Set_of_closures ({ function_decls; free_vars; specialised_args;
          direct_call_surrogates }) ->
        if toplevel then named
        else begin
          let done_something = ref false in
          let funs =
            Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
                let new_body = aux func_decl.body in
                if new_body == func_decl.body then begin
                  func_decl
                end else begin
                  done_something := true;
                  Flambda.create_function_declaration
                    ~params:func_decl.params
                    ~body:new_body
                    ~stub:func_decl.stub
                    ~dbg:func_decl.dbg
                    ~inline:func_decl.inline
                    ~specialise:func_decl.specialise
                    ~is_a_functor:func_decl.is_a_functor
                end)
              function_decls.funs
          in
          if not !done_something then
            named
          else
            let function_decls =
              Flambda.update_function_declarations function_decls ~funs
            in
            let set_of_closures =
              Flambda.create_set_of_closures ~function_decls ~free_vars
                ~specialised_args ~direct_call_surrogates
            in
            Set_of_closures set_of_closures
        end
      | Expr expr ->
        let new_expr = aux expr in
        if new_expr == expr then named
        else Expr new_expr
    in
    f_named id named
  and aux_named_done_something id named done_something =
    let new_named = aux_named id named in
    if not (new_named == named) then begin
      done_something := true
    end;
    new_named
  in
  aux tree

let iter_apply_on_program program ~f =
  iter_exprs_at_toplevel_of_program program ~f:(fun expr ->
    iter (function
        | Apply apply -> f apply
        | _ -> ())
      (fun _ -> ())
      expr)

let map f f_named tree =
  map_general ~toplevel:false f (fun _ n -> f_named n) tree
let map_expr f tree = map f (fun named -> named) tree
let map_named f_named tree = map (fun expr -> expr) f_named tree
let map_named_with_id f_named tree =
  map_general ~toplevel:false (fun expr -> expr) f_named tree
let map_toplevel f f_named tree =
  map_general ~toplevel:true f (fun _ n -> f_named n) tree
let map_toplevel_expr f_expr tree =
  map_toplevel f_expr (fun named -> named) tree
let map_toplevel_named f_named tree =
  map_toplevel (fun tree -> tree) f_named tree

let map_symbols tree ~f =
  map_named (function
      | (Symbol sym) as named ->
        let new_sym = f sym in
        if new_sym == sym then
          named
        else
          Symbol new_sym
      | ((Read_symbol_field (sym, field)) as named) ->
        let new_sym = f sym in
        if new_sym == sym then
          named
        else
          Read_symbol_field (new_sym, field)
      | (Const _ | Allocated_const _ | Set_of_closures _ | Read_mutable _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _) as named -> named)
    tree

let map_symbols_on_set_of_closures
    ({ Flambda.function_decls; free_vars; specialised_args;
        direct_call_surrogates; } as
      set_of_closures)
    ~f =
  let done_something = ref false in
  let funs =
    Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
        let body = map_symbols func_decl.body ~f in
        if not (body == func_decl.body) then begin
          done_something := true;
        end;
        Flambda.create_function_declaration
          ~params:func_decl.params
          ~body
          ~stub:func_decl.stub
          ~dbg:func_decl.dbg
          ~inline:func_decl.inline
          ~specialise:func_decl.specialise
          ~is_a_functor:func_decl.is_a_functor)
      function_decls.funs
  in
  if not !done_something then
    set_of_closures
  else
    let function_decls =
      Flambda.update_function_declarations function_decls ~funs
    in
    Flambda.create_set_of_closures ~function_decls ~free_vars
      ~specialised_args ~direct_call_surrogates

let map_toplevel_sets_of_closures tree ~f =
  map_toplevel_named (function
      | (Set_of_closures set_of_closures) as named ->
        let new_set_of_closures = f set_of_closures in
        if new_set_of_closures == set_of_closures then
          named
        else
          Set_of_closures new_set_of_closures
      | (Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _) as named -> named)
    tree

let map_apply tree ~f =
  map (function
      | (Apply apply) as expr ->
        let new_apply = f apply in
        if new_apply == apply then
          expr
        else
          Apply new_apply
      | expr -> expr)
    (fun named -> named)
    tree

let map_sets_of_closures tree ~f =
  map_named (function
      | (Set_of_closures set_of_closures) as named ->
        let new_set_of_closures = f set_of_closures in
        if new_set_of_closures == set_of_closures then
          named
        else
          Set_of_closures new_set_of_closures
      | (Symbol _ | Const _ | Allocated_const _ | Project_closure _
      | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _ | Read_mutable _
      | Read_symbol_field _) as named -> named)
    tree

let map_project_var_to_expr_opt tree ~f =
  map_named (function
      | (Project_var project_var) as named ->
        begin match f project_var with
        | None -> named
        | Some expr -> Expr expr
        end
      | (Symbol _ | Const _ | Allocated_const _
      | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _
      | Prim _ | Expr _ | Read_mutable _ | Read_symbol_field _)
          as named -> named)
    tree

let map_project_var_to_named_opt tree ~f =
  map_named (function
      | (Project_var project_var) as named ->
        begin match f project_var with
        | None -> named
        | Some named -> named
        end
      | (Symbol _ | Const _ | Allocated_const _
      | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _
      | Prim _ | Expr _ | Read_mutable _ | Read_symbol_field _)
          as named -> named)
    tree

let map_function_bodies (set_of_closures : Flambda.set_of_closures) ~f =
  let done_something = ref false in
  let funs =
    Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
        let new_body = f function_decl.body in
        if new_body == function_decl.body then
          function_decl
        else begin
          done_something := true;
          Flambda.create_function_declaration ~body:new_body
            ~params:function_decl.params
            ~stub:function_decl.stub
            ~dbg:function_decl.dbg
            ~inline:function_decl.inline
            ~specialise:function_decl.specialise
            ~is_a_functor:function_decl.is_a_functor
        end)
      set_of_closures.function_decls.funs
  in
  if not !done_something then
    set_of_closures
  else
    let function_decls =
      Flambda.update_function_declarations set_of_closures.function_decls ~funs
    in
    Flambda.create_set_of_closures
      ~function_decls
      ~free_vars:set_of_closures.free_vars
      ~specialised_args:set_of_closures.specialised_args
      ~direct_call_surrogates:set_of_closures.direct_call_surrogates

let map_sets_of_closures_of_program (program : Flambda.program)
    ~(f : Flambda.set_of_closures -> Flambda.set_of_closures) =
  let rec loop (program : Flambda.program_body) : Flambda.program_body =
    let map_constant_set_of_closures (set_of_closures:Flambda.set_of_closures) =
      let done_something = ref false in
      let function_decls =
        let funs =
          Variable.Map.map (fun
                  (function_decl : Flambda.function_declaration) ->
              let body = map_sets_of_closures ~f function_decl.body in
              if body == function_decl.body then
                function_decl
              else begin
                done_something := true;
                Flambda.create_function_declaration ~body
                  ~params:function_decl.params
                  ~stub:function_decl.stub
                  ~dbg:function_decl.dbg
                  ~inline:function_decl.inline
                  ~specialise:function_decl.specialise
                  ~is_a_functor:function_decl.is_a_functor
              end)
            set_of_closures.function_decls.funs
        in
        if not !done_something then
          set_of_closures.function_decls
        else
          Flambda.update_function_declarations set_of_closures.function_decls
            ~funs
      in
      let new_set_of_closures = f set_of_closures in
      if new_set_of_closures == set_of_closures then
        set_of_closures
      else
        Flambda.create_set_of_closures ~function_decls
          ~free_vars:set_of_closures.free_vars
          ~specialised_args:set_of_closures.specialised_args
          ~direct_call_surrogates:set_of_closures.direct_call_surrogates
    in
    match program with
    | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
      let new_set_of_closures = map_constant_set_of_closures set_of_closures in
      let new_program' = loop program' in
      if new_set_of_closures == set_of_closures
          && new_program' == program' then
        program
      else
        Let_symbol (symbol, Set_of_closures new_set_of_closures, new_program')
    | Let_symbol (symbol, const, program') ->
      let new_program' = loop program' in
      if new_program' == program' then
        program
      else
        Let_symbol (symbol, const, new_program')
    | Let_rec_symbol (defs, program') ->
      let done_something = ref false in
      let defs =
        List.map (function
            | (var, Flambda.Set_of_closures set_of_closures) ->
              let new_set_of_closures =
                map_constant_set_of_closures set_of_closures
              in
              if not (new_set_of_closures == set_of_closures) then begin
                done_something := true
              end;
              var, Flambda.Set_of_closures new_set_of_closures
            | def -> def)
          defs
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Let_rec_symbol (defs, loop program')
    | Initialize_symbol (symbol, tag, fields, program') ->
      let done_something = ref false in
      let fields =
        List.map (fun field ->
            let new_field = map_sets_of_closures field ~f in
            if not (new_field == field) then begin
              done_something := true
            end;
            new_field)
          fields
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Initialize_symbol (symbol, tag, fields, new_program')
    | Effect (expr, program') ->
      let new_expr = map_sets_of_closures expr ~f in
      let new_program' = loop program' in
      if new_expr == expr && new_program' == program' then
        program
      else
        Effect (new_expr, new_program')
    | End _ -> program
  in
  { program with
    program_body = loop program.program_body;
  }

let map_exprs_at_toplevel_of_program (program : Flambda.program)
    ~(f : Flambda.t -> Flambda.t) =
  let rec loop (program : Flambda.program_body) : Flambda.program_body =
    let map_constant_set_of_closures (set_of_closures:Flambda.set_of_closures) =
      let done_something = ref false in
      let funs =
        Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
            let body = f function_decl.body in
            if body == function_decl.body then
              function_decl
            else begin
              done_something := true;
              Flambda.create_function_declaration ~body
                ~params:function_decl.params
                ~stub:function_decl.stub
                ~dbg:function_decl.dbg
                ~inline:function_decl.inline
                ~specialise:function_decl.specialise
                ~is_a_functor:function_decl.is_a_functor
            end)
          set_of_closures.function_decls.funs
      in
      if not !done_something then
        set_of_closures
      else
        let function_decls =
          Flambda.update_function_declarations set_of_closures.function_decls
            ~funs
        in
        Flambda.create_set_of_closures ~function_decls
          ~free_vars:set_of_closures.free_vars
          ~specialised_args:set_of_closures.specialised_args
          ~direct_call_surrogates:set_of_closures.direct_call_surrogates
    in
    (* CR-soon mshinwell: code very similar to the above function *)
    match program with
    | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
      let new_set_of_closures = map_constant_set_of_closures set_of_closures in
      let new_program' = loop program' in
      if new_set_of_closures == set_of_closures
          && new_program' == program' then
        program
      else
        Let_symbol (symbol, Set_of_closures new_set_of_closures, new_program')
    | Let_symbol (symbol, const, program') ->
      let new_program' = loop program' in
      if new_program' == program' then
        program
      else
        Let_symbol (symbol, const, new_program')
    | Let_rec_symbol (defs, program') ->
      let done_something = ref false in
      let defs =
        List.map (function
            | (var, Flambda.Set_of_closures set_of_closures) ->
              let new_set_of_closures =
                map_constant_set_of_closures set_of_closures
              in
              if not (new_set_of_closures == set_of_closures) then begin
                done_something := true
              end;
              var, Flambda.Set_of_closures new_set_of_closures
            | def -> def)
          defs
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Let_rec_symbol (defs, new_program')
    | Initialize_symbol (symbol, tag, fields, program') ->
      let done_something = ref false in
      let fields =
        List.map (fun field ->
            let new_field = f field in
            if not (new_field == field) then begin
              done_something := true
            end;
            new_field)
          fields
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Initialize_symbol (symbol, tag, fields, new_program')
    | Effect (expr, program') ->
      let new_expr = f expr in
      let new_program' = loop program' in
      if new_expr == expr && new_program' == program' then
        program
      else
        Effect (new_expr, new_program')
    | End _ -> program
  in
  { program with
    program_body = loop program.program_body;
  }

let map_named_of_program (program : Flambda.program)
      ~(f : Variable.t -> Flambda.named -> Flambda.named) : Flambda.program =
  map_exprs_at_toplevel_of_program program
      ~f:(fun expr -> map_named_with_id f expr)

let map_all_immutable_let_and_let_rec_bindings (expr : Flambda.t)
      ~(f : Variable.t -> Flambda.named -> Flambda.named) : Flambda.t =
  map_named_with_id f expr

let fold_function_decls_ignoring_stubs
      (set_of_closures : Flambda.set_of_closures) ~init ~f =
  Variable.Map.fold (fun fun_var function_decl acc ->
      f ~fun_var ~function_decl acc)
    set_of_closures.function_decls.funs
    init
