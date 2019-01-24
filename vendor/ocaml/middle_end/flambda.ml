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

type call_kind =
  | Indirect
  | Direct of Closure_id.t

type const =
  | Int of int
  | Char of char
  | Const_pointer of int

type apply = {
  func : Variable.t;
  args : Variable.t list;
  kind : call_kind;
  dbg : Debuginfo.t;
  inline : Lambda.inline_attribute;
  specialise : Lambda.specialise_attribute;
}

type assign = {
  being_assigned : Mutable_variable.t;
  new_value : Variable.t;
}

type send = {
  kind : Lambda.meth_kind;
  meth : Variable.t;
  obj : Variable.t;
  args : Variable.t list;
  dbg : Debuginfo.t;
}

type project_closure = Projection.project_closure
type move_within_set_of_closures = Projection.move_within_set_of_closures
type project_var = Projection.project_var

type specialised_to = {
  var : Variable.t;
  projection : Projection.t option;
}

type t =
  | Var of Variable.t
  | Let of let_expr
  | Let_mutable of let_mutable
  | Let_rec of (Variable.t * named) list * t
  | Apply of apply
  | Send of send
  | Assign of assign
  | If_then_else of Variable.t * t * t
  | Switch of Variable.t * switch
  | String_switch of Variable.t * (string * t) list * t option
  | Static_raise of Static_exception.t * Variable.t list
  | Static_catch of Static_exception.t * Variable.t list * t * t
  | Try_with of t * Variable.t * t
  | While of t * t
  | For of for_loop
  | Proved_unreachable

and named =
  | Symbol of Symbol.t
  | Const of const
  | Allocated_const of Allocated_const.t
  | Read_mutable of Mutable_variable.t
  | Read_symbol_field of Symbol.t * int
  | Set_of_closures of set_of_closures
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Project_var of project_var
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t
  | Expr of t

and let_expr = {
  var : Variable.t;
  defining_expr : named;
  body : t;
  free_vars_of_defining_expr : Variable.Set.t;
  free_vars_of_body : Variable.Set.t;
}

and let_mutable = {
  var : Mutable_variable.t;
  initial_value : Variable.t;
  contents_kind : Lambda.value_kind;
  body : t;
}

and set_of_closures = {
  function_decls : function_declarations;
  free_vars : specialised_to Variable.Map.t;
  specialised_args : specialised_to Variable.Map.t;
  direct_call_surrogates : Variable.t Variable.Map.t;
}

and function_declarations = {
  set_of_closures_id : Set_of_closures_id.t;
  set_of_closures_origin : Set_of_closures_origin.t;
  funs : function_declaration Variable.Map.t;
}

and function_declaration = {
  params : Parameter.t list;
  body : t;
  free_variables : Variable.Set.t;
  free_symbols : Symbol.Set.t;
  stub : bool;
  dbg : Debuginfo.t;
  inline : Lambda.inline_attribute;
  specialise : Lambda.specialise_attribute;
  is_a_functor : bool;
}

and switch = {
  numconsts : Numbers.Int.Set.t;
  consts : (int * t) list;
  numblocks : Numbers.Int.Set.t;
  blocks : (int * t) list;
  failaction : t option;
}

and for_loop = {
  bound_var : Variable.t;
  from_value : Variable.t;
  to_value : Variable.t;
  direction : Asttypes.direction_flag;
  body : t
}

and constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * constant_defining_value_block_field list
  | Set_of_closures of set_of_closures  (* [free_vars] must be empty *)
  | Project_closure of Symbol.t * Closure_id.t

and constant_defining_value_block_field =
  | Symbol of Symbol.t
  | Const of const

type expr = t

type program_body =
  | Let_symbol of Symbol.t * constant_defining_value * program_body
  | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program_body
  | Initialize_symbol of Symbol.t * Tag.t * t list * program_body
  | Effect of t * program_body
  | End of Symbol.t

type program = {
  imported_symbols : Symbol.Set.t;
  program_body : program_body;
}

let fprintf = Format.fprintf
module Int = Numbers.Int

let print_specialised_to ppf (spec_to : specialised_to) =
  match spec_to.projection with
  | None -> fprintf ppf "%a" Variable.print spec_to.var
  | Some projection ->
    fprintf ppf "%a(= %a)"
      Variable.print spec_to.var
      Projection.print projection

(* CR-soon mshinwell: delete uses of old names *)
let print_project_var = Projection.print_project_var
let print_move_within_set_of_closures =
  Projection.print_move_within_set_of_closures
let print_project_closure = Projection.print_project_closure

(** CR-someday lwhite: use better name than this *)
let rec lam ppf (flam : t) =
  match flam with
  | Var (id) ->
      Variable.print ppf id
  | Apply({func; args; kind; inline; dbg}) ->
    let direct ppf () =
      match kind with
      | Indirect -> ()
      | Direct closure_id -> fprintf ppf "*[%a]" Closure_id.print closure_id
    in
    let inline ppf () =
      match inline with
      | Always_inline -> fprintf ppf "<always>"
      | Never_inline -> fprintf ppf "<never>"
      | Unroll i -> fprintf ppf "<unroll %i>" i
      | Default_inline -> ()
    in
    fprintf ppf "@[<2>(apply%a%a<%s>@ %a%a)@]" direct () inline ()
      (Debuginfo.to_string dbg)
      Variable.print func Variable.print_list args
  | Assign { being_assigned; new_value; } ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]"
      Mutable_variable.print being_assigned
      Variable.print new_value
  | Send { kind; meth; obj; args; dbg = _; } ->
    let print_args ppf args =
      List.iter (fun l -> fprintf ppf "@ %a" Variable.print l) args
    in
    let kind =
      match kind with
      | Self -> "self"
      | Public _ -> "public"
      | Cached -> "cached"
    in
    fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind
      Variable.print obj Variable.print meth
      print_args args
  | Proved_unreachable ->
      fprintf ppf "unreachable"
  | Let { var = id; defining_expr = arg; body; _ } ->
      let rec letbody (ul : t) =
        match ul with
        | Let { var = id; defining_expr = arg; body; _ } ->
            fprintf ppf "@ @[<2>%a@ %a@]" Variable.print id print_named arg;
            letbody body
        | _ -> ul
      in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]"
        Variable.print id print_named arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Let_mutable { var = mut_var; initial_value = var; body; contents_kind } ->
    let print_kind ppf (kind : Lambda.value_kind) =
      match kind with
      | Pgenval -> ()
      | _ -> Format.fprintf ppf " %s" (Printlambda.value_kind kind)
    in
    fprintf ppf "@[<2>(let_mutable%a@ @[<2>%a@ %a@]@ %a)@]"
      print_kind contents_kind
      Mutable_variable.print mut_var
      Variable.print var
      lam body
  | Let_rec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<2>%a@ %a@]" Variable.print id print_named l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Switch(larg, sw) ->
      let switch ppf (sw : switch) =
        let spc = ref false in
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
          sw.consts;
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.blocks ;
        begin match sw.failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
        "@[<1>(%s(%i,%i) %a@ @[<v 0>%a@])@]"
        (match sw.failaction with None -> "switch*" | _ -> "switch")
        (Int.Set.cardinal sw.numconsts)
        (Int.Set.cardinal sw.numblocks)
        Variable.print larg switch sw
  | String_switch(arg, cases, default) ->
      let switch ppf cases =
        let spc = ref false in
        List.iter
         (fun (s, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
          cases;
        begin match default with
        | Some default ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam default
        | None -> ()
        end in
      fprintf ppf
       "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" Variable.print arg switch cases
  | Static_raise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" Variable.print l) largs in
      fprintf ppf "@[<2>(exit@ %a%a)@]" Static_exception.print i lams ls;
  | Static_catch(i, vars, lbody, lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%a%a)@ %a)@]"
        lam lbody Static_exception.print i
        (fun ppf vars -> match vars with
           | [] -> ()
           | _ ->
               List.iter
                 (fun x -> fprintf ppf " %a" Variable.print x)
                 vars)
        vars
        lam lhandler
  | Try_with(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Variable.print param lam lhandler
  | If_then_else(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ then begin@ %a@ end else begin@ %a@ end)@]"
        Variable.print lcond
        lam lif lam lelse
  | While(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | For { bound_var; from_value; to_value; direction; body; } ->
    fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
      Variable.print bound_var Variable.print from_value
      (match direction with
        Asttypes.Upto -> "to" | Asttypes.Downto -> "downto")
      Variable.print to_value lam body
and print_named ppf (named : named) =
  match named with
  | Symbol (symbol) -> Symbol.print ppf symbol
  | Const (cst) -> fprintf ppf "Const(%a)" print_const cst
  | Allocated_const (cst) -> fprintf ppf "Aconst(%a)" Allocated_const.print cst
  | Read_mutable mut_var ->
    fprintf ppf "Read_mut(%a)" Mutable_variable.print mut_var
  | Read_symbol_field (symbol, field) ->
    fprintf ppf "%a.(%d)" Symbol.print symbol field
  | Project_closure (project_closure) ->
    print_project_closure ppf project_closure
  | Project_var (project_var) -> print_project_var ppf project_var
  | Move_within_set_of_closures (move_within_set_of_closures) ->
    print_move_within_set_of_closures ppf move_within_set_of_closures
  | Set_of_closures (set_of_closures) ->
    print_set_of_closures ppf set_of_closures
  | Prim(prim, args, dbg) ->
    fprintf ppf "@[<2>(%a<%s>%a)@]" Printlambda.primitive prim
      (Debuginfo.to_string dbg)
      Variable.print_list args
  | Expr expr ->
    fprintf ppf "*%a" lam expr
    (* lam ppf expr *)

and print_function_declaration ppf var (f : function_declaration) =
  let param ppf p =
    Variable.print ppf (Parameter.var p)
  in
  let params ppf =
    List.iter (fprintf ppf "@ %a" param) in
  let stub =
    if f.stub then
      " *stub*"
    else
      ""
  in
  let is_a_functor =
    if f.is_a_functor then
      " *functor*"
    else
      ""
  in
  let inline =
    match f.inline with
    | Always_inline -> " *inline*"
    | Never_inline -> " *never_inline*"
    | Unroll _ -> " *unroll*"
    | Default_inline -> ""
  in
  let specialise =
    match f.specialise with
    | Always_specialise -> " *specialise*"
    | Never_specialise -> " *never_specialise*"
    | Default_specialise -> ""
  in
  fprintf ppf "@[<2>(%a%s%s%s%s@ =@ fun@[<2>%a@] ->@ @[<2>%a@])@]@ "
    Variable.print var stub is_a_functor inline specialise
    params f.params lam f.body

and print_set_of_closures ppf (set_of_closures : set_of_closures) =
  match set_of_closures with
  | { function_decls; free_vars; specialised_args} ->
    let funs ppf =
      Variable.Map.iter (print_function_declaration ppf)
    in
    let vars ppf =
      Variable.Map.iter (fun id v ->
          fprintf ppf "@ %a -rename-> %a"
            Variable.print id print_specialised_to v)
    in
    let spec ppf spec_args =
      if not (Variable.Map.is_empty spec_args)
      then begin
        fprintf ppf "@ ";
        Variable.Map.iter (fun id (spec_to : specialised_to) ->
            fprintf ppf "@ %a := %a"
              Variable.print id print_specialised_to spec_to)
          spec_args
      end
    in
    fprintf ppf "@[<2>(set_of_closures id=%a@ %a@ @[<2>free_vars={%a@ }@]@ \
        @[<2>specialised_args={%a})@]@ \
        @[<2>direct_call_surrogates=%a@]@]"
      Set_of_closures_id.print function_decls.set_of_closures_id
      funs function_decls.funs
      vars free_vars
      spec specialised_args
      (Variable.Map.print Variable.print)
      set_of_closures.direct_call_surrogates

and print_const ppf (c : const) =
  match c with
  | Int n -> fprintf ppf "%i" n
  | Char c -> fprintf ppf "%C" c
  | Const_pointer n -> fprintf ppf "%ia" n

let print_function_declarations ppf (fd : function_declarations) =
  let funs ppf =
    Variable.Map.iter (print_function_declaration ppf)
  in
  fprintf ppf "@[<2>(%a)@]" funs fd.funs

let print ppf flam =
  fprintf ppf "%a@." lam flam

let print_function_declaration ppf (var, decl) =
  print_function_declaration ppf var decl

let print_constant_defining_value ppf (const : constant_defining_value) =
  match const with
  | Allocated_const const ->
    fprintf ppf "(Allocated_const %a)" Allocated_const.print const
  | Block (tag, []) -> fprintf ppf "(Atom (tag %d))" (Tag.to_int tag)
  | Block (tag, fields) ->
    let print_field ppf (field : constant_defining_value_block_field) =
      match field with
      | Symbol symbol -> Symbol.print ppf symbol
      | Const const -> print_const ppf const
    in
    let print_fields ppf =
      List.iter (fprintf ppf "@ %a" print_field)
    in
    fprintf ppf "(Block (tag %d, %a))" (Tag.to_int tag)
      print_fields fields
  | Set_of_closures set_of_closures ->
    fprintf ppf "@[<2>(Set_of_closures (@ %a))@]" print_set_of_closures
      set_of_closures
  | Project_closure (set_of_closures, closure_id) ->
    fprintf ppf "(Project_closure (%a, %a))" Symbol.print set_of_closures
      Closure_id.print closure_id

let rec print_program_body ppf (program : program_body) =
  let symbol_binding ppf (symbol, constant_defining_value) =
    fprintf ppf "@[<2>(%a@ %a)@]"
      Symbol.print symbol
      print_constant_defining_value constant_defining_value
  in
  match program with
  | Let_symbol (symbol, constant_defining_value, body) ->
    let rec extract acc (ul : program_body) =
      match ul with
      | Let_symbol (symbol, constant_defining_value, body) ->
        extract ((symbol, constant_defining_value) :: acc) body
      | _ ->
        List.rev acc,  ul
    in
    let defs, program = extract [symbol, constant_defining_value] body in
    fprintf ppf
      "@[<2>let_symbol@ @[%a@]@]@."
      (Format.pp_print_list symbol_binding) defs;
    print_program_body ppf program
  | Let_rec_symbol (defs, program) ->
    fprintf ppf
      "@[<2>let_rec_symbol@ @[%a@]@]@."
      (Format.pp_print_list symbol_binding) defs;
    print_program_body ppf program
  | Initialize_symbol (symbol, tag, fields, program) ->
    fprintf ppf "@[<2>initialize_symbol@ (@[<2>%a@ %a@ %a@])@]@."
      Symbol.print symbol
      Tag.print tag
      (Format.pp_print_list lam) fields;
    print_program_body ppf program
  | Effect (expr, program) ->
    fprintf ppf "@[<2>effect@ %a@]@."
      lam expr;
    print_program_body ppf program;
  | End root -> fprintf ppf "End %a" Symbol.print root

let print_program ppf program =
  Symbol.Set.iter (fun symbol ->
      fprintf ppf "@[import_symbol@ %a@]@." Symbol.print symbol)
    program.imported_symbols;
  print_program_body ppf program.program_body

let rec variables_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_in_project_var ~all_used_variables tree =
  match tree with
  | Var var -> Variable.Set.singleton var
  | _ ->
    let free = ref Variable.Set.empty in
    let bound = ref Variable.Set.empty in
    let free_variables ids = free := Variable.Set.union ids !free in
    let free_variable fv = free := Variable.Set.add fv !free in
    let bound_variable id = bound := Variable.Set.add id !bound in
    (* N.B. This function assumes that all bound identifiers are distinct. *)
    let rec aux (flam : t) : unit =
      match flam with
      | Var var -> free_variable var
      | Apply { func; args; kind = _; dbg = _} ->
        begin match ignore_uses_as_callee with
        | None -> free_variable func
        | Some () -> ()
        end;
        begin match ignore_uses_as_argument with
        | None -> List.iter free_variable args
        | Some () -> ()
        end
      | Let { var; free_vars_of_defining_expr; free_vars_of_body;
              defining_expr; body; _ } ->
        bound_variable var;
        if all_used_variables
           || ignore_uses_as_callee <> None
           || ignore_uses_as_argument <> None
           || ignore_uses_in_project_var <> None
        then begin
          (* In these cases we can't benefit from the pre-computed free
             variable sets. *)
          free_variables
            (variables_usage_named ?ignore_uses_in_project_var
                ?ignore_uses_as_callee ?ignore_uses_as_argument
                ~all_used_variables defining_expr);
          aux body
        end else begin
          free_variables free_vars_of_defining_expr;
          free_variables free_vars_of_body
        end
      | Let_mutable { initial_value = var; body; _ } ->
        free_variable var;
        aux body
      | Let_rec (bindings, body) ->
        List.iter (fun (var, defining_expr) ->
            bound_variable var;
            free_variables
              (variables_usage_named ?ignore_uses_in_project_var
                 ~all_used_variables defining_expr))
          bindings;
        aux body
      | Switch (scrutinee, switch) ->
        free_variable scrutinee;
        List.iter (fun (_, e) -> aux e) switch.consts;
        List.iter (fun (_, e) -> aux e) switch.blocks;
        Misc.may aux switch.failaction
      | String_switch (scrutinee, cases, failaction) ->
        free_variable scrutinee;
        List.iter (fun (_, e) -> aux e) cases;
        Misc.may aux failaction
      | Static_raise (_, es) ->
        List.iter free_variable es
      | Static_catch (_, vars, e1, e2) ->
        List.iter bound_variable vars;
        aux e1;
        aux e2
      | Try_with (e1, var, e2) ->
        aux e1;
        bound_variable var;
        aux e2
      | If_then_else (var, e1, e2) ->
        free_variable var;
        aux e1;
        aux e2
      | While (e1, e2) ->
        aux e1;
        aux e2
      | For { bound_var; from_value; to_value; direction = _; body; } ->
        bound_variable bound_var;
        free_variable from_value;
        free_variable to_value;
        aux body
      | Assign { being_assigned = _; new_value; } ->
        free_variable new_value
      | Send { kind = _; meth; obj; args; dbg = _ } ->
        free_variable meth;
        free_variable obj;
        List.iter free_variable args;
      | Proved_unreachable -> ()
    in
    aux tree;
    if all_used_variables then
      !free
    else
      Variable.Set.diff !free !bound

and variables_usage_named ?ignore_uses_in_project_var
    ?ignore_uses_as_callee ?ignore_uses_as_argument
    ~all_used_variables named =
  let free = ref Variable.Set.empty in
  let free_variable fv = free := Variable.Set.add fv !free in
  begin match named with
  | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
  | Read_symbol_field _ -> ()
  | Set_of_closures { free_vars; specialised_args; _ } ->
    (* Sets of closures are, well, closed---except for the free variable and
       specialised argument lists, which may identify variables currently in
       scope outside of the closure. *)
    Variable.Map.iter (fun _ (renamed_to : specialised_to) ->
        (* We don't need to do anything with [renamed_to.projectee.var], if
           it is present, since it would only be another free variable
           in the same set of closures. *)
        free_variable renamed_to.var)
      free_vars;
    Variable.Map.iter (fun _ (spec_to : specialised_to) ->
        (* We don't need to do anything with [spec_to.projectee.var], if
           it is present, since it would only be another specialised arg
           in the same set of closures. *)
        free_variable spec_to.var)
      specialised_args
  | Project_closure { set_of_closures; closure_id = _ } ->
    free_variable set_of_closures
  | Project_var { closure; closure_id = _; var = _ } ->
    begin match ignore_uses_in_project_var with
    | None -> free_variable closure
    | Some () -> ()
    end
  | Move_within_set_of_closures { closure; start_from = _; move_to = _ } ->
    free_variable closure
  | Prim (_, args, _) -> List.iter free_variable args
  | Expr flam ->
    free := Variable.Set.union
        (variables_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
           ~all_used_variables flam) !free
  end;
  !free

let free_variables ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_in_project_var tree =
  variables_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_in_project_var ~all_used_variables:false tree

let free_variables_named ?ignore_uses_in_project_var named =
  variables_usage_named ?ignore_uses_in_project_var
    ~all_used_variables:false named

let used_variables ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_in_project_var tree =
  variables_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_in_project_var ~all_used_variables:true tree

let used_variables_named ?ignore_uses_in_project_var named =
  variables_usage_named ?ignore_uses_in_project_var
    ~all_used_variables:true named

let create_let var defining_expr body : t =
  begin match !Clflags.dump_flambda_let with
  | None -> ()
  | Some stamp ->
    Variable.debug_when_stamp_matches var ~stamp ~f:(fun () ->
      Printf.eprintf "Creation of [Let] with stamp %d:\n%s\n%!"
        stamp
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
  end;
  let defining_expr, free_vars_of_defining_expr =
    match defining_expr with
    | Expr (Let { var = var1; defining_expr; body = Var var2;
          free_vars_of_defining_expr; _ }) when Variable.equal var1 var2 ->
      defining_expr, free_vars_of_defining_expr
    | _ -> defining_expr, free_variables_named defining_expr
  in
  Let {
    var;
    defining_expr;
    body;
    free_vars_of_defining_expr;
    free_vars_of_body = free_variables body;
  }

let map_defining_expr_of_let let_expr ~f =
  let defining_expr = f let_expr.defining_expr in
  if defining_expr == let_expr.defining_expr then
    Let let_expr
  else
    let free_vars_of_defining_expr =
      free_variables_named defining_expr
    in
    Let {
      var = let_expr.var;
      defining_expr;
      body = let_expr.body;
      free_vars_of_defining_expr;
      free_vars_of_body = let_expr.free_vars_of_body;
    }

let iter_lets t ~for_defining_expr ~for_last_body ~for_each_let =
  let rec loop (t : t) =
    match t with
    | Let { var; defining_expr; body; _ } ->
      for_each_let t;
      for_defining_expr var defining_expr;
      loop body
    | t ->
      for_last_body t
  in
  loop t

let map_lets t ~for_defining_expr ~for_last_body ~after_rebuild =
  let rec loop (t : t) ~rev_lets =
    match t with
    | Let { var; defining_expr; body; _ } ->
      let new_defining_expr =
        for_defining_expr var defining_expr
      in
      let original =
        if new_defining_expr == defining_expr then
          Some t
        else
          None
      in
      let rev_lets = (var, new_defining_expr, original) :: rev_lets in
      loop body ~rev_lets
    | t ->
      let last_body = for_last_body t in
      (* As soon as we see a change, we have to rebuild that [Let] and every
         outer one. *)
      let seen_change = ref (not (last_body == t)) in
      List.fold_left (fun t (var, defining_expr, original) ->
          let let_expr =
            match original with
            | Some original when not !seen_change -> original
            | Some _ | None ->
              seen_change := true;
              create_let var defining_expr t
          in
          let new_let = after_rebuild let_expr in
          if not (new_let == let_expr) then begin
            seen_change := true
          end;
          new_let)
        last_body
        rev_lets
  in
  loop t ~rev_lets:[]

(** CR-someday lwhite: Why not use two functions? *)
type maybe_named =
  | Is_expr of t
  | Is_named of named

let iter_general ~toplevel f f_named maybe_named =
  let rec aux (t : t) =
    match t with
    | Let _ ->
      iter_lets t
        ~for_defining_expr:(fun _var named -> aux_named named)
        ~for_last_body:aux
        ~for_each_let:f
    | _ ->
      f t;
      match t with
      | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable
      | Static_raise _ -> ()
      | Let _ -> assert false
      | Let_mutable { body; _ } ->
        aux body
      | Let_rec (defs, body) ->
        List.iter (fun (_,l) -> aux_named l) defs;
        aux body
      | Try_with (f1,_,f2)
      | While (f1,f2)
      | Static_catch (_,_,f1,f2) ->
        aux f1; aux f2
      | For { body; _ } -> aux body
      | If_then_else (_, f1, f2) ->
        aux f1; aux f2
      | Switch (_, sw) ->
        List.iter (fun (_,l) -> aux l) sw.consts;
        List.iter (fun (_,l) -> aux l) sw.blocks;
        Misc.may aux sw.failaction
      | String_switch (_, sw, def) ->
        List.iter (fun (_,l) -> aux l) sw;
        Misc.may aux def
  and aux_named (named : named) =
    f_named named;
    match named with
    | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
    | Read_symbol_field _
    | Project_closure _ | Project_var _ | Move_within_set_of_closures _
    | Prim _ -> ()
    | Set_of_closures ({ function_decls = funcs; free_vars = _;
          specialised_args = _}) ->
      if not toplevel then begin
        Variable.Map.iter (fun _ (decl : function_declaration) ->
            aux decl.body)
          funcs.funs
      end
    | Expr flam -> aux flam
  in
  match maybe_named with
  | Is_expr expr -> aux expr
  | Is_named named -> aux_named named

module With_free_variables = struct
  type 'a t =
    | Expr : expr * Variable.Set.t -> expr t
    | Named : named * Variable.Set.t -> named t

  let of_defining_expr_of_let let_expr =
    Named (let_expr.defining_expr, let_expr.free_vars_of_defining_expr)

  let of_body_of_let let_expr =
    Expr (let_expr.body, let_expr.free_vars_of_body)

  let of_expr expr =
    Expr (expr, free_variables expr)

  let of_named named =
    Named (named, free_variables_named named)

  let create_let_reusing_defining_expr var (t : named t) body =
    match t with
    | Named (defining_expr, free_vars_of_defining_expr) ->
      Let {
        var;
        defining_expr;
        body;
        free_vars_of_defining_expr;
        free_vars_of_body = free_variables body;
      }

  let create_let_reusing_body var defining_expr (t : expr t) =
    match t with
    | Expr (body, free_vars_of_body) ->
      Let {
        var;
        defining_expr;
        body;
        free_vars_of_defining_expr = free_variables_named defining_expr;
        free_vars_of_body;
      }

  let create_let_reusing_both var (t1 : named t) (t2 : expr t) =
    match t1, t2 with
    | Named (defining_expr, free_vars_of_defining_expr),
        Expr (body, free_vars_of_body) ->
      Let {
        var;
        defining_expr;
        body;
        free_vars_of_defining_expr;
        free_vars_of_body;
      }

  let expr (t : expr t) =
    match t with
    | Expr (expr, free_vars) -> Named (Expr expr, free_vars)

  let contents (type a) (t : a t) : a =
    match t with
    | Expr (expr, _) -> expr
    | Named (named, _) -> named

  let free_variables (type a) (t : a t) =
    match t with
    | Expr (_, free_vars) -> free_vars
    | Named (_, free_vars) -> free_vars
end

let fold_lets_option
    t ~init
    ~(for_defining_expr:('a -> Variable.t -> named -> 'a * Variable.t * named))
    ~for_last_body
    ~(filter_defining_expr:('b -> Variable.t -> named -> Variable.Set.t ->
                            'b * Variable.t * named option)) =
  let finish ~last_body ~acc ~rev_lets =
    let module W = With_free_variables in
    let acc, t =
      List.fold_left (fun (acc, t) (var, defining_expr) ->
          let free_vars_of_body = W.free_variables t in
          let acc, var, defining_expr =
            filter_defining_expr acc var defining_expr free_vars_of_body
          in
          match defining_expr with
          | None -> acc, t
          | Some defining_expr ->
            let let_expr =
              W.create_let_reusing_body var defining_expr t
            in
            acc, W.of_expr let_expr)
        (acc, W.of_expr last_body)
        rev_lets
    in
    W.contents t, acc
  in
  let rec loop (t : t) ~acc ~rev_lets =
    match t with
    | Let { var; defining_expr; body; _ } ->
      let acc, var, defining_expr =
        for_defining_expr acc var defining_expr
      in
      let rev_lets = (var, defining_expr) :: rev_lets in
      loop body ~acc ~rev_lets
    | t ->
      let last_body, acc = for_last_body acc t in
      finish ~last_body ~acc ~rev_lets
  in
  loop t ~acc:init ~rev_lets:[]

let free_symbols_helper symbols (named : named) =
  match named with
  | Symbol symbol
  | Read_symbol_field (symbol, _) -> symbols := Symbol.Set.add symbol !symbols
  | Set_of_closures set_of_closures ->
    Variable.Map.iter (fun _ (function_decl : function_declaration) ->
        symbols := Symbol.Set.union function_decl.free_symbols !symbols)
      set_of_closures.function_decls.funs
  | _ -> ()

let free_symbols expr =
  let symbols = ref Symbol.Set.empty in
  iter_general ~toplevel:true
    (fun (_ : t) -> ())
    (fun (named : named) -> free_symbols_helper symbols named)
    (Is_expr expr);
  !symbols

let free_symbols_named named =
  let symbols = ref Symbol.Set.empty in
  iter_general ~toplevel:true
    (fun (_ : t) -> ())
    (fun (named : named) -> free_symbols_helper symbols named)
    (Is_named named);
  !symbols

let free_symbols_allocated_constant_helper symbols
      (const : constant_defining_value) =
  match const with
  | Allocated_const _ -> ()
  | Block (_, fields) ->
    List.iter
      (function
        | (Symbol s : constant_defining_value_block_field) ->
          symbols := Symbol.Set.add s !symbols
        | (Const _ : constant_defining_value_block_field) -> ())
      fields
  | Set_of_closures set_of_closures ->
    symbols := Symbol.Set.union !symbols
      (free_symbols_named (Set_of_closures set_of_closures))
  | Project_closure (s, _) ->
    symbols := Symbol.Set.add s !symbols

let free_symbols_program (program : program) =
  let symbols = ref Symbol.Set.empty in
  let rec loop (program : program_body) =
    match program with
    | Let_symbol (_, const, program) ->
      free_symbols_allocated_constant_helper symbols const;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (fun (_, const) ->
          free_symbols_allocated_constant_helper symbols const)
        defs;
      loop program
    | Initialize_symbol (_, _, fields, program) ->
      List.iter (fun field ->
          symbols := Symbol.Set.union !symbols (free_symbols field))
        fields;
      loop program
    | Effect (expr, program) ->
      symbols := Symbol.Set.union !symbols (free_symbols expr);
      loop program
    | End symbol -> symbols := Symbol.Set.add symbol !symbols
  in
  (* Note that there is no need to count the [imported_symbols]. *)
  loop program.program_body;
  !symbols

let create_function_declaration ~params ~body ~stub ~dbg
      ~(inline : Lambda.inline_attribute)
      ~(specialise : Lambda.specialise_attribute) ~is_a_functor
      : function_declaration =
  begin match stub, inline with
  | true, (Never_inline | Default_inline)
  | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
  | true, (Always_inline | Unroll _) ->
    Misc.fatal_errorf
      "Stubs may not be annotated as [Always_inline] or [Unroll]: %a"
      print body
  end;
  begin match stub, specialise with
  | true, (Never_specialise | Default_specialise)
  | false, (Never_specialise | Default_specialise | Always_specialise) -> ()
  | true, Always_specialise ->
    Misc.fatal_errorf
      "Stubs may not be annotated as [Always_specialise]: %a"
      print body
  end;
  { params;
    body;
    free_variables = free_variables body;
    free_symbols = free_symbols body;
    stub;
    dbg;
    inline;
    specialise;
    is_a_functor;
  }

let create_function_declarations ~funs =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let set_of_closures_id = Set_of_closures_id.create compilation_unit in
  let set_of_closures_origin =
    Set_of_closures_origin.create set_of_closures_id
  in
  { set_of_closures_id;
    set_of_closures_origin;
    funs;
  }

let update_function_declarations function_decls ~funs =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let set_of_closures_id = Set_of_closures_id.create compilation_unit in
  let set_of_closures_origin = function_decls.set_of_closures_origin in
  { set_of_closures_id;
    set_of_closures_origin;
    funs;
  }

let import_function_declarations_for_pack function_decls
    import_set_of_closures_id import_set_of_closures_origin =
  { set_of_closures_id =
      import_set_of_closures_id function_decls.set_of_closures_id;
    set_of_closures_origin =
      import_set_of_closures_origin function_decls.set_of_closures_origin;
    funs = function_decls.funs;
  }

let create_set_of_closures ~function_decls ~free_vars ~specialised_args
      ~direct_call_surrogates =
  if !Clflags.flambda_invariant_checks then begin
    let all_fun_vars = Variable.Map.keys function_decls.funs in
    let expected_free_vars =
      Variable.Map.fold (fun _fun_var function_decl expected_free_vars ->
          let free_vars =
            Variable.Set.diff function_decl.free_variables
              (Variable.Set.union (Parameter.Set.vars function_decl.params)
                all_fun_vars)
          in
          Variable.Set.union free_vars expected_free_vars)
        function_decls.funs
        Variable.Set.empty
    in
    (* CR-soon pchambart: We do not seem to be able to maintain the
       invariant that if a variable is not used inside the closure, it
       is not used outside either. This would be a nice property for
       better dead code elimination during inline_and_simplify, but it
       is not obvious how to ensure that.

       This would be true when the function is known never to have
       been inlined.

       Note that something like that may maybe enforcable in
       inline_and_simplify, but there is no way to do that on other
       passes.

       mshinwell: see CR in Flambda_invariants about this too
    *)
    let free_vars_domain = Variable.Map.keys free_vars in
    if not (Variable.Set.subset expected_free_vars free_vars_domain) then begin
      Misc.fatal_errorf "create_set_of_closures: [free_vars] mapping of \
          variables bound by the closure(s) is wrong.  (Must map at least \
          %a but only maps %a.)@ \nfunction_decls:@ %a"
        Variable.Set.print expected_free_vars
        Variable.Set.print free_vars_domain
        print_function_declarations function_decls
    end;
    let all_params =
      Variable.Map.fold (fun _fun_var function_decl all_params ->
          Variable.Set.union (Parameter.Set.vars function_decl.params)
            all_params)
        function_decls.funs
        Variable.Set.empty
    in
    let spec_args_domain = Variable.Map.keys specialised_args in
    if not (Variable.Set.subset spec_args_domain all_params) then begin
      Misc.fatal_errorf "create_set_of_closures: [specialised_args] \
          maps variable(s) that are not parameters of the given function \
          declarations.  specialised_args domain=%a all_params=%a \n\
          function_decls:@ %a"
        Variable.Set.print spec_args_domain
        Variable.Set.print all_params
        print_function_declarations function_decls
    end
  end;
  { function_decls;
    free_vars;
    specialised_args;
    direct_call_surrogates;
  }

let used_params function_decl =
  Variable.Set.filter
    (fun param -> Variable.Set.mem param function_decl.free_variables)
    (Parameter.Set.vars function_decl.params)

let compare_const (c1:const) (c2:const) =
  match c1, c2 with
  | Int i1, Int i2 -> compare i1 i2
  | Char i1, Char i2 -> compare i1 i2
  | Const_pointer i1, Const_pointer i2 -> compare i1 i2
  | Int _, (Char _ | Const_pointer _) -> -1
  | (Char _ | Const_pointer _), Int _ -> 1
  | Char _, Const_pointer _ -> -1
  | Const_pointer _, Char _ -> 1

let compare_constant_defining_value_block_field
    (c1:constant_defining_value_block_field)
    (c2:constant_defining_value_block_field) =
  match c1, c2 with
  | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
  | Const c1, Const c2 -> compare_const c1 c2
  | Symbol _, Const _ -> -1
  | Const _, Symbol _ -> 1

module Constant_defining_value = struct
  type t = constant_defining_value

  include Identifiable.Make (struct
    type nonrec t = t

    let compare (t1 : t) (t2 : t) =
      match t1, t2 with
      | Allocated_const c1, Allocated_const c2 ->
        Allocated_const.compare c1 c2
      | Block (tag1, fields1), Block (tag2, fields2) ->
        let c = Tag.compare tag1 tag2 in
        if c <> 0 then c
        else
          Misc.Stdlib.List.compare compare_constant_defining_value_block_field
            fields1 fields2
      | Set_of_closures set1, Set_of_closures set2 ->
        Set_of_closures_id.compare set1.function_decls.set_of_closures_id
          set2.function_decls.set_of_closures_id
      | Project_closure (set1, closure_id1),
          Project_closure (set2, closure_id2) ->
        let c = Symbol.compare set1 set2 in
        if c <> 0 then c
        else Closure_id.compare closure_id1 closure_id2
      | Allocated_const _, Block _ -> -1
      | Allocated_const _, Set_of_closures _ -> -1
      | Allocated_const _, Project_closure _ -> -1
      | Block _, Allocated_const _ -> 1
      | Block _, Set_of_closures _ -> -1
      | Block _, Project_closure _ -> -1
      | Set_of_closures _, Allocated_const _ -> 1
      | Set_of_closures _, Block _ -> 1
      | Set_of_closures _, Project_closure _ -> -1
      | Project_closure _, Allocated_const _ -> 1
      | Project_closure _, Block _ -> 1
      | Project_closure _, Set_of_closures _ -> 1

    let equal t1 t2 =
      t1 == t2 || compare t1 t2 = 0

    let hash = Hashtbl.hash

    let print = print_constant_defining_value

    let output o v =
      output_string o (Format.asprintf "%a" print v)
  end)
end

let equal_specialised_to (spec_to1 : specialised_to)
      (spec_to2 : specialised_to) =
  Variable.equal spec_to1.var spec_to2.var
    && begin
      match spec_to1.projection, spec_to2.projection with
      | None, None -> true
      | Some _, None | None, Some _ -> false
      | Some proj1, Some proj2 -> Projection.equal proj1 proj2
    end

let compare_project_var = Projection.compare_project_var
let compare_project_closure = Projection.compare_project_closure
let compare_move_within_set_of_closures =
  Projection.compare_move_within_set_of_closures
