(* A -ppx rewriter which evaluates expressions at compile-time,
   using the OCaml toplevel interpreter.

   The following extensions are supported:

   [%eval e] in expression context: the expression e will be evaluated
   at compile time, and the resulting value will be inserted as a
   constant literal.

   [%%eval.start] as a structure item: forthcoming structure items
   until the next [%%eval.stop] will be evaluated at compile time (the
   result is ignored) only.

   [%%eval.start both] as a structure item: forthcoming structure
   items until the next [%%eval.stop] will be evaluated at compile
   time (the result is ignored), but also kept in the compiled unit.

   [%%eval.load "..."] as a structure item: load the specified
   .cmo unit or .cma library, so that it can be used in the forthcoming
   compile-time components.
*)


module Main : sig end = struct

  open Location
  open Parsetree
  open Ast_helper
  open Outcometree
  open Ast_helper.Convenience

  let rec lid_of_out_ident = function
    | Oide_apply _ -> assert false
    | Oide_dot (x, s) -> lid_of_out_ident x ^ "." ^ s
    | Oide_ident s -> s

  let rec exp_of_out_value = function
    | Oval_string x -> str x
    | Oval_int x -> int x
    | Oval_char x -> char x
    | Oval_float x -> Ast_helper.Convenience.float x
    | Oval_list l -> list (List.map exp_of_out_value l)
    | Oval_array l -> Exp.array (List.map exp_of_out_value l)
    | Oval_constr (c, args) -> constr (lid_of_out_ident c) (List.map exp_of_out_value args)
    | Oval_record l ->
        record
          (List.map
             (fun (s, v) -> lid_of_out_ident s, exp_of_out_value v) l)
    | v ->
        Format.eprintf "[%%eval] cannot map value to expression:@.%a@."
          !Toploop.print_out_value
          v;
        exit 2

  let empty_str_item = Str.include_ (Mod.structure [])

  let run phr =
    try Toploop.execute_phrase true Format.err_formatter phr
    with exn ->
      Errors.report_error Format.err_formatter exn;
      exit 2

  let get_exp loc = function
    | PStr [ {pstr_desc=Pstr_eval (e, _); _} ] -> e
    | _ ->
        Format.eprintf "%aExpression expected@."
          Location.print_error loc;
        exit 2

  let eval _args =
    let open Ast_mapper in
    let eval_str_items = ref None in
    let super = default_mapper in
    let my_structure_item this i =
      match i.pstr_desc with
      | Pstr_extension(({txt="eval.load";loc}, e0), _) ->
          let e0 = get_exp loc e0 in
          let s =
            match get_str e0 with
            | Some s -> s
            | None ->
                Location.print_error Format.err_formatter e0.pexp_loc;
                Format.eprintf "string literal expected";
                exit 2
          in
          if not (Topdirs.load_file Format.err_formatter s) then begin
            Location.print Format.err_formatter e0.pexp_loc;
            exit 2;
          end;
          empty_str_item
      | Pstr_extension(({txt="eval.start";_},
                        PStr [{pstr_desc=Pstr_eval (e, _);_}]
                       ), _) when get_lid e = Some "both" ->
          eval_str_items := Some true;
          empty_str_item
      | Pstr_extension(({txt="eval.start";_}, PStr []), _) ->
          eval_str_items := Some false;
          empty_str_item
      | Pstr_extension(({txt="eval.stop";_}, PStr []), _) ->
          eval_str_items := None;
          empty_str_item
      | _ ->
          let s = super.structure_item this i in
          match !eval_str_items with
          | None -> s
          | Some both ->
              if not (run (Ptop_def [s])) then begin
                Location.print_error Format.err_formatter s.pstr_loc;
                Format.eprintf "this structure item raised an exception@.";
                exit 2
              end;
              if both then s else empty_str_item
    in
    let my_expr this e =
      match e.pexp_desc with
      | Pexp_extension({txt="eval";loc}, e0) ->
          let e0 = get_exp loc e0 in
          let last_result = ref None in
          let pop = !Toploop.print_out_phrase in
          Toploop.print_out_phrase := begin fun _ppf -> function
            | Ophr_eval (v, _) -> last_result := Some v
            | r ->
                Location.print_error Format.err_formatter e.pexp_loc;
                Format.eprintf "error while evaluating expression:@.%a@."
                  pop
                  r;
                exit 2
          end;
          assert (run (Ptop_def [Str.eval e0]));
          Toploop.print_out_phrase := pop;
          let v = match !last_result with None -> assert false | Some v -> v in
          with_default_loc e0.pexp_loc (fun () -> exp_of_out_value v)
      | _ ->
          super.expr this e
    in
    Toploop.initialize_toplevel_env ();
    {super with expr = my_expr; structure_item = my_structure_item}


  let () = Ast_mapper.run_main eval
end
