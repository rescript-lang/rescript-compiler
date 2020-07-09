let did_you_mean ppf choices : bool = 
  (* flush now to get the error report early, in the (unheard of) case
     where the linear search would take a bit of time; in the worst
     case, the user has seen the error, she can interrupt the process
     before the spell-checking terminates. *)
  Format.fprintf ppf "@?";  
  match choices () with   
  | [] -> false
  | last :: rev_rest ->
    Format.fprintf ppf "@[<v 2>@,@,@{<info>Hint: Did you mean %s%s%s?@}@]"
      (String.concat ", " (List.rev rev_rest))
      (if rev_rest = [] then "" else " or ")
      last;
    true  


let spellcheck ppf fold env lid =
  let choices path name : string list = 
    let env : string list = fold (fun x  _ _ xs -> x ::xs ) path env []   in
    Misc.spellcheck env name in 
  match lid with
    | Longident.Lapply _ -> false
    | Longident.Lident s ->
      did_you_mean ppf (fun _ -> choices None s)
    | Longident.Ldot (r, s) ->
      did_you_mean ppf (fun _ -> choices (Some r) s)


let fold_descr fold get_name f = fold (fun descr acc -> f (get_name descr) acc)
let fold_constructors x  = fold_descr Env.fold_constructors (fun d -> d.cstr_name) x
let fold_labels x = fold_descr Env.fold_labels (fun d -> d.lbl_name) x

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typetexp.ml#L918 *)
(* modified branches are commented *)
let report_error env ppf = function
  | Typetexp.Unbound_type_constructor lid ->
    (* modified *)
    Format.fprintf ppf "@[<v>This type constructor, `%a`, can't be found.@ "  Printtyp.longident lid;
    let has_candidate = spellcheck ppf Env.fold_types env lid in
    if !Js_config.napkin && not has_candidate then 
      Format.fprintf ppf "If you wanted to write a recursive type, don't forget the `rec` in `type rec`@]"
  | Unbound_value lid ->
      (* modified *)
      begin
        match lid with
        | Ldot (outer, inner) ->
          Format.fprintf ppf "The value %s can't be found in %a"
            inner
            Printtyp.longident outer;
        | other_ident -> Format.fprintf ppf "The value %a can't be found" Printtyp.longident other_ident
      end;
      spellcheck ppf Env.fold_values env lid |> ignore
  | Unbound_module lid ->
      (* modified *)
      begin match lid with
      | Lident "Str" ->
        begin
          Format.fprintf ppf "@[\
              @{<info>The module or file %a can't be found.@}@,@,\
              Are you trying to use the standard library's Str?@ \
              If you're compiling to JavaScript,@ use @{<info>Js.Re@} instead.@ \
              Otherwise, add str.cma to your ocamlc/ocamlopt command.\
            @]"
            Printtyp.longident lid
        end
      | lid ->
        begin
          Format.fprintf ppf "@[<v>\
              @{<info>The module or file %a can't be found.@}@,\
              @[<v 2>- If it's a third-party dependency:@,\
                - Did you list it in bsconfig.json?@,\
                - @[Did you run `bsb` instead of `bsb -make-world`@ (latter builds third-parties)@]?\
              @]@,\
              - Did you include the file's directory in bsconfig.json?@]\
            @]"
            Printtyp.longident lid
        end
      end;
      spellcheck ppf Env.fold_modules env lid |> ignore
  | Unbound_constructor lid ->
      (* modified *)
      Format.fprintf ppf "@[<v>\
      @{<info>The variant constructor %a can't be found.@}@,@,\
      @[<v 2>- If it's defined in another module or file, bring it into scope by:@,\
        @[- Prefixing it with said module name:@ @{<info>TheModule.%a@}@]@,\
        @[- Or specifying its type:@ @{<info>let theValue: TheModule.theType = %a@}@]\
      @]@,\
      - @[Constructors and modules are both capitalized.@ Did you want the latter?@ Then instead of @{<dim>let foo = Bar@}, try @{<info>module Foo = Bar@}.@]\
      @]"
      Printtyp.longident lid
      Printtyp.longident lid
      Printtyp.longident lid;
      Typetexp.spellcheck ppf fold_constructors env lid
  | Unbound_label lid ->
      (* modified *)
      Format.fprintf ppf "@[<v>\
      @{<info>The record field %a can't be found.@}@,@,\
      If it's defined in another module or file, bring it into scope by:@,\
      @[- Prefixing it with said module name:@ @{<info>TheModule.%a@}@]@,\
      @[- Or specifying its type:@ @{<info>let theValue: TheModule.theType = {%a: VALUE}@}@]\
      @]"
      Printtyp.longident lid
      Printtyp.longident lid
      Printtyp.longident lid;
      Typetexp.spellcheck ppf fold_labels env lid
  | anythingElse ->
      Typetexp.report_error env ppf anythingElse

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typetexp.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      (* typetexp doesn't expose Error_forward  *)
      (* | Error_forward err ->
        Some err *)
      | _ ->
        None
    )
