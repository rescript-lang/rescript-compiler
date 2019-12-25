open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype

open Format
open Printtyp

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typetexp.ml#L869 *)
let spellcheck ppf fold env lid =
  let cutoff =
    match String.length (Longident.last lid) with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target head acc =
    let (best_choice, best_dist) = acc in
    match Misc.edit_distance target head cutoff with
      | None -> (best_choice, best_dist)
      | Some dist ->
        let choice =
          if dist < best_dist then [head]
          else if dist = best_dist then head :: best_choice
          else best_choice in
        (choice, min dist best_dist)
  in
  let init = ([], max_int) in
  let handle (choice, _dist) =
    match List.rev choice with
      | [] -> ()
      | last :: rev_rest ->
        (* the modified part *)
        fprintf ppf "@[<v 2>@,@,@{<info>Hint: Did you mean %s%s%s?@}@]"
          (String.concat ", " (List.rev rev_rest))
          (if rev_rest = [] then "" else " or ")
          last
  in
  (* flush now to get the error report early, in the (unheard of) case
     where the linear search would take a bit of time; in the worst
     case, the user has seen the error, she can interrupt the process
     before the spell-checking terminates. *)
  fprintf ppf "@?";
  match lid with
    | Longident.Lapply _ -> ()
    | Longident.Lident s ->
      handle (fold (compare s) None env init)
    | Longident.Ldot (r, s) ->
      handle (fold (compare s) (Some r) env init)

let spellcheck ppf fold =
  spellcheck ppf (fun f -> fold (fun s _ _ x -> f s x))

let fold_descr fold get_name f = fold (fun descr acc -> f (get_name descr) acc)
let fold_constructors x  = fold_descr Env.fold_constructors (fun d -> d.cstr_name) x
let fold_labels x = fold_descr Env.fold_labels (fun d -> d.lbl_name) x

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typetexp.ml#L918 *)
(* modified branches are commented *)
let report_error env ppf = function
  | Typetexp.Unbound_type_constructor lid ->
    (* modified *)
    fprintf ppf "This type constructor's parameter, `%a`, can't be found. Is it a typo?" longident lid;
    spellcheck ppf Env.fold_types env lid;
  | Unbound_value lid ->
      (* modified *)
      begin
        match lid with
        | Ldot (outer, inner) ->
          fprintf ppf "The value %s can't be found in %a"
            inner
            Printtyp.longident outer;
        | other_ident -> fprintf ppf "The value %a can't be found" Printtyp.longident other_ident
      end;
      spellcheck ppf Env.fold_values env lid;
  | Unbound_module lid ->
      (* modified *)
      begin match lid with
      | Lident "Str" ->
        begin
          fprintf ppf "@[\
              @{<info>The module or file %a can't be found.@}@,@,\
              Are you trying to use the standard library's Str?@ \
              If you're compiling to JavaScript,@ use @{<info>Js.Re@} instead.@ \
              Otherwise, add str.cma to your ocamlc/ocamlopt command.\
            @]"
            longident lid
        end
      | lid ->
        begin
          fprintf ppf "@[<v>\
              @{<info>The module or file %a can't be found.@}@,\
              @[<v 2>- If it's a third-party dependency:@,\
                - Did you list it in bsconfig.json?@,\
                - @[Did you run `bsb` instead of `bsb -make-world`@ (latter builds third-parties)@]?\
              @]@,\
              - Did you include the file's directory in bsconfig.json?@]\
            @]"
            longident lid
        end
      end;
      spellcheck ppf Env.fold_modules env lid
  | Unbound_constructor lid ->
      (* modified *)
      fprintf ppf "@[<v>\
      @{<info>The variant constructor %a can't be found.@}@,@,\
      @[<v 2>- If it's defined in another module or file, bring it into scope by:@,\
        @[- Annotating it with said module name:@ @{<info>let food = MyModule.Apple@}@]@,\
        @[- Or specifying its type:@ @{<info>let food: MyModule.fruit = Apple@}@]\
      @]@,\
      - @[Constructors and modules are both capitalized.@ Did you want the latter?@ Then instead of @{<dim>let foo = Bar@}, try @{<info>module Foo = Bar@}.@]\
      @]"
      longident lid;
      Typetexp.spellcheck ppf fold_constructors env lid
  | Unbound_label lid ->
      (* modified *)
      fprintf ppf "@[<v>\
      @{<info>The record field %a can't be found.@}@,@,\
      If it's defined in another module or file, bring it into scope by:@,\
      @[- Annotating it with said module name:@ @{<info>let baby = {MyModule.age: 3}@}@]@,\
      @[- Or specifying its type:@ @{<info>let baby: MyModule.person = {age: 3}@}@]\
      @]"
      longident lid;
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
