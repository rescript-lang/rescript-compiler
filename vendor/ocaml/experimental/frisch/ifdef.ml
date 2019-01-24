(* This filter implements the following extensions:

   In structures:

   [%%IFDEF X]
   ...             --> included if the environment variable X is defined
   [%%ELSE]
   ...             --> included if the environment variable X is undefined
   [%%END]


   In expressions:

   [%GETENV X]    ---> the string literal representing the compile-time value
                    of environment variable X


   In variant type declarations:

   type t =
      ..
     | C [@IFDEF X] of ...   --> the constructor is kept only if X is defined


   In match clauses (function/match...with/try...with):


   P when [%IFDEF X] -> E    --> the case is kept only if X is defined

*)

open Ast_helper
open! Asttypes
open Parsetree
open Longident

let getenv loc arg =
  match arg with
  | PStr [{pstr_desc=Pstr_eval({pexp_desc = Pexp_construct ({txt = Lident sym; _}, None); _}, _); _}] ->
      (try Sys.getenv sym with Not_found -> "")
  | _ ->
      Format.eprintf "%a** IFDEF: bad syntax."
        Location.print_error loc;
      exit 2

let empty_str_item = Str.include_ (Mod.structure [])

let ifdef _args =
  let stack = ref [] in
  let eval_attributes =
    List.for_all
      (function
        | {txt="IFDEF"; loc}, arg -> getenv loc arg <> ""
        | {txt="IFNDEF"; loc}, arg -> getenv loc arg = ""
        | _ -> true)
  in
  let filter_constr cd = eval_attributes cd.pcd_attributes in
  let open Ast_mapper in
  let super = default_mapper in
  {
    super with

    type_declaration =
      (fun this td ->
         let td =
           match td with
           | {ptype_kind = Ptype_variant cstrs; _} as td ->
               {td
                with ptype_kind = Ptype_variant(List.filter filter_constr cstrs)}
           | td -> td
         in
         super.type_declaration this td
      );

    cases =
      (fun this l ->
         let l =
           List.fold_right
             (fun c rest ->
                match c with
                | {pc_guard=Some {pexp_desc=Pexp_extension({txt="IFDEF";loc}, arg); _}; _} ->
                    if getenv loc arg = "" then rest else {c with pc_guard=None} :: rest
                | c -> c :: rest
             ) l []
         in
         super.cases this l
      );

    structure_item =
      (fun this i ->
         match i.pstr_desc, !stack with
         | Pstr_extension(({txt="IFDEF";loc}, arg), _), _ ->
             stack := (getenv loc arg <> "") :: !stack;
             empty_str_item
         | Pstr_extension(({txt="ELSE";loc=_}, _), _), (hd :: tl) ->
             stack := not hd :: tl;
             empty_str_item
         | Pstr_extension(({txt="END";loc=_}, _), _), _ :: tl ->
             stack := tl;
             empty_str_item
         | Pstr_extension(({txt="ELSE"|"END";loc}, _), _), [] ->
             Format.printf "%a** IFDEF: mo matching [%%%%IFDEF]"
               Location.print_error loc;
             exit 2
         | _, (true :: _ | []) -> super.structure_item this i
         | _, false :: _ -> empty_str_item
      );

    expr =
      (fun this -> function
         | {pexp_desc = Pexp_extension({txt="GETENV";loc=l}, arg);
            pexp_loc = loc; _} ->
             Exp.constant ~loc (Const_string (getenv l arg, None))
         | x -> super.expr this x
      );
  }

let () = Ast_mapper.run_main ifdef
