open! Migrate_parsetree
open! OCaml_411.Ast
open Ast_mapper
open! Ast_helper
open Asttypes
open Parsetree

open Ast_convenience_411

(** {2 Convenient stuff} *)

let with_loc f {txt ; loc = _loc} =
  (f txt) [@metaloc _loc]

(** Test if a case is a catchall. *)
let is_catchall case =
  let rec is_catchall_pat p = match p.ppat_desc with
    | Ppat_any | Ppat_var _ -> true
    | Ppat_alias (p, _) | Ppat_constraint (p,_) -> is_catchall_pat p
    | _ -> false
  in
  case.pc_guard = None && is_catchall_pat case.pc_lhs

(** Add a wildcard case in there is none. Useful for exception handlers. *)
let add_wildcard_case cases =
  let has_wildcard =
    List.exists is_catchall cases
  in
  if not has_wildcard
  then cases @ [Exp.case [%pat? exn] [%expr Lwt.fail exn]] [@metaloc Location.none]
  else cases

(** {3 Internal names} *)

let lwt_prefix = "__ppx_lwt_"

(** {2 Here we go!} *)

let sequence   = ref true
let strict_seq = ref true

let used_no_sequence_option = ref false
let used_no_strict_sequence_option = ref false

let no_sequence_option () =
  sequence := false;
  used_no_sequence_option := true

let no_strict_sequence_option () =
  strict_seq := false;
  used_no_strict_sequence_option := true

(** let%lwt related functions *)

let gen_name i = lwt_prefix ^ string_of_int i

(** [p = x] ? [__ppx_lwt_$i = x] *)
let gen_bindings l =
  let aux i binding =
    { binding with
      pvb_pat = pvar ~loc:binding.pvb_expr.pexp_loc (gen_name i)
    }
  in
  List.mapi aux l

(** [p = x] and e ? [Lwt.bind __ppx_lwt_$i (fun p -> e)] *)
let gen_binds e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
      let name = (* __ppx_lwt_$i, at the position of $x$ *)
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name i)
      in
      let fun_ =
        [%expr (fun [%p binding.pvb_pat] -> [%e aux (i+1) t])] [@metaloc e_loc]
      in
      let new_exp =
          [%expr
            let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
            Lwt.backtrace_bind
              (fun exn -> try Reraise.reraise exn with exn -> exn)
              [%e name]
              [%e fun_]
          ] [@metaloc e_loc]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in aux 0 l

(* Note: instances of [@metaloc !default_loc] below are workarounds for
    https://github.com/ocaml-ppx/ppx_tools_versioned/issues/21. *)

let lwt_sequence mapper ~exp ~lhs ~rhs ~ext_loc =
  let pat= [%pat? ()][@metaloc ext_loc] in
  let lhs, rhs =
    mapper.expr mapper lhs, mapper.expr mapper rhs
  in
    [%expr
      let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
      Lwt.backtrace_bind
        (fun exn -> try Reraise.reraise exn with exn -> exn)
        [%e lhs]
        (fun [%p pat] -> [%e rhs])
    ]
  [@metaloc exp.pexp_loc]

(** For expressions only *)
(* We only expand the first level after a %lwt.
   After that, we call the mapper to expand sub-expressions. *)
let lwt_expression mapper exp attributes ext_loc =
  default_loc := exp.pexp_loc;
  let pexp_attributes = attributes @ exp.pexp_attributes in
  match exp.pexp_desc with

  (* $e$;%lwt $e'$ ? [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_sequence (lhs, rhs) ->
    Some (lwt_sequence mapper ~exp ~lhs ~rhs ~ext_loc)
  (* [let%lwt $p$ = $e$ in $e'$] ? [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_let (Nonrecursive, vbl , e) ->
    let new_exp =
      Exp.let_
        Nonrecursive
        (gen_bindings vbl)
        (gen_binds exp.pexp_loc vbl e)
    in
    Some (mapper.expr mapper { new_exp with pexp_attributes })

  (* [match%lwt $e$ with $c$] ? [Lwt.bind $e$ (function $c$)]
     [match%lwt $e$ with exception $x$ | $c$] ?
     [Lwt.try_bind (fun () -> $e$) (function $c$) (function $x$)] *)
  | Pexp_match (e, cases) ->
    let exns, cases =
      cases |> List.partition (
        function
        | {pc_lhs = [%pat? exception [%p? _]]; _} -> true
        | _ -> false)
    in
    if cases = [] then
        raise (Location.Error (
          Location.errorf
            ~loc:exp.pexp_loc
            "match%%lwt must contain at least one non-exception pattern."
        ));
    let exns =
      exns |> List.map (
        function
        | {pc_lhs = [%pat? exception [%p? pat]]; _} as case ->
          { case with pc_lhs = pat }
        | _ -> assert false)
    in
    let exns = add_wildcard_case exns in
    let new_exp =
      match exns with
      | [] ->
        [%expr Lwt.bind [%e e] [%e Exp.function_ cases]] [@metaloc !default_loc]
      | _  ->  [%expr Lwt.try_bind (fun () -> [%e e])
                                   [%e Exp.function_ cases]
                                   [%e Exp.function_ exns]]
          [@metaloc !default_loc]
    in
    Some (mapper.expr mapper { new_exp with pexp_attributes })

  (* [assert%lwt $e$] ?
     [try Lwt.return (assert $e$) with exn -> Lwt.fail exn] *)
  | Pexp_assert e ->
    let new_exp =
      [%expr try Lwt.return (assert [%e e]) with exn -> Lwt.fail exn]
        [@metaloc !default_loc]
    in
    Some (mapper.expr mapper { new_exp with pexp_attributes })

  (* [while%lwt $cond$ do $body$ done] ?
     [let rec __ppx_lwt_loop () =
        if $cond$ then Lwt.bind $body$ __ppx_lwt_loop
        else Lwt.return_unit
      in __ppx_lwt_loop]
  *)
  | Pexp_while (cond, body) ->
    let new_exp =
      [%expr
        let rec __ppx_lwt_loop () =
          if [%e cond] then Lwt.bind [%e body] __ppx_lwt_loop
          else Lwt.return_unit
        in __ppx_lwt_loop ()
      ]
        [@metaloc !default_loc]
    in
    Some (mapper.expr mapper { new_exp with pexp_attributes })

  (* [for%lwt $p$ = $start$ (to|downto) $end$ do $body$ done] ?
     [let __ppx_lwt_bound = $end$ in
     let rec __ppx_lwt_loop $p$ =
       if $p$ COMP __ppx_lwt_bound then Lwt.return_unit
       else Lwt.bind $body$ (fun () -> __ppx_lwt_loop ($p$ OP 1))
     in __ppx_lwt_loop $start$]
  *)
  | Pexp_for ({ppat_desc = Ppat_var p_var; _} as p, start, bound, dir, body) ->
    let comp, op = match dir with
      | Upto ->   evar ">", evar "+"
      | Downto -> evar "<", evar "-"
    in
    let p' = with_loc (fun s -> evar s) p_var in

    let exp_bound = [%expr __ppx_lwt_bound] [@metaloc bound.pexp_loc] in
    let pat_bound = [%pat? __ppx_lwt_bound] [@metaloc bound.pexp_loc] in

    let new_exp =
      [%expr
        let [%p pat_bound] : int = [%e bound] in
        let rec __ppx_lwt_loop [%p p] =
          if [%e comp] [%e p'] [%e exp_bound] then Lwt.return_unit
          else Lwt.bind [%e body] (fun () -> __ppx_lwt_loop ([%e op] [%e p'] 1))
        in __ppx_lwt_loop [%e start]
      ]
        [@metaloc !default_loc]
    in
    Some (mapper.expr mapper { new_exp with pexp_attributes })


  (* [try%lwt $e$ with $c$] ?
     [Lwt.catch (fun () -> $e$) (function $c$)]
  *)
  | Pexp_try (expr, cases) ->
    let cases = add_wildcard_case cases in
    let new_exp =
        [%expr
          let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
          Lwt.backtrace_catch
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            (fun () -> [%e expr])
            [%e Exp.function_ cases]
        ]
          [@metaloc !default_loc]
    in
    Some (mapper.expr mapper { new_exp with pexp_attributes })

  (* [if%lwt $c$ then $e1$ else $e2$] ?
     [match%lwt $c$ with true -> $e1$ | false -> $e2$]
     [if%lwt $c$ then $e1$] ?
     [match%lwt $c$ with true -> $e1$ | false -> Lwt.return_unit]
  *)
  | Pexp_ifthenelse (cond, e1, e2) ->
    let e2 =
      match e2 with
      | None -> [%expr Lwt.return_unit] [@metaloc !default_loc]
      | Some e -> e
    in
    let cases =
      [
        Exp.case ([%pat? true] [@metaloc !default_loc]) e1 ;
        Exp.case ([%pat? false] [@metaloc !default_loc]) e2 ;
      ]
    in
    let new_exp =
      [%expr Lwt.bind [%e cond] [%e Exp.function_ cases]]
        [@metaloc !default_loc]
    in
    Some (mapper.expr mapper { new_exp with pexp_attributes })

  | _ ->
    None

let warned = ref false

let mapper =
  { default_mapper with

    structure = begin fun mapper structure ->
      if !warned then
        default_mapper.structure mapper structure

      else begin
        warned := true;
        let structure = default_mapper.structure mapper structure in
        let loc = Location.in_file !Location.input_name in

        let warn_if condition message structure =
          if condition then
            (Str.attribute ~loc (attribute_of_warning loc message))::structure
          else
            structure
        in

        structure
        |> warn_if (!used_no_strict_sequence_option)
          ("-no-strict-sequence is a deprecated Lwt PPX option\n" ^
           "  See https://github.com/ocsigen/lwt/issues/495")
        |> warn_if (!used_no_sequence_option)
          ("-no-sequence is a deprecated Lwt PPX option\n" ^
           "  See https://github.com/ocsigen/lwt/issues/495")
      end
    end;

    expr = (fun mapper expr ->
      match expr with
      | { pexp_desc=
            Pexp_extension (
              {txt="lwt"; loc= ext_loc},
              PStr[{pstr_desc= Pstr_eval (exp, _);_}]);
          _
        }->
        begin match lwt_expression mapper exp expr.pexp_attributes ext_loc with
        | Some expr' -> expr'
        | None -> expr
        end
      (* [($e$)[%finally $f$]] ?
         [Lwt.finalize (fun () -> $e$) (fun () -> $f$)] *)
      | [%expr [%e? exp ] [%finally     [%e? finally]] ]
      | [%expr [%e? exp ] [%lwt.finally [%e? finally]] ] ->
        let new_exp =
            [%expr
              let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
              Lwt.backtrace_finalize
                (fun exn -> try Reraise.reraise exn with exn -> exn)
                (fun () -> [%e exp])
                (fun () -> [%e finally])
            ]
              [@metaloc !default_loc]
        in
        mapper.expr mapper
          { new_exp with
            pexp_attributes = expr.pexp_attributes @ exp.pexp_attributes
          }

      | [%expr [%finally     [%e? _ ]]]
      | [%expr [%lwt.finally [%e? _ ]]] ->
        raise (Location.Error (
          Location.errorf
            ~loc:expr.pexp_loc
            "Lwt's finally should be used only with the syntax: \"(<expr>)[%%finally ...]\"."
        ))

      | _ ->
        default_mapper.expr mapper expr);
    structure_item = (fun mapper stri ->
      default_loc := stri.pstr_loc;
      match stri with
      | [%stri let%lwt [%p? var] = [%e? exp]] ->
        let warning =
          str
            ("let%lwt should not be used at the module item level.\n" ^
             "Replace let%lwt x = e by let x = Lwt_main.run (e)")
        in
        [%stri
          let [%p var] =
            (Lwt_main.run [@ocaml.ppwarning [%e warning]])
              [%e mapper.expr mapper exp]]
          [@metaloc !default_loc]

      | x -> default_mapper.structure_item mapper x);
}


let args =
  [
    "-no-sequence",
      Arg.Unit no_sequence_option,
      " has no effect (deprecated)";

    "-no-strict-sequence",
      Arg.Unit no_strict_sequence_option,
      " has no effect (deprecated)";
  ]

let () =
  Driver.register ~name:"ppx_lwt" ~args Versions.ocaml_411
    (fun _config _cookies -> mapper)
