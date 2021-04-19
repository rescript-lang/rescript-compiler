(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(**
`jsoo_refmt_main` is the JSOO compilation entry point for building BuckleScript + Refmt + Res syntax as one bundle.
This is usually the file you want to build for the full playground experience.
*)


(*
 * The API version is giving information about the feature set
 * of the resulting BS bundle API.
 *
 * It follows the semver format {major.minor} which means:
 * - Whenever there is a breaking change, raise the major version
 * - Whenever there is a feature addition, raise the minor version
 *
 * Whenever you are changing functionality in here, please double check
 * if you are breaking any APIs. If yes, make sure to update this apiVersion
 * value accordingly.
 *
 * Reason:
 * We ship BuckleScript bindings that bind to this API. To be able to handle
 * different bundles with different API versions, we need a way to tell the
 * consumer on what interface the bundle provides.
 *
 * This will allow the frontend to have different sets of the same bindings,
 * and use the proper interfaces as stated by the apiVersion.
 * *)
let apiVersion = "1.1"

module Js = Jsoo_common.Js
module Sys_js = Jsoo_common.Sys_js

let export (field : string) v =
  Js.Unsafe.set (Js.Unsafe.global) field v
;;

module Lang = struct
  type t = OCaml | Reason | Res

  let fromString t = match t with
    | "ocaml" | "ml" -> Some OCaml
    | "reason" | "re" -> Some Reason
    | "res" -> Some Res
    | _ -> None

  let toString t = match t with
    | OCaml -> "ml"
    | Reason -> "re"
    | Res -> "res"
end

module BundleConfig = struct
  type t = {
    mutable module_system: Js_packages_info.module_system;
    mutable filename: string option;
    mutable warn_flags: string;
  }

  let make () = {
    module_system=Js_packages_info.NodeJS;
    filename=None;
    warn_flags=Bsc_warnings.defaults_w;
  }


  let default_filename (lang: Lang.t) = "playground." ^ (Lang.toString lang)

  let string_of_module_system m = (match m with
    | Js_packages_info.NodeJS -> "nodejs"
    | Es6 -> "es6"
    | Es6_global -> "es6_global")
end

type locErrInfo = {
  fullMsg: string; (* Full report string with all context *)
  shortMsg: string; (* simple explain message without any extra context *)
  loc: Location.t;
}

module LocWarnInfo = struct
  type t = {
    fullMsg: string; (* Full super_error related warn string *)
    shortMsg: string; (* Plain warn message without any context *)
    warnNumber: int;
    isError: bool;
    loc: Location.t;
  }
end


exception RescriptParsingErrors of locErrInfo list

module ErrorRet = struct
  type err =
    | SyntaxErr of locErrInfo array
    | TypecheckErr of locErrInfo array
    | WarningFlagErr of string * string (* warning, warning_error flags *)
    | WarningErrs of LocWarnInfo.t array
    | UnexpectedErr of string

  let locErrorAttributes ~(type_: string) ~(fullMsg: string) ~(shortMsg: string) (loc: Location.t) =
    let (_file,line,startchar) = Location.get_pos_info loc.Location.loc_start in
    let (_file,endline,endchar) = Location.get_pos_info loc.Location.loc_end in
    Js.Unsafe.([|
      "fullMsg", inject @@ Js.string fullMsg;
      "row"    , inject line;
      "column" , inject startchar;
      "endRow" , inject endline;
      "endColumn" , inject endchar;
      "shortMsg" , inject @@ Js.string shortMsg;
      "type" , inject @@ Js.string type_;
    |])

  let makeWarning (e: LocWarnInfo.t) =
    let locAttrs = locErrorAttributes
        ~type_:"warning"
        ~fullMsg: e.fullMsg
        ~shortMsg: e.shortMsg
        e.loc in
    let warnAttrs =  Js.Unsafe.([|
        "warnNumber", inject @@ (e.warnNumber |> float_of_int |> Js.number_of_float);
        "isError", inject @@ Js.bool e.isError;
      |]) in
    let attrs = Array.append locAttrs warnAttrs in
    Js.Unsafe.obj attrs

  let fromLocErrors ?(warnings: LocWarnInfo.t array option) ~(type_: string) (errors: locErrInfo array) =
    let jsErrors = Array.map
        (fun (e: locErrInfo) ->
           Js.Unsafe.(obj
                        (locErrorAttributes
                           ~type_
                           ~fullMsg: e.fullMsg
                           ~shortMsg: e.shortMsg
                           e.loc))) errors
    in
    let locErrAttrs = Js.Unsafe.([|
        "errors" , inject @@ Js.array jsErrors;
        "type" , inject @@ Js.string type_
      |])
    in
    let warningAttr = match warnings with
      | Some warnings -> Js.Unsafe.([|
          "warnings",
          inject @@ Js.array (Array.map makeWarning warnings)
        |])
      | None -> [||]
    in
    let attrs = Array.append locErrAttrs warningAttr in
    Js.Unsafe.(obj attrs)

  let fromSyntaxErrors (errors: locErrInfo array) =
    fromLocErrors ~type_:"syntax_error" errors

  (* for raised errors caused by malformed warning / warning_error flags *)
  let makeWarningFlagError ~(warn_flags: string) (msg: string)  =
    Js.Unsafe.(obj [|
        "msg" , inject @@ Js.string msg;
        "warn_flags", inject @@ Js.string warn_flags;
        "type" , inject @@ Js.string "warning_flag_error"
      |])

  let makeWarningError (errors: LocWarnInfo.t array) =
    let type_ = "warning_error" in
    let jsErrors = Array.map makeWarning errors in
    Js.Unsafe.(obj [|
        "errors" , inject @@ Js.array jsErrors;
        "type" , inject @@ Js.string type_
      |])

  let makeUnexpectedError msg =
    Js.Unsafe.(obj [|
        "msg" , inject @@ Js.string msg;
        "type" , inject @@ Js.string "unexpected_error"
      |])

end

(* One time setup for all relevant modules *)
let () =
  Bs_conditional_initial.setup_env ();
  Clflags.binary_annotations := false;
  Misc.Color.setup (Some Always);
  Lazy.force Super_main.setup;
  Lazy.force Res_outcome_printer.setup

let error_of_exn e =
  (match Location.error_of_exn e with
  | Some (`Ok e) -> Some e
  | Some `Already_displayed
  | None -> None)

module Converter = Refmt_api.Migrate_parsetree.Convert(Refmt_api.Migrate_parsetree.OCaml_404)(Refmt_api.Migrate_parsetree.OCaml_406)
module Converter404 = Refmt_api.Migrate_parsetree.Convert(Refmt_api.Migrate_parsetree.OCaml_406)(Refmt_api.Migrate_parsetree.OCaml_404)

(* Returns a default filename in case given value opt is not set *)
let get_filename ~(lang: Lang.t) opt =
  match opt with
  | Some fname -> fname
  | None -> BundleConfig.default_filename lang

let lexbuf_from_string ~filename str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf

(* We need this for a specific parsing issue in Reason: Whenever you are
 * parsing a source where the last line is a comment (and not a \n) the parser
 * enters an infinite loop. To prevent this, we need to make sure to append a
 * newline before doing any parsing attempt *)
let maybe_add_newline str =
  let last = (String.length str) - 1 in
  match String.get str last with
  | '\n' -> str
  | _ -> str ^ "\n"

let reason_parse ~filename str =
  str
  |> maybe_add_newline
  |> lexbuf_from_string ~filename
  |> Refmt_api.Reason_toolchain.RE.implementation
  |> Converter.copy_structure

let ocaml_parse ~filename str =
  lexbuf_from_string ~filename str |> Parse.implementation

module ResDriver = struct
  (* For now we are basically overriding functionality from Res_driver *)
  open Res_driver

  (* adds ~src parameter *)
  let setup ~src ~filename ~forPrinter () =
    let mode = if forPrinter
      then Res_parser.Default
      else ParseForTypeChecker
    in
    Res_parser.make ~mode src filename

  (* get full super error message *)
  let diagnosticToString ~src (d: Res_diagnostics.t) =
    let startPos = Res_diagnostics.getStartPos(d) in
    let endPos = Res_diagnostics.getEndPos(d) in
    let msg = Res_diagnostics.explain(d) in
    let loc = {loc_start = startPos; Location.loc_end=endPos; loc_ghost=false} in
    let err = { Location.loc; msg; sub=[]; if_highlight=""} in
    Res_diagnostics_printing_utils.Super_location.super_error_reporter
      Format.str_formatter
      src
      err;
    Format.flush_str_formatter ()

  module ReasonBinary = struct
    (* copied from res_driver_reason_binary.ml bc it was not exposed in the mli *)
    let isReasonDocComment (comment: Res_comment.t) =
      let content = Res_comment.txt comment in
      let len = String.length content in
      if len = 0 then true
      else if len >= 2 && (String.unsafe_get content 0 = '*' && String.unsafe_get content 1 = '*') then false
      else if len >= 1 && (String.unsafe_get content 0 = '*') then true
      else false

    (* Originally taken and adapted from res_driver_reason_binary.ml to decouple from file access *)
    let extractConcreteSyntax ~(filename:string) (src:string) =
      let commentData = ref [] in
      let stringData = ref [] in
      let scanner = Res_scanner.make src ~filename in

      let rec next prevEndPos scanner =
        let (startPos, endPos, token) = Res_scanner.scan scanner in
        match token with
        | Eof -> ()
        | Comment c ->
          Res_comment.setPrevTokEndPos c prevEndPos;
          commentData := c::(!commentData);
          next endPos scanner
        | String _ ->
          let loc = {Location.loc_start = startPos; loc_end = endPos; loc_ghost = false} in
          let len = endPos.pos_cnum - startPos.pos_cnum in
          let txt = (String.sub [@doesNotRaise]) src startPos.pos_cnum len in
          stringData := (txt, loc)::(!stringData);
          next endPos scanner
        | _ ->
          next endPos scanner
      in
      next Lexing.dummy_pos scanner;
      let comments =
        !commentData
        |> List.filter (fun c -> not (isReasonDocComment c))
        |> List.rev
      in
      (comments, !stringData)
  end

  let parse_implementation ~sourcefile ~forPrinter ~src =
    Location.input_name := sourcefile;
    let parseResult =
      let engine = setup ~filename:sourcefile ~forPrinter ~src () in
      let structure = Res_core.parseImplementation engine in
      let (invalid, diagnostics) = match engine.diagnostics with
        | [] as diagnostics -> (false, diagnostics)
        | _ as diagnostics -> (true, diagnostics)
      in {
        filename = engine.scanner.filename;
        source = engine.scanner.src;
        parsetree = structure;
        diagnostics;
        invalid;
        comments = List.rev engine.comments;
      }
    in
    let () = if parseResult.invalid then
        let errors = parseResult.diagnostics
                     |> List.map (fun d ->
                         let fullMsg = diagnosticToString ~src:parseResult.source d in
                         let shortMsg = Res_diagnostics.explain d in
                         let loc = {
                           Location.loc_start = Res_diagnostics.getStartPos d;
                           Location.loc_end = Res_diagnostics.getEndPos d;
                           loc_ghost = false
                         } in
                         {
                           fullMsg;
                           shortMsg;
                           loc;
                         }
                       )
                     |> List.rev
        in
        raise (RescriptParsingErrors errors)
    in
    (parseResult.parsetree, parseResult.comments)
end

let rescript_parse ~filename src =
  let (structure, _ ) = ResDriver.parse_implementation ~forPrinter:false ~sourcefile:filename ~src
  in
  structure


module Printer = struct
  let printExpr typ =
    Printtyp.reset_names();
    Res_doc.toString
      ~width:60 (Res_outcome_printer.printOutTypeDoc (Printtyp.tree_of_typexp false typ))


  let printDecl ~recStatus name decl =
    Printtyp.reset_names();
    Res_doc.toString
      ~width:60
      (Res_outcome_printer.printOutSigItemDoc (Printtyp.tree_of_type_declaration (Ident.create name) decl recStatus))
end

module Compile = struct
  (* Apparently it's not possible to retrieve the loc info from
   * Location.error_of_exn properly, so we need to do some extra
   * overloading action
   * *)
  let warning_infos: LocWarnInfo.t array ref = ref [||]
  let warning_buffer = Buffer.create 512
  let warning_ppf = Format.formatter_of_buffer warning_buffer

  let flush_warning_buffer () =
    Format.pp_print_flush warning_ppf ();
    let str = Buffer.contents warning_buffer in
    Buffer.reset warning_buffer;
    str

  let super_warning_printer loc ppf w =
    match Warnings.report w with
      | `Inactive -> ()
      | `Active { Warnings. number; is_error; } ->
        Super_location.super_warning_printer loc ppf w;
        let open LocWarnInfo in
        let fullMsg = flush_warning_buffer () in
        let shortMsg = Warnings.message w in
        let info = {
          fullMsg;
          shortMsg;
          warnNumber=number;
          isError=is_error;
          loc;
        } in
        warning_infos := Array.append !warning_infos [|info|]

  let () =
    Location.formatter_for_warnings := warning_ppf;
    Location.warning_printer := super_warning_printer

  let handle_err e =
    (match error_of_exn e with
     | Some error ->
       (* This branch handles all
        * errors handled by the Location error reporting
        * system.
        *
        * Here we can differentiate between the different kinds
        * of error types just by looking at the raised exn names *)
       let type_ = match e with
         | Typetexp.Error _
         | Typecore.Error _
         | Typemod.Error _ -> "type_error"
         | Lexer.Error _
         | Syntaxerr.Error _ -> "syntax_error"
         | _ -> "other_error"
       in
       let fullMsg =
         Location.report_error Format.str_formatter error;
         Format.flush_str_formatter ()
       in
       let err = { fullMsg; shortMsg=error.msg; loc=error.loc; } in
       ErrorRet.fromLocErrors ~type_ [|err|]
     | None ->
       match e with
       | RescriptParsingErrors errors ->
         ErrorRet.fromSyntaxErrors(Array.of_list errors)
       | _ ->
         let msg = Printexc.to_string e in
         match e with
         | Warnings.Errors ->
           ErrorRet.makeWarningError !warning_infos
         | Refmt_api.Migrate_parsetree.Def.Migration_error (_,loc) ->
           let error = { fullMsg=msg; shortMsg=msg; loc; } in
           ErrorRet.fromSyntaxErrors [|error|]
         | Refmt_api.Reason_errors.Reason_error (reason_error,loc) ->
           let fullMsg =
             Refmt_api.Reason_errors.report_error Format.str_formatter ~loc reason_error;
             Format.flush_str_formatter ()
           in
           let error = { fullMsg; shortMsg=msg; loc; } in
           ErrorRet.fromSyntaxErrors [|error|]
         | _ -> ErrorRet.makeUnexpectedError msg)

  (* Responsible for resetting all compiler state as if it were a new instance *)
  let reset_compiler () =
    warning_infos := [||];
    flush_warning_buffer () |> ignore;
    Location.reset();
    Warnings.reset_fatal ();
    Env.reset_cache_toplevel ()

  (* Collects the type information from the typed_tree, so we can use that
   * data to display types on hover etc.
   *
   * Note: start / end positions
   * *)
  let collectTypeHints typed_tree =
    let open Typedtree in
    let createTypeHintObj loc kind hint =
      let open Lexing in
      let open Location in
      let (_ , startline, startcol) = Location.get_pos_info loc.loc_start in
      let (_ , endline, endcol) = Location.get_pos_info loc.loc_end in
      Js.Unsafe.(obj [|
          "start", inject @@ (obj [|
              "line", inject @@ (startline |> float_of_int |> Js.number_of_float);
              "col", inject @@ (startcol|> float_of_int |> Js.number_of_float);
            |]);
          "end", inject @@ (obj [|
              "line", inject @@ (endline |> float_of_int |> Js.number_of_float);
              "col", inject @@ (endcol |> float_of_int |> Js.number_of_float);
            |]);
          "kind", inject @@ Js.string kind;
          "hint", inject @@ Js.string hint;
        |])
    in
    let (structure, _) = typed_tree in
    let acc = ref [] in
    let module Iter = TypedtreeIter.MakeIterator (struct
        include TypedtreeIter.DefaultIteratorArgument

        let cur_rec_status = ref None

        let enter_expression expr =
          let hint = Printer.printExpr expr.exp_type in
          let obj = createTypeHintObj expr.exp_loc "expression" hint in
          acc := obj :: !acc

        let enter_binding binding =
          let hint = Printer.printExpr binding.vb_expr.exp_type in
          let obj = createTypeHintObj binding.vb_loc "binding" hint in
          acc := obj :: !acc

        let enter_core_type ct =
          let hint = Printer.printExpr ct.ctyp_type in
          let obj = createTypeHintObj ct.ctyp_loc "core_type" hint in
          acc := obj :: !acc

        let enter_type_declarations recFlag =
          let status = match recFlag with
          | Asttypes.Nonrecursive -> Types.Trec_not
          | Recursive -> Trec_first
          in
          cur_rec_status := Some status

        let enter_type_declaration tdecl =
          let open Types in
          match !cur_rec_status with
          | Some recStatus ->
            let hint = Printer.printDecl ~recStatus tdecl.typ_name.Asttypes.txt tdecl.typ_type in
            let obj = createTypeHintObj tdecl.typ_loc "type_declaration" hint in
            acc := obj :: !acc;
            (match recStatus with
              | Trec_not
              | Trec_first -> cur_rec_status := Some Trec_next
              | _ -> ())
          | None -> ()
      end)
    in
    List.iter Iter.iter_structure_item structure.str_items;
    Js.array (!acc |> Array.of_list)

  let implementation ~(config: BundleConfig.t) ~lang str  : Js.Unsafe.obj =
    let {BundleConfig.module_system; warn_flags} = config in
    try
      reset_compiler ();
      Warnings.parse_options false warn_flags;
      let filename = get_filename ~lang config.filename in
      let modulename = "Playground" in
      let impl = match lang with
        | Lang.OCaml -> ocaml_parse ~filename
        | Reason -> reason_parse ~filename
        | Res -> rescript_parse ~filename
      in
      (* let env = !Toploop.toplevel_env in *)
      (* Res_compmisc.init_path (); *)
      (* let modulename = module_of_filename ppf sourcefile outputprefix in *)
      (* Env.set_unit_name modulename; *)
      Lam_compile_env.reset () ;
      let env = Res_compmisc.initial_env () in (* Question ?? *)
      (* let finalenv = ref Env.empty in *)
      let types_signature = ref [] in
      Js_config.jsx_version := 3; (* default *)
      let ast = impl (str) in
      let ast = Ppx_entry.rewrite_implementation ast in
      let typed_tree =
        let (a,b,_,signature) = Typemod.type_implementation_more modulename modulename modulename env ast in
        (* finalenv := c ; *)
        types_signature := signature;
        (a,b) in
      typed_tree
      |>  Translmod.transl_implementation modulename
      |> (* Printlambda.lambda ppf *) (fun {Lambda.code = lam} ->
          let buffer = Buffer.create 1000 in
          let () = Js_dump_program.pp_deps_program
              ~output_prefix:"" (* does not matter here *)
              module_system
              (Lam_compile_main.compile ""
                 lam)
              (Ext_pp.from_buffer buffer) in
          let v = Buffer.contents buffer in
          let typeHints = collectTypeHints typed_tree in
          Js.Unsafe.(obj [|
              "js_code", inject @@ Js.string v;
              "warnings",
              inject @@ (
                !warning_infos
                |> Array.map ErrorRet.makeWarning
                |> Js.array
                |> inject
              );
              "type_hints", inject @@ typeHints;
              "type" , inject @@ Js.string "success"
            |]))
    with
    | e ->
      match e with
      | Arg.Bad msg ->
        ErrorRet.makeWarningFlagError ~warn_flags msg
      | _ -> handle_err e;;

  let syntax_format ?(filename: string option) ~(from:Lang.t) ~(to_:Lang.t) (src: string) =
    let open Lang in
    let src = match from with
      | Reason -> maybe_add_newline src
      | _ -> src
    in
    let filename = get_filename ~lang:from filename in
    try
      let code = match (from, to_) with
        | (Reason, OCaml) ->
          src
          |> lexbuf_from_string ~filename
          |> Refmt_api.Reason_toolchain.RE.implementation_with_comments
          |> Refmt_api.Reason_toolchain.ML.print_implementation_with_comments Format.str_formatter;
          Format.flush_str_formatter ()
        | (OCaml, Reason) ->
          src
          |> lexbuf_from_string ~filename
          |> Refmt_api.Reason_toolchain.ML.implementation_with_comments
          |> Refmt_api.Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter;
          Format.flush_str_formatter ()
        | (Reason, Res) ->
          let (comments, stringData) = ResDriver.ReasonBinary.extractConcreteSyntax ~filename src in
          let ast =
            src
            |> lexbuf_from_string ~filename
            |> Refmt_api.Reason_toolchain.RE.implementation
          in
          let structure =
            Refmt_api.Reason_syntax_util.(apply_mapper_to_structure ast remove_stylistic_attrs_mapper)
            |> Converter.copy_structure
            |> Res_ast_conversion.replaceStringLiteralStructure stringData
            |> Res_ast_conversion.normalizeReasonArityStructure ~forPrinter:true
            |> Res_ast_conversion.structure
          in
          Res_printer.printImplementation ~width:80 structure ~comments
        | (Res, Reason) ->
          let (comments, stringData) = ResDriver.ReasonBinary.extractConcreteSyntax ~filename src in
          let (structure, _) =
            ResDriver.parse_implementation ~forPrinter:false ~sourcefile:filename ~src
          in
          let sanitized = structure
                          |> Res_ast_conversion.replaceStringLiteralStructure stringData
                          |> Res_ast_conversion.normalizeReasonArityStructure ~forPrinter:false
                          |> Res_ast_conversion.structure
          in
          let reasonComments = comments|> List.map (fun comment -> 
              let open Refmt_api.Reason_comment in
              let text = Res_comment.txt comment in
              let category =
                if Res_comment.isSingleLineComment comment then
                  SingleLine
                else
                  Regular
              in
              {
                location = Res_comment.loc comment;
                category;
                text;
              }
            ) in
          Refmt_api.Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter (Converter404.copy_structure sanitized, reasonComments);
          Format.flush_str_formatter ()
        | (OCaml, Res) ->
          let structure =
            src
            |> lexbuf_from_string ~filename
            |> Parse.implementation
          in
          Res_printer.printImplementation ~width:80 structure ~comments:[]
        | (Res, OCaml) ->
          let (structure, _) =
            ResDriver.parse_implementation ~forPrinter:false ~sourcefile:filename ~src
          in
          Pprintast.structure Format.str_formatter structure;
          Format.flush_str_formatter ()
        | (Res, Res) ->
          (* Essentially pretty printing.
           * IMPORTANT: we need forPrinter:true when parsing code here,
           * otherwise we will loose some information for the ReScript printer *)
          let (structure, comments) =
            ResDriver.parse_implementation ~forPrinter:true ~sourcefile:filename ~src
          in
          Res_printer.printImplementation ~width:80 structure ~comments
        | (OCaml, OCaml) -> src
        | (Reason, Reason) ->
          (* Pretty printing *)
          let astAndComments = src
                               |> lexbuf_from_string ~filename
                               |> Refmt_api.Reason_toolchain.RE.implementation_with_comments
          in
          Refmt_api.Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter astAndComments;
          Format.flush_str_formatter ()
      in
      Js.Unsafe.(obj [|
          "code", inject @@ Js.string code;
          "fromLang", inject @@ Js.string (Lang.toString from);
          "toLang", inject @@ Js.string (Lang.toString to_);
          "type" , inject @@ Js.string "success"
        |])
    with
    | e -> handle_err e
end


(* To add a directory to the load path *)
let dir_directory d =
  Config.load_path := d :: !Config.load_path
let () =
  dir_directory "/static"

module Export = struct
  let make_compiler ~config ~lang =
    let open Lang in
    let open Js.Unsafe in
    let baseAttrs =
      [|"compile",
        inject @@
        Js.wrap_meth_callback
          (fun _ code ->
             (Compile.implementation ~config ~lang (Js.to_string code)));
        "version",
        inject @@
        Js.string
          (match lang with
           | Reason -> Refmt_api.version
           | Res -> Bs_version.version
           | OCaml -> Sys.ocaml_version);
      |] in
    let attrs =
      if lang != OCaml then
        Array.append baseAttrs [|
          ("format",
           inject @@
           Js.wrap_meth_callback
             (fun _ code ->
                (match lang with
                 | OCaml -> ErrorRet.makeUnexpectedError ("OCaml pretty printing not supported")
                 | _ -> Compile.syntax_format ?filename:config.filename ~from:lang ~to_:lang (Js.to_string code))))
        |]
      else
        baseAttrs
    in
    obj attrs

  (* Creates a "compiler instance" binding the configuration context to the specific compile / formatter functions *)
  let make () =
    let open Lang in
    let config = BundleConfig.make () in
    let set_module_system value =
      match value with
      | "es6" ->
        config.module_system <- Js_packages_info.Es6; true
      | "nodejs" ->
        config.module_system <- NodeJS; true
      | _ -> false in
    let set_filename value =
      config.filename <- Some value; true
    in
    let set_warn_flags value =
      config.warn_flags <- value; true
    in
    let convert_syntax ~(fromLang: string) ~(toLang: string) (src: string) =
      let open Lang in
      match (fromString fromLang, fromString toLang) with
      | (Some from, Some to_) ->
        Compile.syntax_format ?filename:config.filename ~from ~to_ src
      | other ->
        let msg = match other with
          | (None, None) -> "Unknown from / to language: " ^ fromLang ^ ", " ^ toLang
          | (None, Some _) -> "Unknown from language: " ^ fromLang
          | (Some _, None) -> "Unknown to language: " ^ toLang
          | (Some _, Some _) -> "Can't convert from " ^ fromLang ^ " to " ^ toLang
        in
        ErrorRet.makeUnexpectedError(msg)
    in
    Js.Unsafe.(obj [|
        "version",
        inject @@ Js.string Bs_version.version;
        "ocaml",
        inject @@ make_compiler ~config ~lang:OCaml;
        "reason",
        inject @@ make_compiler ~config ~lang:Reason;
        "rescript",
        inject @@ make_compiler ~config ~lang:Res;
        "convertSyntax",
        inject @@
        Js.wrap_meth_callback
          (fun _ fromLang toLang src ->
             (convert_syntax ~fromLang:(Js.to_string fromLang) ~toLang:(Js.to_string toLang) (Js.to_string src))
          );
        "setModuleSystem",
        inject @@
        Js.wrap_meth_callback
          (fun _ value ->
             (Js.bool (set_module_system (Js.to_string value)))
          );
        "setFilename",
        inject @@
        Js.wrap_meth_callback
          (fun _ value ->
             (Js.bool (set_filename (Js.to_string value)))
          );
        "setWarnFlags",
        inject @@
        Js.wrap_meth_callback
          (fun _ value ->
             (Js.bool (set_warn_flags (Js.to_string value)))
          );
        "getConfig",
        inject @@
        Js.wrap_meth_callback
          (fun _ ->
             (Js.Unsafe.(obj
                           [|
                             "module_system",
                             inject @@ (
                               config.module_system
                               |> BundleConfig.string_of_module_system
                               |> Js.string
                             );
                             "warn_flags",
                             inject @@ (Js.string config.warn_flags);
                           |]))
          );
      |])

end

let () =
  let open Lang in
  export "rescript_compiler"
    (Js.Unsafe.(obj
                  [|
                    "api_version",
                    inject @@ Js.string apiVersion;
                    "version",
                    inject @@ Js.string Bs_version.version;
                    "make",
                    inject @@ Export.make
                  |]))

