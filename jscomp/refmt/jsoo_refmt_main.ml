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

module Js = Jsoo_common.Js

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
  }

  let make () = {
    module_system=Js_packages_info.NodeJS;
    filename=None;
  }


  let default_filename (lang: Lang.t) = "playground." ^ (Lang.toString lang)

  let string_of_module_system m = (match m with
    | Js_packages_info.NodeJS -> "nodejs"
    | Es6 -> "es6"
    | Es6_global -> "es6_global")
end

type napkinError = {
  fullMsg: string; (* Full report string with all context *)
  text: string; (* simple explain message without any extra context *)
  loc: Location.t;
}

exception NapkinParsingErrors of napkinError list

module ErrorRet = struct
  let makeJsError ~(js_error_msg: string) ~(text: string) (loc: Location.t) =
    let (_file,line,startchar) = Location.get_pos_info loc.Location.loc_start in
    let (_file,endline,endchar) = Location.get_pos_info loc.Location.loc_end in
    Js.Unsafe.(obj
                 [|
                   "js_error_msg",
                   inject @@ Js.string js_error_msg;
                   "row"    , inject line;
                   "column" , inject startchar;
                   "endRow" , inject endline;
                   "endColumn" , inject endchar;
                   "text" , inject @@ Js.string text;
                   "type" , inject @@ Js.string "error"
                 |]
              )
  let fromLocErrors (errors: Location.error array) =
    let jsErrors = Array.map (fun (e: Location.error) -> makeJsError ~js_error_msg:e.msg ~text:e.msg e.loc) errors  in
    Js.Unsafe.(obj [|
        "errors" , inject @@ Js.array jsErrors;
        "type" , inject @@ Js.string "error"
      |])

  let fromNapkinErrors (errors: napkinError array) =
    let jsErrors = Array.map (fun (e: napkinError) -> makeJsError ~js_error_msg:e.fullMsg ~text:e.text e.loc) errors in
    Js.Unsafe.(obj [|
        "errors" , inject @@ Js.array jsErrors;
        "type" , inject @@ Js.string "error"
      |])

  let makeUnexpectedError msg =
    Js.Unsafe.(obj [|
        "js_error_msg" , inject @@ Js.string msg;
        "type" , inject @@ Js.string "unexpected_error"
      |])
end

let () =
  Bs_conditional_initial.setup_env ();
  Clflags.binary_annotations := false;
  Misc.Color.setup (Some Always);
  Lazy.force Super_main.setup

let error_of_exn e =
  (match Location.error_of_exn e with
  | Some (`Ok e) -> Some e
  | Some `Already_displayed
  | None -> None)

(* Handles parse / type check errors / unexpected errors and converts them to Js.object results *)
let handle_err e =
  (match error_of_exn e with
   | Some error ->
     Location.report_error Format.err_formatter  error;
     ErrorRet.fromLocErrors [|error|]
   | None ->
     match e with
     | NapkinParsingErrors errors ->
       ErrorRet.fromNapkinErrors(Array.of_list errors)
     | _ ->
       let msg = Printexc.to_string e in
       match e with
       | Refmt_api.Migrate_parsetree.Def.Migration_error (_,loc)
       | Refmt_api.Reason_errors.Reason_error (_,loc) ->
         let error = Location.error ~loc msg in
         ErrorRet.fromLocErrors [|error|]
       | _ -> ErrorRet.makeUnexpectedError msg)

module Converter = Refmt_api.Migrate_parsetree.Convert(Refmt_api.Migrate_parsetree.OCaml_404)(Refmt_api.Migrate_parsetree.OCaml_406)
module Converter404 = Refmt_api.Migrate_parsetree.Convert(Refmt_api.Migrate_parsetree.OCaml_406)(Refmt_api.Migrate_parsetree.OCaml_404)

let get_filename ~(lang: Lang.t) opt =
  match opt with
  | Some fname -> fname
  | None -> BundleConfig.default_filename lang

let lexbuf_from_string ~filename str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf

let reason_parse ~filename str =
  lexbuf_from_string ~filename str |> Refmt_api.Reason_toolchain.RE.implementation |> Converter.copy_structure;;

let ocaml_parse ~filename str =
  lexbuf_from_string ~filename str |> Parse.implementation

module NapkinDriver = struct
  (* For now we are basically overriding functionality from Napkin_driver *)
  open Napkin_driver

  (* adds ~src parameter *)
  let setup ~src ~filename ~forPrinter () =
    let mode = if forPrinter then Napkin_parser.Default
      else ParseForTypeChecker
    in
    Napkin_parser.make ~mode src filename

  let parse_implementation ~sourcefile ~forPrinter ~src =
    Location.input_name := sourcefile;
    let parseResult =
      let engine = setup ~filename:sourcefile ~forPrinter ~src () in
      let structure = Napkin_core.parseImplementation engine in
      let (invalid, diagnostics) = match engine.diagnostics with
        | [] as diagnostics -> (false, diagnostics)
        | _ as diagnostics -> (true, diagnostics)
      in {
        filename = engine.scanner.filename;
        source = Bytes.to_string engine.scanner.src;
        parsetree = structure;
        diagnostics;
        invalid;
        comments = List.rev engine.comments;
      }
    in
    let () = if parseResult.invalid then
        let errors = parseResult.diagnostics
                     |> List.map (fun d ->
                         let fullMsg = Napkin_diagnostics.toString d parseResult.source in
                         let text = Napkin_diagnostics.explain d in
                         let loc = {
                           Location.loc_start = Napkin_diagnostics.getStartPos d;
                           Location.loc_end = Napkin_diagnostics.getEndPos d;
                           loc_ghost = false
                         } in
                         {
                           fullMsg;
                           text;
                           loc;
                         }

                       )
                     |> List.rev
        in
        raise (NapkinParsingErrors errors)
    in
    (parseResult.parsetree, parseResult.comments)
    [@@raises Location.Error]
end

let napkin_parse ~filename src =
  let (structure, _ ) = NapkinDriver.parse_implementation ~forPrinter:true ~sourcefile:filename ~src
  in
  structure

let implementation ~(config: BundleConfig.t) ~lang str  : Js.Unsafe.obj =
  let {BundleConfig.module_system} = config in
  let filename = get_filename ~lang config.filename in
  let modulename = "Playground" in
  let impl = match lang with
    | Lang.OCaml -> ocaml_parse ~filename
    | Reason -> reason_parse ~filename
    | Res -> napkin_parse ~filename
  in
  (* let env = !Toploop.toplevel_env in *)
  (* Compmisc.init_path false; *)
  (* let modulename = module_of_filename ppf sourcefile outputprefix in *)
  (* Env.set_unit_name modulename; *)
  Lam_compile_env.reset () ;
  let env = Compmisc.initial_env() in (* Question ?? *)
  (* let finalenv = ref Env.empty in *)
  let types_signature = ref [] in
  try
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
      Js.Unsafe.(obj [|
          "js_code", inject @@ Js.string v;
          "type" , inject @@ Js.string "success"
        |]))
  with
  | e -> handle_err e;;


(* To add a directory to the load path *)
let dir_directory d =
  Config.load_path := d :: !Config.load_path
let () =
  dir_directory "/static"

let parse_and_print ?(filename: string option) ~(from:Lang.t) ~(to_:Lang.t) (src: string) =
  let open Lang in
  let filename = get_filename ~lang:from filename in
  let handle_ret ~lang str =
    Js.Unsafe.(obj [|
        "code", inject @@ Js.string str;
        "lang", inject @@ Js.string (Lang.toString lang);
        "type" , inject @@ Js.string "success"
      |])
  in
  try
    (match (from, to_) with
     | (Reason, OCaml) ->
       src
       |> lexbuf_from_string ~filename
       |> Refmt_api.Reason_toolchain.RE.implementation_with_comments
       |> Refmt_api.Reason_toolchain.ML.print_implementation_with_comments Format.str_formatter;
       handle_ret ~lang:OCaml (Format.flush_str_formatter ())
     | (OCaml, Reason) ->
       src
       |> lexbuf_from_string ~filename
       |> Refmt_api.Reason_toolchain.ML.implementation_with_comments
       |> Refmt_api.Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter;
       handle_ret ~lang:Reason (Format.flush_str_formatter ())
     | (Reason, Res) ->
       let ast = 
         src
         |> lexbuf_from_string ~filename
         |> Refmt_api.Reason_toolchain.RE.implementation
         |> Converter.copy_structure
       in
       let structure = ast
                       |> Napkin_ast_conversion.normalizeReasonArityStructure ~forPrinter:true
                       |> Napkin_ast_conversion.structure
       in
       handle_ret ~lang:Res (Napkin_printer.printImplementation ~width:80 structure ~comments:[])
     | (Res, Reason) ->
       let (structure, _) =
         NapkinDriver.parse_implementation ~forPrinter:true ~sourcefile:filename ~src
       in
       let sanitized = structure
                       |> Napkin_ast_conversion.normalizeReasonArityStructure ~forPrinter:true
                       |> Napkin_ast_conversion.structure
       in
       Refmt_api.Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter (Converter404.copy_structure sanitized, []);
       handle_ret ~lang:Reason (Format.flush_str_formatter ())
     | (OCaml, Res) ->
       let structure = 
         src
         |> lexbuf_from_string ~filename
         |> Parse.implementation
       in
       handle_ret ~lang:Res (Napkin_printer.printImplementation ~width:80 structure ~comments:[])
     | (Res, OCaml) ->
       let (structure, _) =
         NapkinDriver.parse_implementation ~forPrinter:true ~sourcefile:filename ~src
       in
       Pprintast.structure Format.str_formatter structure;
       handle_ret ~lang:OCaml (Format.flush_str_formatter ())
     | (Res, Res) ->
       (* Basically pretty printing *)
       let (structure, comments) =
         NapkinDriver.parse_implementation ~forPrinter:true ~sourcefile:filename ~src
       in
       handle_ret ~lang:Res (Napkin_printer.printImplementation ~width:80 structure ~comments)
     | (OCaml, OCaml) ->
       handle_ret ~lang:OCaml src
     | (Reason, Reason) ->
       let astAndComments = src
                            |> lexbuf_from_string ~filename
                            |> Refmt_api.Reason_toolchain.RE.implementation_with_comments
       in
       Refmt_api.Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter astAndComments;
       handle_ret ~lang:Reason (Format.flush_str_formatter ())
    )
  with
  | e -> handle_err e


module Export = struct
  let make_compiler ~config ~lang =
    let open Lang in
    let open Js.Unsafe in
    let baseAttrs = 
      [|"compile",
        inject @@
        Js.wrap_meth_callback
          (fun _ code ->
             (implementation ~config ~lang (Js.to_string code)));
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
          ("pretty_print",
           inject @@
           Js.wrap_meth_callback
             (fun _ code ->
                (match lang with
                 | OCaml -> ErrorRet.makeUnexpectedError ("OCaml pretty printing not supported")
                 | _ -> parse_and_print ?filename:config.filename ~from:lang ~to_:lang (Js.to_string code))))
        |]
      else
        baseAttrs
    in
    obj attrs

  let make_settings ~(config: BundleConfig.t) = 
    let open Lang in
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
    (Js.Unsafe.(obj
                  [|
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
                    "list",
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
                                       |]))
                      );

                  |]))

  let convert ~(config: BundleConfig.t) = 
    let open Lang in 
    (Js.Unsafe.(obj
                  [|"reason_to_ocaml",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (parse_and_print ?filename:config.filename ~from:Reason ~to_:OCaml (Js.to_string code)));
                    "ocaml_to_reason",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (parse_and_print ?filename:config.filename ~from:OCaml ~to_:Reason (Js.to_string code)));
                    "reason_to_res",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (parse_and_print ?filename:config.filename ~from:Reason ~to_:Res (Js.to_string code)));
                    "res_to_reason",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (parse_and_print ?filename:config.filename ~from:Res ~to_:Reason (Js.to_string code)));
                    "res_to_ocaml",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (parse_and_print ?filename:config.filename ~from:Res ~to_:OCaml (Js.to_string code)));
                    "ocaml_to_res",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (parse_and_print ?filename:config.filename ~from:OCaml ~to_:Res (Js.to_string code)));
                  |]))

  let make () =
    let config = BundleConfig.make () in
    (Js.Unsafe.(obj
                  [|
                    "version",
                    inject @@ Js.string Bs_version.version;
                    "ocaml",
                    inject @@ make_compiler ~config ~lang:OCaml;
                    "reason",
                    inject @@ make_compiler ~config ~lang:Reason;
                    "napkin",
                    inject @@ make_compiler ~config ~lang:Res;
                    "convert",
                    inject @@ convert ~config;
                    "settings",
                    inject @@ make_settings ~config
                  |]))

end

let () =
  let open Lang in
  export "bs_platform"
    (Js.Unsafe.(obj
                  [|
                    "make",
                    inject @@ Export.make
                  |]))

 

(* local variables: *)
(* compile-command: "ocamlbuild -use-ocamlfind -pkg compiler-libs -no-hygiene driver.cmo" *)
(* end: *)

