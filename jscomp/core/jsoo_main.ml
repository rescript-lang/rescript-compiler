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

(** *)
module Js = struct
  module Unsafe = struct
    type any
    external inject : 'a -> any = "%identity"
    external get : 'a -> 'b -> 'c = "caml_js_get"
    external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
    external pure_js_expr : string -> 'a = "caml_pure_js_expr"
    let global = pure_js_expr "joo_global_object"
    type obj
    external obj : (string * any) array -> obj = "caml_js_object"
  end
  type (-'a, +'b) meth_callback
  type 'a callback = (unit, 'a) meth_callback
  external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback = "caml_js_wrap_callback"
  external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback = "caml_js_wrap_meth_callback"
  type + 'a t
  type js_string
  external string : string -> js_string t = "caml_js_from_string"
  external to_string : js_string t -> string = "caml_js_to_string"
  external create_file : js_string t -> js_string t -> unit = "caml_create_file"
  external to_bytestring : js_string t -> string = "caml_js_to_byte_string"
end


(*
 Error:
     *  {
     *    row: 12,
     *    column: 2, //can be undefined
     *    text: "Missing argument",
     *    type: "error" // or "warning" or "info"
     *  }
*)
let () =
  Clflags.bs_only := true;
  Oprint.out_ident := Outcome_printer_ns.out_ident;
  Clflags.assume_no_mli := Clflags.Mli_non_exists;
  Bs_conditional_initial.setup_env ();
  Clflags.dont_write_files := true;
  Clflags.unsafe_string := false;
  Clflags.record_event_when_debug := false

let implementation prefix impl  str  : Js.Unsafe.obj =
  let modulename = "Test" in
  (* let env = !Toploop.toplevel_env in *)
  (* Compmisc.init_path false; *)
  (* let modulename = module_of_filename ppf sourcefile outputprefix in *)
  (* Env.set_unit_name modulename; *)
  Lam_compile_env.reset () ;
  let env = Compmisc.initial_env() in (* Question ?? *)
  let finalenv = ref Env.empty in
  let types_signature = ref [] in
  try
  impl (Lexing.from_string
    (if prefix then "[@@@bs.config{no_export}]\n#1 \"repl.ml\"\n"  ^ str else str ))
  |> !Ppx_entry.rewrite_implementation
  |> (fun x ->
      let (a,b,c,signature) = Typemod.type_implementation_more modulename modulename modulename env x in
      finalenv := c ;
      types_signature := signature;
      (a,b)
     )
  |>  Translmod.transl_implementation modulename
  |> (* Printlambda.lambda ppf *) (fun lam ->
      let buffer = Buffer.create 1000 in
      let () = Js_dump_program.pp_deps_program
                          ~output_prefix:"" (* does not matter here *)
                          NodeJS
                          (Lam_compile_main.compile ~filename:"" ""
                             !finalenv !types_signature lam)
                          (Ext_pp.from_buffer buffer) in
      let v = Buffer.contents buffer in
      Js.Unsafe.(obj [| "js_code", inject @@ Js.string v |]) )
      (* Format.fprintf output_ppf {| { "js_code" : %S }|} v ) *)
  with
  | e ->
      begin match Location.error_of_exn  e with
      | Some error ->
          Location.report_error Format.std_formatter  error;
          let (file,line,startchar) = Location.get_pos_info error.loc.loc_start in
          let (file,endline,endchar) = Location.get_pos_info error.loc.loc_end in
          Js.Unsafe.(obj
          [|
            "js_error_msg",
              inject @@ Js.string (Printf.sprintf "Line %d, %d: %s"  line startchar error.msg);
               "row"    , inject (line - 1);
               "column" , inject startchar;
               "endRow" , inject (endline - 1);
               "endColumn" , inject endchar;
               "text" , inject @@ Js.string error.msg;
               "type" , inject @@ Js.string "error"
          |]
          );

      | None ->
        Js.Unsafe.(obj [|
        "js_error_msg" , inject @@ Js.string (Printexc.to_string e)
        |])

      end



let compile  impl : string -> Js.Unsafe.obj =
    implementation  false impl
(** TODO: add `[@@bs.config{no_export}]\n# 1 "repl.ml"`*)
let shake_compile impl : string -> Js.Unsafe.obj =
   implementation true impl





let load_module cmi_path cmi_content cmj_name cmj_content =
  Js.create_file cmi_path cmi_content;
  Js_cmj_datasets.data_sets :=
    String_map.add
      cmj_name (lazy (Js_cmj_format.from_string cmj_content))
      !Js_cmj_datasets.data_sets


let export (field : string) v =
  Js.Unsafe.set (Js.Unsafe.global) field v
;;

(* To add a directory to the load path *)

let dir_directory d =
  Config.load_path := d :: !Config.load_path


let () =
  dir_directory "/static/cmis"



let make_compiler name impl =
  export name
    (Js.Unsafe.(obj
                  [|"compile",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (compile impl (Js.to_string code)));
                    "shake_compile",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code ->
                         (shake_compile impl (Js.to_string code)));
                    "version", Js.Unsafe.inject (Js.string (Bs_version.version));
                    "load_module",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ cmi_path cmi_content cmj_name cmj_content ->
                        let cmj_bytestring = Js.to_bytestring cmj_content in
                        (* HACK: force string tag to ASCII (9) to avoid
                         * UTF-8 encoding *)
                        Js.Unsafe.set cmj_bytestring "t" 9;
                        load_module cmi_path cmi_content (Js.to_string cmj_name) cmj_bytestring);
                  |]))
let () = make_compiler "ocaml" Parse.implementation

(* local variables: *)
(* compile-command: "ocamlbuild -use-ocamlfind -pkg compiler-libs -no-hygiene driver.cmo" *)
(* end: *)
