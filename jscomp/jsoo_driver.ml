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
  Js_config.set_browser ();
  Clflags.unsafe_string := false

let implementation no_export ppf  str  =
  let modulename = "Test" in
  (* let env = !Toploop.toplevel_env in *)
  (* Compmisc.init_path false; *)
  (* let modulename = module_of_filename ppf sourcefile outputprefix in *)
  (* Env.set_unit_name modulename; *)
  let env = Compmisc.initial_env() in (* Question ?? *)
  let finalenv = ref Env.empty in
  let types_signature = ref [] in
  try 
  Parse.implementation (Lexing.from_string str )
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
      let () = Js_dump.(pp_deps_program `NodeJS
                          (Lam_compile_group.compile ~filename:"" "" no_export
                             !finalenv !types_signature lam)
                          (Ext_pp.from_buffer buffer)) in
      let v = Buffer.contents buffer in 
      Format.fprintf ppf {| { "js_code" : %S }|} v )
  with 
  | e -> 
      begin match Location.error_of_exn  e with 
      | Some error -> 
          let (file,line,startchar) = Location.get_pos_info error.loc.loc_start in
          let (file,endline,endchar) = Location.get_pos_info error.loc.loc_end in
          Format.fprintf ppf 
            {| {
               "js_error_msg" : %S,
               "row"    : %d,
               "column" : %d,
               "endRow" : %d, 
               "endColumn" : %d,
               "text" : %S,
               "type" : "error"     
              }|}
            (Printf.sprintf "Line %d, %d: %s"  line startchar error.msg)
            (line - 1)  startchar (endline - 1) endchar  error.msg ;
            Location.report_error Format.std_formatter  error
      | None -> Format.fprintf ppf {| {"js_error_msg" : %S} |} (Printexc.to_string e)
      end

  

let string_of_fmt (f : Format.formatter -> 'a -> unit) v = 
  let buf = Buffer.create 37 in
  let fmt  = Format.formatter_of_buffer buf in
  let () = 
    f fmt v;
    Format.pp_print_flush fmt () in
  Buffer.contents buf 

let compile  : string -> string = string_of_fmt (implementation false)
let shake_compile : string -> string = string_of_fmt (implementation true)


(* local variables: *)
(* compile-command: "ocamlbuild -use-ocamlfind -pkg compiler-libs -no-hygiene driver.cmo" *)
(* end: *)
