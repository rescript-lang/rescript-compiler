(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



let fprintf = Format.fprintf

let tool_name = "ocamlscript"

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = Compenv.module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  try
    let (typedtree, coercion, finalenv, current_signature) =
      Pparse.parse_implementation ~tool_name ppf sourcefile
      |> print_if ppf Clflags.dump_parsetree Printast.implementation
      |> print_if ppf Clflags.dump_source Pprintast.structure
      |> Typemod.type_implementation_more sourcefile outputprefix modulename env 
      |> print_if ppf Clflags.dump_typedtree
          (fun fmt (ty,co,_,_) -> Printtyped.implementation_with_coercion fmt  (ty,co))
    in
    if !Clflags.print_types then begin
      Warnings.check_fatal ();
    end else begin
      (typedtree, coercion)
      |> Translmod.transl_implementation modulename
      |> print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      |> (fun lambda -> 
          match           
          Lam_compile_group.lambda_as_module
            finalenv current_signature 
            sourcefile  lambda with
          | e -> e 
          | exception e -> 
            (* Save to a file instead so that it will not scare user *)            
            let file = "osc.dump" in
            Ext_pervasives.with_file_as_chan file
              (fun ch -> output_string ch @@             
                Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()));
            Ext_log.err __LOC__ "Compilation fatal error, stacktrace saved into %s" file ;             
            raise e             
        );
    end;
    Stypes.dump (Some (outputprefix ^ ".annot"));
  with x ->
    Stypes.dump (Some (outputprefix ^ ".annot"));
    raise x
