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
#if BS_NATIVE then
let verbose = ref false

let warnings = ref ""

let warn_error = ref ""

let ocaml_dependencies = ref []

let add_ocaml_dependencies s = 
  ocaml_dependencies := s :: !ocaml_dependencies

let main_module = ref None

let set_main_module modulename =
  main_module := Some (Ext_string.capitalize_ascii modulename)

let includes :  _ list ref = ref []

let add_include =
  let normalize cwd s =
    Ext_path.normalize_absolute_path (Ext_path.combine cwd s) in
  fun dir ->
    includes := (normalize (Sys.getcwd ()) dir) :: !includes
#end
let compilation_kind = ref Bsb_helper_depfile_gen.Js

let hash : string ref = ref ""
let batch_files = ref []
let collect_file name =
  batch_files := name :: !batch_files

(* let output_prefix = ref None *)
let dev_group = ref 0
let namespace = ref None


let anonymous filename =
  collect_file filename
let usage = "Usage: bsb_helper.exe [options] \nOptions are:"
#if BS_NATIVE then
let link link_byte_or_native = 
  begin match !main_module with
    | None -> failwith "Linking needs a main module. Please add -bs-main MyMainModule to the invocation."
    | Some main_module ->
      Bsb_helper_linker.link 
        link_byte_or_native
        ~main_module:main_module
        (* `includes` is not reversed here because it gets reversed inside when we fold_list and 
         ~batch_files:!batch_files		           prepend a new list. *)
       ~includes:!includes
       ~batch_files:!batch_files
       ~namespace:!namespace
       ~ocaml_dependencies:(List.rev !ocaml_dependencies)
       ~warnings:!warnings
       ~warn_error:!warn_error
       ~verbose:!verbose
       ~cwd:Bsb_global_paths.cwd
  end
let pack link_byte_or_native =
   Bsb_helper_packer.pack
     link_byte_or_native
     ~includes:!includes
     ~batch_files:!batch_files
     ~namespace:!namespace
     ~warnings:!warnings
     ~warn_error:!warn_error
     ~verbose:!verbose
     ~cwd:Bsb_global_paths.cwd
#end  
let () =
  Bsb_helper_arg.parse_exn [
    "-g",  Set_int dev_group ,
    " Set the dev group (default to be 0)"
    ;
    "-bs-ns",  String (fun s -> namespace := Some s),
    " Set namespace";
    "-hash",  Set_string hash,
    " Set hash(internal)";
#if BS_NATIVE then    
    "-MD-bytecode", Unit (fun () -> 
      compilation_kind := Bsb_helper_depfile_gen.Bytecode
    ),          
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";
    "-MD-native", Unit (fun () -> 
        compilation_kind := Bsb_helper_depfile_gen.Native
           ),
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";

    (**
       The args below are used for packing/linking.

       This makes bsb_helper act as an ocaml linker where we automatically figure
       out the dependencies graph to do a topological sort before calling 
       ocamlc/ocamlopt.
    *)
    "-bs-main", ( String set_main_module),
    " set the main entry module. Only used in conjunction with -link-bytecode and -link-native";

    (* This is a way to add a directory to the search path. This is used for the 
       compiler to look for cmi files. It's also used to look for a file called `lib.cma` to 
       link with the current executable.

       For example if called like so

          bsb_helper -I theExtLib myMainFile.cmo -link-bytecode

       Then we'll go look for `theExtLib/lib.cma` to link with the final exec.
    *)
    "-I",  ( String add_include),
    " add dir to search path for the linker and packer";

    (* Both linking and packing arguments must come _after_ all of the other args and files have been listed.
       For example:

          bsb_helper -bs-main MyModule myFile.cmo myOtherFile.cmo -link-bytecode 

       In the following example, the file called `myIgnoredFile.cmo` is not linked nor is `myLibFolder/lib.cma`

          bsb_helper -bs-main MyModule myFile.cmo myOtherFile.cmo -link-bytecode -I myLibFolder myIgnoredFile.cmo

    *)
    "-link-bytecode", ( String (fun x -> link (Bsb_helper_linker.LinkBytecode x))),
    " link bytecode files into an executable";

    "-link-native", ( String (fun x -> link (Bsb_helper_linker.LinkNative x))),
    " link native files into an executable";

    "-pack-native-library", ( Unit (fun () -> 
        pack Bsb_helper_packer.PackNative
      )),
    " pack native files (cmx) into a library file (cmxa)";

    "-pack-bytecode-library", ( Unit (fun () -> 
        pack Bsb_helper_packer.PackBytecode
      )),
    " pack bytecode files (cmo) into a library file (cma)";

    "-add-ocaml-dependency", ( String add_ocaml_dependencies),
    " Add a dependency on otherlibs or compiler-libs.";

    "-w", ( String (fun w -> warnings := w )),
    " Use warnings for packer/linker.";

    "-warn-error", ( String (fun w -> warn_error := w )),
    " Turn warnings into errors for packer/linker.";

    "-verbose", ( Unit (fun v -> verbose := true)),
    " Turn on verbose Maude.";
#end    
  ] anonymous usage;
  (* arrange with mlast comes first *)
  match !batch_files with
  | [x]
    ->  Bsb_helper_depfile_gen.emit_d
          !compilation_kind
          (Bsb_dir_index.of_int !dev_group )          
          !namespace x ""
  | [y; x] (* reverse order *)
    -> 
    Bsb_helper_depfile_gen.emit_d
      !compilation_kind
      (Bsb_dir_index.of_int !dev_group)
      !namespace x y
  | _ -> 
    ()
