(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let gen_annot = ref false
let gen_ml = ref false
let print_info_arg = ref false
let target_filename = ref None

let arg_list = [
  "-o", Arg.String (fun s -> target_filename := Some s),
    " FILE (or -) : dump to file FILE (or stdout)";
  "-annot", Arg.Set gen_annot, " : generate the corresponding .annot file";
  "-src", Arg.Set gen_ml,
    " : convert .cmt or .cmti back to source code (without comments)";
  "-info", Arg.Set print_info_arg, " : print information on the file";
  ]

let arg_usage =
  "read_cmt [OPTIONS] FILE.cmt : read FILE.cmt and print related information"

let dummy_crc = String.make 32 '-'

let print_info cmt =
  let open Cmt_format in
      Printf.printf "module name: %s\n" cmt.cmt_modname;
      begin match cmt.cmt_annots with
          Packed (_, list) ->
            Printf.printf "pack: %s\n" (String.concat " " list)
        | Implementation _ -> Printf.printf "kind: implementation\n"
        | Interface _ -> Printf.printf "kind: interface\n"
        | Partial_implementation _ ->
            Printf.printf "kind: implementation with errors\n"
        | Partial_interface _ -> Printf.printf "kind: interface with errors\n"
      end;
      Printf.printf "command: %s\n"
                    (String.concat " " (Array.to_list cmt.cmt_args));
      begin match cmt.cmt_sourcefile with
          None -> ()
        | Some name ->
          Printf.printf "sourcefile: %s\n" name;
      end;
      Printf.printf "build directory: %s\n" cmt.cmt_builddir;
      List.iter (Printf.printf "load path: %s\n%!") cmt.cmt_loadpath;
      begin
      match cmt.cmt_source_digest with
          None -> ()
        | Some digest ->
            Printf.printf "source digest: %s\n" (Digest.to_hex digest);
      end;
      begin
      match cmt.cmt_interface_digest with
          None -> ()
        | Some digest ->
            Printf.printf "interface digest: %s\n" (Digest.to_hex digest);
      end;
      List.iter (fun (name, crco) ->
        let crc =
          match crco with
            None -> dummy_crc
          | Some crc -> Digest.to_hex crc
        in
          Printf.printf "import: %s %s\n" name crc;
      ) (List.sort compare cmt.cmt_imports);
      Printf.printf "%!";
      ()

let _ =
  Clflags.annotations := true;

  Arg.parse arg_list  (fun filename ->
    if
      Filename.check_suffix filename ".cmt" ||
        Filename.check_suffix filename ".cmti"
    then begin
      (*      init_path(); *)
      let cmt = Cmt_format.read_cmt filename in
      if !gen_annot then Cmt2annot.gen_annot !target_filename filename cmt;
      if !gen_ml then Cmt2annot.gen_ml !target_filename filename cmt;
      if !print_info_arg || not (!gen_ml || !gen_annot) then print_info cmt;
    end else begin
      Printf.fprintf stderr
                     "Error: the file's extension must be .cmt or .cmti.\n%!";
      Arg.usage arg_list arg_usage
    end
  ) arg_usage
