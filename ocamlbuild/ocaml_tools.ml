(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std
open Pathname.Operators
open Tags.Operators
open Tools
open Command
open Ocaml_utils

let add_suffix s = List.map (fun x -> x -.- s) ;;

let ocamldep_command' tags =
  let tags' = tags++"ocaml"++"ocamldep" in
    S [!Options.ocamldep; T tags'; ocaml_ppflags (tags++"pp:dep"); A "-modules"]

let menhir_ocamldep_command' tags ~menhir_spec out =
  let menhir = if !Options.ocamlyacc = N then V"MENHIR" else !Options.ocamlyacc in
  Cmd(S[menhir; T tags; A"--raw-depend";
        A"--ocamldep"; Quote (ocamldep_command' Tags.empty);
        menhir_spec ; Sh ">"; Px out])

let menhir_ocamldep_command arg out env _build =
  let arg = env arg and out = env out in
  let tags = tags_of_pathname arg++"ocaml"++"menhir_ocamldep" in
  menhir_ocamldep_command' tags ~menhir_spec:(P arg) out

let import_mlypack build mlypack =
  let tags1 = tags_of_pathname mlypack in
  let files = string_list_of_file mlypack in
  let include_dirs = Pathname.include_dirs_of (Pathname.dirname mlypack) in
  let files_alternatives =
    List.map begin fun module_name ->
      expand_module include_dirs module_name ["mly"]
    end files
  in
  let files = List.map Outcome.good (build files_alternatives) in
  let tags2 =
    List.fold_right
      (fun file -> Tags.union (tags_of_pathname file))
      files tags1
  in
  (tags2, files)

let menhir_modular_ocamldep_command mlypack out env build =
  let mlypack = env mlypack and out = env out in
  let (tags,files) = import_mlypack build mlypack in
  let tags = tags++"ocaml"++"menhir_ocamldep" in
  let menhir_base = Pathname.remove_extensions mlypack in
  let menhir_spec = S[A "--base" ; P menhir_base ; atomize_paths files] in
  menhir_ocamldep_command' tags ~menhir_spec out

let menhir_modular menhir_base mlypack mlypack_depends env build =
  let menhir = if !Options.ocamlyacc = N then V"MENHIR" else !Options.ocamlyacc in
  let menhir_base = env menhir_base in
  let mlypack = env mlypack in
  let mlypack_depends = env mlypack_depends in
  let (tags,files) = import_mlypack build mlypack in
  let () = List.iter Outcome.ignore_good (build [[mlypack_depends]]) in
  Ocaml_compiler.prepare_compile build mlypack;
  let ocamlc_tags = tags++"ocaml"++"byte"++"compile" in
  let tags = tags++"ocaml"++"parser"++"menhir" in
  Cmd(S[menhir ;
        A "--ocamlc"; Quote(S[!Options.ocamlc; T ocamlc_tags; ocaml_include_flags mlypack]);
        T tags ; A "--base" ; Px menhir_base ; atomize_paths files])

let ocamldep_command arg out env _build =
  let arg = env arg and out = env out in
  let tags = tags_of_pathname arg in
  Cmd(S[ocamldep_command' tags; P arg; Sh ">"; Px out])

let ocamlyacc mly env _build =
  let mly = env mly in
  let ocamlyacc = if !Options.ocamlyacc = N then V"OCAMLYACC" else !Options.ocamlyacc in
  Cmd(S[ocamlyacc; T(tags_of_pathname mly++"ocaml"++"parser"++"ocamlyacc"); Px mly])

let ocamllex mll env _build =
  let mll = env mll in
  Cmd(S[!Options.ocamllex; T(tags_of_pathname mll++"ocaml"++"lexer"++"ocamllex"); Px mll])

let infer_interface ml mli env build =
  let ml = env ml and mli = env mli in
  let tags = tags_of_pathname ml++"ocaml" in
  Ocaml_compiler.prepare_compile build ml;
  Cmd(S[!Options.ocamlc; ocaml_ppflags tags; ocaml_include_flags ml; A"-i";
        (if Tags.mem "thread" tags then A"-thread" else N);
        T(tags++"infer_interface"); P ml; Sh">"; Px mli])

let menhir mly env build =
  let mly = env mly in
  let ml = Pathname.update_extension "ml" mly in
  let menhir = if !Options.ocamlyacc = N then V"MENHIR" else !Options.ocamlyacc in
  let ocamlc_tags = tags_of_pathname ml ++"ocaml"++"byte"++"compile" in
  let menhir_tags = tags_of_pathname mly ++"ocaml"++"parser"++"menhir" in
  Ocaml_compiler.prepare_compile build mly;
  Cmd(S[menhir;
        A"--ocamlc"; Quote(S[!Options.ocamlc; T ocamlc_tags; ocaml_include_flags mly]);
        T menhir_tags; Px mly])

let ocamldoc_c tags arg odoc =
  let tags = tags++"ocaml" in
  Cmd (S [!Options.ocamldoc; A"-dump"; Px odoc; T(tags++"doc");
          ocaml_ppflags (tags++"pp:doc");
          ocaml_include_flags arg; P arg])

let ocamldoc_l_dir tags deps _docout docdir =
  Seq[Cmd (S[A"rm"; A"-rf"; Px docdir]);
      Cmd (S[A"mkdir"; A"-p"; Px docdir]);
      Cmd (S [!Options.ocamldoc;
              S(List.map (fun a -> S[A"-load"; P a]) deps);
              T(tags++"doc"++"docdir"); A"-d"; Px docdir])]

let ocamldoc_l_file tags deps docout _docdir =
  Seq[Cmd (S[A"rm"; A"-rf"; Px docout]);
      Cmd (S[A"mkdir"; A"-p"; Px (Pathname.dirname docout)]);
      Cmd (S [!Options.ocamldoc;
              S(List.map (fun a -> S[A"-load"; P a]) deps);
              T(tags++"doc"++"docfile"); A"-o"; Px docout])]

let document_ocaml_interf mli odoc env build =
  let mli = env mli and odoc = env odoc in
  Ocaml_compiler.prepare_compile build mli;
  ocamldoc_c (tags_of_pathname mli++"interf") mli odoc

let document_ocaml_implem ml odoc env build =
  let ml = env ml and odoc = env odoc in
  Ocaml_compiler.prepare_compile build ml;
  ocamldoc_c (tags_of_pathname ml++"implem") ml odoc

let document_ocaml_project ?(ocamldoc=ocamldoc_l_file) odocl docout docdir env build =
  let odocl = env odocl and docout = env docout and docdir = env docdir in
  let contents = string_list_of_file odocl in
  let include_dirs = Pathname.include_dirs_of (Pathname.dirname odocl) in
  let to_build =
    List.map begin fun module_name ->
      expand_module include_dirs module_name ["odoc"]
    end contents in
  let module_paths = List.map Outcome.good (build to_build) in
  let tags = (Tags.union (tags_of_pathname docout) (tags_of_pathname docdir))++"ocaml" in
  ocamldoc tags module_paths docout docdir

let camlp4 ?(default=A"camlp4o") tag i o env build =
  let ml = env i and pp_ml = env o in
  let tags = tags_of_pathname ml++"ocaml"++"pp"++tag in
  let _ = Rule.build_deps_of_tags build tags in
  let pp = Command.reduce (Flags.of_tags tags) in
  let pp =
    match pp with
    | N -> default
    | _ -> pp
  in
  Cmd(S[pp; P ml; A"-printer"; A"o"; A"-o"; Px pp_ml])
