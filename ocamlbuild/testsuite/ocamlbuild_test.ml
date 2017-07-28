(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*                           Wojciech Meyer                            *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

external (|>) :  'a -> ('a -> 'b) -> 'b = "%revapply"

let print_list ~sep f ppf = function
| [] -> ()
| x :: [] -> f ppf x
| x :: xs -> f ppf x; List.iter (fun x -> sep ppf (); f ppf x) xs

let print_list_com f =
  print_list ~sep:(fun ppf () -> pp_print_string ppf ",") f
let print_list_blank f =
  print_list ~sep:(fun ppf () -> pp_print_string ppf " ") f

let print_string_list = print_list_com pp_print_string
let print_string_list_com = print_list_com pp_print_string
let print_string_list_blank = print_list_blank pp_print_string

let exists filename =
  try ignore(Unix.stat filename); true
  with Unix.Unix_error ((Unix.ENOENT),_,_) -> false

let execute cmd =
  let ic = Unix.open_process_in cmd and lst = ref [] in
  try while true do lst := input_line ic :: !lst done; assert false
  with End_of_file ->
    let ret_code = Unix.close_process_in ic
    in ret_code, List.rev !lst

let rm f =
  if exists f then
    ignore(Sys.command (Printf.sprintf "rm -r %s" f))

module Match = struct

  type atts = unit

  (* File consists of file attribute and name *)
  type file = atts * string

  (* Result is an outcome of execution, if consists of returned exit code,
     and stream from stdout *)
  type result = int * string

  type t =
    (* Represents file in the tree *)
    | F of file
    (* Directory, consists of name and sub entries *)
    | D of file * t list
    (* Like file, but will be executed, and the result will compared *)
    | X of file * result
    (* Symlink; currently not supported *)
 (* | L of file * file *)
    (* We request that everything below should match exactly *)
    | Exact of t
    (* Here we want just the tree contained entities but we allow some
     other stuff to be there too *)
    | Contains of t
    (* matching on Empty always fail *)
    | Empty
    (* matches the negation of its argument: fails when it succeeds
       and vice versa; Any can be expressed as (Not Empty) *)
    | Not of t

  (* Type of error, we either expect something or something is un-expected *)
  type error =
      Expected of string
    | Unexpected of string
    | Structure of string * string list
    | Output of string * string * string

  (* This will print the tree *)
  let print ppf tree =
    let rec lines ppf lst =
      List.iter (fun line -> pp_print_space ppf (); item ppf line) lst
    and item ppf = function
    | F (_, name) -> fprintf ppf "@[<h>%s@]" name
    | D ((_, name), children) ->
      fprintf ppf "@[<v 1>@[<h>%s/@]%a@]" name lines children
    | X ((_,name), _) -> fprintf ppf "@[<h>%s@]" name
 (* | L ((_,src), (_,dst)) -> fprintf ppf "@[<h>%s->%s@]@" src dst *)
    | Exact content -> fprintf ppf "{%a}" item content
    | Contains content -> fprintf ppf "<%a>" item content
    | Empty -> pp_print_char ppf '#'
    | Not t -> fprintf ppf "not(@[%a@])" item t
    in
    pp_open_vbox ppf 0;
    item ppf tree;
    pp_close_box ppf ()

  let f ?(atts=()) name = F (atts, name)
  let d ?(atts=()) name children = D ((atts, name), children)
  let lf ?(atts=()) lst = List.map (fun nm -> F (atts,nm)) lst
  let x ?(atts=()) name ~output = X ((atts,name), (0,output))

  let match_with_fs ~root m =
    let rec visit ~exact ~successes ~errors path m =
      let string_of_path path = "./" ^ String.concat "/" (List.rev path) in
      let file name = string_of_path (name :: path) in
      let push li x = li := x :: !li in
      let exists_assert filename =
        push (if exists filename then successes else errors) (Expected filename)
      in
      let rec take_name = function
        | F (_, name)
        | D ((_, name), _)
        | X ((_, name), _) -> [name]
        | Exact sub
        | Contains sub
        | Not sub -> take_name sub
        | Empty -> []
      in
      match m with
        | F ((),name) ->
          exists_assert (file name)
        | D (((),name), sub) ->
          exists_assert (file name);
          let lst = List.flatten (List.map take_name sub) in
          let lst' = Sys.readdir name |> Array.to_list in
          let lst' = List.filter (fun x -> not (List.mem x lst)) lst' in
          (if exact && lst' <> [] then
              errors := Structure ((file name), lst') :: !errors);
          List.iter (visit ~exact ~successes ~errors (name :: path)) sub
        | X (((), name), (retcode, output)) ->
          let _,output' = execute (file name) in
          let output' = String.concat "\n" output' in
          push (if output <> output' then errors else successes)
            (Output (file name, output, output'));
        | Exact sub -> visit ~exact:true ~successes ~errors path sub
        | Contains sub -> visit ~exact:false ~successes ~errors path sub
        | Empty -> push errors (Unexpected (string_of_path path))
        | Not sub -> visit ~exact ~errors:successes ~successes:errors path sub
    in
    let dir = Sys.getcwd () in
    Unix.chdir root;
    let successes = ref [] in
    let errors = ref [] in
    visit ~exact:false ~successes ~errors [] m;
    Unix.chdir dir;
    List.rev !errors

  let string_of_error = function
  | Expected s -> Printf.sprintf "expected '%s' on a file system" s
  | Unexpected s -> Printf.sprintf "un-expected '%s' on a file system" s
  | Structure (s,l) ->
    Printf.sprintf  "directory structure '%s' has un-expected files %s"
      s (String.concat ", " l)
  | Output (s, e, p) ->
    Printf.sprintf "executable %s expected output %S but got %S"
      s e p
end

module Option = struct

  type flag = string
  type path = string
  type level = int
  type package = string
  type file = string
  type command = string
  type _module = string
  type tag = string

  type t =
    [ `version
    |  `vnum
    |  `quiet
    |  `verbose of level
    |  `documentation
    |  `log of file
    |  `no_log
    |  `clean
    |  `r
    |  `I of path
    |  `Is of path list
    |  `X of path
    |  `Xs of path list
    |  `lib of flag
    |  `libs of flag list
    |  `_mod of _module
    |  `mods of _module list
    |  `pkg of package
    |  `pkgs of package list
    |  `package of package
    |  `syntax of string
    |  `lflag of flag
    |  `lflags of flag list
    |  `cflag of flag
    |  `cflags of flag list
    |  `docflag of flag
    |  `docflags of flag list
    |  `yaccflag of flag
    |  `yaccflags of flag list
    |  `lexflag of flag
    |  `lexflags of flag list
    |  `ppflag of flag
    |  `pp of flag list
    |  `tag of tag
    |  `tags of tag list
    |  `plugin_tag of tag
    |  `plugin_tags of tag list
    |  `tag_line of tag
    |  `show_tags of path
    |  `ignore of _module list
    |  `no_links
    |  `no_skip
    |  `no_hygiene
    |  `no_ocamlfind
    |  `no_plugin
    |  `no_stdlib
    |  `dont_catch_errors
    |  `just_plugin
    |  `byte_plugin
    |  `plugin_option
    |  `sanitization_script
    |  `no_sanitize
    |  `nothing_should_be_rebuilt
    |  `classic_display
    |  `use_menhir
    |  `use_jocaml
    |  `use_ocamlfind
    |  `j of level
    |  `build_dir of path
    |  `install_lib_dir of path
    |  `install_bin_dir of path
    |  `where
    |  `ocamlc of command
    |  `ocamlopt of command
    |  `ocamldep of command
    |  `ocamldoc of command
    |  `ocamlyacc of command
    |  `menhir of command
    |  `ocamllex of command
    |  `ocamlmktop of command
    |  `ocamlrun of command
    |  `help ]

  type arg = string * string list

  let print_level = pp_print_int
  let print_flag = pp_print_string
  let print_package = pp_print_string
  let print_tag = pp_print_string
  let print_tags = print_string_list_com
  let print_path = pp_print_string
  let print_paths = print_string_list_com
  let print_flags = print_string_list_com
  let print_module = pp_print_string
  let print_modules = print_string_list_com
  let print_packages = print_string_list_com
  let print_command = pp_print_string

  let print_opt ppf o =
    fprintf ppf "-";
    match o with
    | `version -> fprintf ppf "version"
    | `vnum -> fprintf ppf "vnum"
    | `quiet -> fprintf ppf "quiet"
    | `verbose level -> fprintf ppf "verbose %a" print_level level
    | `documentation -> fprintf ppf "documentation"
    | `log file -> fprintf ppf "log"
    | `no_log -> fprintf ppf "no-log"
    | `clean -> fprintf ppf "clean"
    | `r -> fprintf ppf "r"
    | `I path -> fprintf ppf "I %a" print_path path
    | `Is paths -> fprintf ppf "Is %a" print_paths paths
    | `X path -> fprintf ppf "X %a" print_path path
    | `Xs paths -> fprintf ppf "Xs %a" print_paths paths
    | `lib flag -> fprintf ppf "lib %a" print_flag flag
    | `libs flags -> fprintf ppf "libs %a" print_flags flags
    | `_mod _module -> fprintf ppf "mod %a" print_module _module
    | `mods _modules -> fprintf ppf "mods %a" print_modules _modules
    | `pkg package -> fprintf ppf "pkg %a" print_package package
    | `pkgs packages -> fprintf ppf "pkgs %a" print_packages packages
    | `package package -> fprintf ppf "package %a" print_package package
    | `syntax syntax -> fprintf ppf "syntax %a" pp_print_string syntax
    | `lflag flag -> fprintf ppf "lflag %a" print_flag flag
    | `lflags flags -> fprintf ppf "lflags %a" print_flags flags
    | `cflag flag -> fprintf ppf "cflag %a" print_flag flag
    | `cflags flags -> fprintf ppf "cflags %a" print_flags flags
    | `docflag flag -> fprintf ppf "docflag %a" print_flag flag
    | `docflags flags -> fprintf ppf "docflags %a" print_flags flags
    | `yaccflag flag -> fprintf ppf "yaccflag %a" print_flag flag
    | `yaccflags flags -> fprintf ppf "yaccflags %a" print_flags flags
    | `lexflag flag -> fprintf ppf "lexflag %a" print_flag flag
    | `lexflags flags -> fprintf ppf "lexflags %a" print_flags flags
    | `ppflag flag -> fprintf ppf "ppflag %a" print_flag flag
    | `pp flags -> fprintf ppf "pp %a" print_flags flags
    | `tag tag -> fprintf ppf "tag %a" print_tag tag
    | `tags tags -> fprintf ppf "tags %a" print_tags tags
    | `plugin_tag tag -> fprintf ppf "plugin-tag %a" print_tag tag
    | `plugin_tags tags -> fprintf ppf "plugin-tags %a" print_tags tags
    | `tag_line tag -> fprintf ppf "tag-line %a" print_tag tag
    | `show_tags path -> fprintf ppf "show-tags %a" print_path path
    | `ignore _modules -> fprintf ppf "ignore %a" print_modules _modules
    | `no_links -> fprintf ppf "no-links"
    | `no_skip -> fprintf ppf "no-skip"
    | `no_hygiene -> fprintf ppf "no-hygiene"
    | `no_ocamlfind -> fprintf ppf "no-ocamlfind"
    | `no_plugin -> fprintf ppf "no-plugin"
    | `no_stdlib -> fprintf ppf "no-stdlib"
    | `dont_catch_errors -> fprintf ppf "dont"
    | `just_plugin -> fprintf ppf "just-plugin"
    | `byte_plugin -> fprintf ppf "byte-plugin"
    | `plugin_option -> fprintf ppf "plugin-option"
    | `sanitization_script -> fprintf ppf "sanitization-script"
    | `no_sanitize -> fprintf ppf "no-sanitze"
    | `nothing_should_be_rebuilt -> fprintf ppf "nothing_should_be_rebuilt"
    | `classic_display -> fprintf ppf "classic-display"
    | `use_menhir -> fprintf ppf "use-menhir"
    | `use_jocaml -> fprintf ppf "use-jocaml"
    | `use_ocamlfind -> fprintf ppf "use-ocamlfind"
    | `j level -> fprintf ppf "j %a" print_level level
    | `build_dir path -> fprintf ppf "build-dir %a" print_path path
    | `install_lib_dir path -> fprintf ppf "install %a" print_path path
    | `install_bin_dir path -> fprintf ppf "install %a" print_path path
    | `where -> fprintf ppf "where"
    | `ocamlc command -> fprintf ppf "ocamlc %a" print_command command
    | `ocamlopt command -> fprintf ppf "ocamlopt %a" print_command command
    | `ocamldep command -> fprintf ppf "ocamldep %a" print_command command
    | `ocamldoc command -> fprintf ppf "ocamldoc %a" print_command command
    | `ocamlyacc command -> fprintf ppf "ocamlyacc %a" print_command command
    | `menhir command -> fprintf ppf "menhir %a" print_command command
    | `ocamllex command -> fprintf ppf "ocamllex %a" print_command command
    | `ocamlmktop command -> fprintf ppf "ocamlmktop %a" print_command command
    | `ocamlrun command -> fprintf ppf "ocamlrun %a" print_command command
    | `help -> fprintf ppf "help"

end

module Tree = struct

  type name = string
  type content = string

  type t =
      F of name * content
    | D of name * t list
    | E

  let f ?(content="") name = F (name, content)
  let d name children = D (name, children)

  let create_on_fs ~root f =

    let rec visit path f =
      let file name =
        List.rev (name :: path)
      |> String.concat "/"
      in
      match f with
      | F (name, content) ->
        let ch = file name |> open_out in
        output_string ch content;
        close_out ch
      | D (name, sub) ->
        (* print_endline ("mking " ^ (file name)); *)
        Unix.mkdir (file name) 0o750;
        List.iter (visit (name :: path)) sub
      | E -> ()
    in

    let dir = Sys.getcwd () in
    Unix.chdir root;
    visit [] f;
    Unix.chdir dir

end

type content = string
type filename = string
type run = filename * content

type requirements = Fullfilled | Missing of string

type test = { name     : string
            ; description : string
            ; requirements : requirements option
            ; tree     : Tree.t list
            ; matching : Match.t list
            ; options  : Option.t list
            ; targets  : string * string list
            ; pre_cmd  : string option
            ; failing_msg : string option
            ; run      : run list }

let tests = ref []

let test name
    ~description
    ?requirements
    ?(options=[]) ?(run=[]) ?pre_cmd ?failing_msg
    ?(tree=[])
    ?(matching=[])
    ~targets ()
     =
  tests := !tests @ [{ 
    name; 
    description;
    requirements;
    tree;
    matching;
    options;
    targets;
    pre_cmd;
    failing_msg;
    run;
  }]

let print_colored header_color header name body_color body =
  let color_code = function
      | `Red -> "31"
      | `Green -> "32"
      | `Yellow -> "33"
      | `Blue -> "34"
      | `Magenta -> "35"
      | `Cyan -> "36"
  in
  Printf.printf "\x1b[0;%sm\x1b[1m[%s]\x1b[0m \
                 \x1b[1m%-20s\x1b[0;%sm%s.\n\x1b[m%!"
    (color_code header_color) header name
    (color_code body_color) body

let run ~root =
  let dir = Sys.getcwd () in
  let root = dir ^ "/" ^ root in
  rm root;
  Unix.mkdir root 0o750;

  let command opts args =
    let b = Buffer.create 127 in
    let f = Format.formatter_of_buffer b in
    fprintf f "%s %a %a" ocamlbuild (print_list_blank Option.print_opt) opts (print_list_blank pp_print_string) args;
    Format.pp_print_flush f ();
    Buffer.contents b
  in

  let one_test
      { name
      ; description
      ; requirements
      ; tree
      ; matching
      ; options
      ; targets
      ; failing_msg
      ; pre_cmd
      ; run } =

    let full_name = root ^ "/" ^ name in
    rm full_name;
    Unix.mkdir full_name 0o750;
    List.iter (Tree.create_on_fs ~root:full_name) tree;
    Unix.chdir full_name;

    match requirements with
      | Some (Missing req) ->
        print_colored `Yellow "SKIPPED" name `Yellow
          (Printf.sprintf "%s is required and missing" req)

      | Some Fullfilled | None -> begin

        (match pre_cmd with
          | None -> ()
          | Some str -> ignore(Sys.command str));
        
        let log_name = full_name ^ ".log" in
        
        let cmd = command options (fst targets :: snd targets) in
        let allow_failure = failing_msg <> None in
        
        let open Unix in

        match execute cmd with
          | WEXITED n,lines
          | WSIGNALED n,lines
          | WSTOPPED n,lines when allow_failure || n <> 0 ->
            begin match failing_msg with
                          | None ->
                let ch = open_out log_name in
                List.iter
                  (fun l -> output_string ch l; output_string ch "\n")
                  lines;
                close_out ch;
                print_colored `Red "FAILED" name `Yellow
                  (Printf.sprintf "Command '%s' with error code %n \
                                   output written to %s" cmd n log_name);
              | Some failing_msg ->
                let starts_with_plus s = String.length s > 0 && s.[0] = '+' in
                let lines =
                  (* filter out -classic-display output *)
                  List.filter (fun s -> not (starts_with_plus s)) lines in
                let msg = String.concat "\n" lines in
                if failing_msg = msg then
                  print_colored `Green "PASSED" name `Cyan description
                else
                  print_colored `Red "FAILED" name `Yellow
                    ((Printf.sprintf "Failure with not matching message:\n\
                                      %s\n!=\n%s\n") msg failing_msg)
            end;
          | _ ->
            let errors =
              List.concat
                (List.map (Match.match_with_fs ~root:full_name) matching) in
            begin if errors == [] then
                print_colored `Green "PASSED" name `Cyan description
              else begin
                let ch = open_out log_name in
                output_string ch ("Run '" ^ cmd ^ "'\n");
                List.iter
                  (fun e ->
                    output_string ch (Match.string_of_error e); 
                    output_string ch ".\n")
                  errors;
                close_out ch;
                print_colored `Red "FAILED" name `Yellow
                  (Printf.sprintf "Some system checks failed, \
                                   output written to %s"
                     log_name)
              end
            end
      end

  in List.iter one_test !tests
