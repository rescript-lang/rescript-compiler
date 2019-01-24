(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Execute a list of phrases from a .ml file and compare the result to the
   expected output, written inside [%%expect ...] nodes. At the end, create
   a .corrected file containing the corrected expectations. The test is
   successful if there is no differences between the two files.

   An [%%expect] node always contains both the expected outcome with and
   without -principal. When the two differ the expectation is written as
   follows:

   {[
     [%%expect {|
     output without -principal
     |}, Principal{|
     output with -principal
     |}]
   ]}
*)

[@@@ocaml.warning "-40"]

open StdLabels

(* representation of: {tag|str|tag} *)
type string_constant =
  { str : string
  ; tag : string
  }

type expectation =
  { extid_loc   : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc : Location.t (* Location of the whole payload *)
  ; normal      : string_constant (* expectation without -principal *)
  ; principal   : string_constant (* expectation with -principal *)
  }

(* A list of phrases with the expected toplevel output *)
type chunk =
  { phrases     : Parsetree.toplevel_phrase list
  ; expectation : expectation
  }

type correction =
  { corrected_expectations : expectation list
  ; trailing_output        : string
  }

let match_expect_extension (ext : Parsetree.extension) =
  match ext with
  | ({Asttypes.txt="expect"|"ocaml.expect"; loc = extid_loc}, payload) ->
    let invalid_payload () =
      Location.raise_errorf ~loc:extid_loc
        "invalid [%%%%expect payload]"
    in
    let string_constant (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_constant (Pconst_string (str, Some tag)) ->
        { str; tag }
      | _ -> invalid_payload ()
    in
    let expectation =
      match payload with
      | PStr [{ pstr_desc = Pstr_eval (e, []) }] ->
        let normal, principal =
          match e.pexp_desc with
          | Pexp_tuple
              [ a
              ; { pexp_desc = Pexp_construct
                                ({ txt = Lident "Principal"; _ }, Some b) }
              ] ->
            (string_constant a, string_constant b)
          | _ -> let s = string_constant e in (s, s)
        in
        { extid_loc
        ; payload_loc = e.pexp_loc
        ; normal
        ; principal
        }
      | PStr [] ->
        let s = { tag = ""; str = "" } in
        { extid_loc
        ; payload_loc  = { extid_loc with loc_start = extid_loc.loc_end }
        ; normal    = s
        ; principal = s
        }
      | _ -> invalid_payload ()
    in
    Some expectation
  | _ ->
    None

(* Split a list of phrases from a .ml file *)
let split_chunks phrases =
  let rec loop (phrases : Parsetree.toplevel_phrase list) code_acc acc =
    match phrases with
    | [] ->
      if code_acc = [] then
        (List.rev acc, None)
      else
        (List.rev acc, Some (List.rev code_acc))
    | phrase :: phrases ->
      match phrase with
      | Ptop_def [] -> loop phrases code_acc acc
      | Ptop_def [{pstr_desc = Pstr_extension(ext, [])}] -> begin
          match match_expect_extension ext with
          | None -> loop phrases (phrase :: code_acc) acc
          | Some expectation ->
            let chunk =
              { phrases     = List.rev code_acc
              ; expectation
              }
            in
            loop phrases [] (chunk :: acc)
        end
      | _ -> loop phrases (phrase :: code_acc) acc
  in
  loop phrases [] []

module Compiler_messages = struct
  let print_loc ppf (loc : Location.t) =
    let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "Line _";
    if startchar >= 0 then
      Format.fprintf ppf ", characters %d-%d" startchar endchar;
    Format.fprintf ppf ":@,"

  let capture ppf ~f =
    Misc.protect_refs
      [ R (Location.formatter_for_warnings , ppf)
      ; R (Location.printer                , print_loc)
      ]
      f
end

let collect_formatters buf pps ~f =
  let ppb = Format.formatter_of_buffer buf in
  let out_functions = Format.pp_get_formatter_out_functions ppb () in

  List.iter (fun pp -> Format.pp_print_flush pp ()) pps;
  let save =
    List.map (fun pp -> Format.pp_get_formatter_out_functions pp ()) pps
  in
  let restore () =
    List.iter2
      (fun pp out_functions ->
         Format.pp_print_flush pp ();
         Format.pp_set_formatter_out_functions pp out_functions)
      pps save
  in
  List.iter
    (fun pp -> Format.pp_set_formatter_out_functions pp out_functions)
    pps;
  match f () with
  | x             -> restore (); x
  | exception exn -> restore (); raise exn

(* Invariant: ppf = Format.formatter_of_buffer buf *)
let capture_everything buf ppf ~f =
  collect_formatters buf [Format.std_formatter; Format.err_formatter]
                     ~f:(fun () -> Compiler_messages.capture ppf ~f)

let exec_phrase ppf phrase =
  if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
  if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
  Toploop.execute_phrase true ppf phrase

let parse_contents ~fname contents =
  let lexbuf = Lexing.from_string contents in
  Location.init lexbuf fname;
  Location.input_name := fname;
  Parse.use_file lexbuf

let eval_expectation expectation ~output =
  let s =
    if !Clflags.principal then
      expectation.principal
    else
      expectation.normal
  in
  if s.str = output then
    None
  else
    let s = { s with str = output } in
    Some (
      if !Clflags.principal then
        { expectation with principal = s }
      else
        { expectation with normal = s }
    )

let shift_lines delta phrases =
  let position (pos : Lexing.position) =
    { pos with pos_lnum = pos.pos_lnum + delta }
  in
  let location _this (loc : Location.t) =
    { loc with
      loc_start = position loc.loc_start
    ; loc_end   = position loc.loc_end
    }
  in
  let mapper = { Ast_mapper.default_mapper with location } in
  List.map phrases ~f:(function
    | Parsetree.Ptop_dir _ as p -> p
    | Parsetree.Ptop_def st ->
      Parsetree.Ptop_def (mapper.structure mapper st))

let rec min_line_number : Parsetree.toplevel_phrase list -> int option =
function
  | [] -> None
  | (Ptop_dir _  | Ptop_def []) :: l -> min_line_number l
  | Ptop_def (st :: _) :: _ -> Some st.pstr_loc.loc_start.pos_lnum

let eval_expect_file _fname ~file_contents =
  Warnings.reset_fatal ();
  let chunks, trailing_code =
    parse_contents ~fname:"" file_contents |> split_chunks
  in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let exec_phrases phrases =
    let phrases =
      match min_line_number phrases with
      | None -> phrases
      | Some lnum -> shift_lines (1 - lnum) phrases
    in
    (* For formatting purposes *)
    Buffer.add_char buf '\n';
    let _ : bool =
      List.fold_left phrases ~init:true ~f:(fun acc phrase ->
        acc &&
        try
          exec_phrase ppf phrase
        with exn ->
          Location.report_exception ppf exn;
          false)
    in
    Format.pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
      (* For formatting purposes *)
      Buffer.add_char buf '\n';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    Misc.delete_eol_spaces s
  in
  let corrected_expectations =
    capture_everything buf ppf ~f:(fun () ->
      List.fold_left chunks ~init:[] ~f:(fun acc chunk ->
        let output = exec_phrases chunk.phrases in
        match eval_expectation chunk.expectation ~output with
        | None -> acc
        | Some correction -> correction :: acc)
      |> List.rev)
  in
  let trailing_output =
    match trailing_code with
    | None -> ""
    | Some phrases ->
      capture_everything buf ppf ~f:(fun () -> exec_phrases phrases)
  in
  { corrected_expectations; trailing_output }

let output_slice oc s a b =
  output_string oc (String.sub s ~pos:a ~len:(b - a))

let output_corrected oc ~file_contents correction =
  let output_body oc { str; tag } =
    Printf.fprintf oc "{%s|%s|%s}" tag str tag
  in
  let ofs =
    List.fold_left correction.corrected_expectations ~init:0
      ~f:(fun ofs c ->
        output_slice oc file_contents ofs c.payload_loc.loc_start.pos_cnum;
        output_body oc c.normal;
        if c.normal.str <> c.principal.str then begin
          output_string oc ", Principal";
          output_body oc c.principal
        end;
        c.payload_loc.loc_end.pos_cnum)
  in
  output_slice oc file_contents ofs (String.length file_contents);
  match correction.trailing_output with
  | "" -> ()
  | s  -> Printf.fprintf oc "\n[%%%%expect{|%s|}]\n" s

let write_corrected ~file ~file_contents correction =
  let oc = open_out file in
  output_corrected oc ~file_contents correction;
  close_out oc

let process_expect_file fname =
  let corrected_fname = fname ^ ".corrected" in
  let file_contents =
    let ic = open_in_bin fname in
    match really_input_string ic (in_channel_length ic) with
    | s           -> close_in ic; Misc.normalise_eol s
    | exception e -> close_in ic; raise e
  in
  let correction = eval_expect_file fname ~file_contents in
  write_corrected ~file:corrected_fname ~file_contents correction

let repo_root = ref ""

let main fname =
  Toploop.override_sys_argv
    (Array.sub Sys.argv ~pos:!Arg.current
       ~len:(Array.length Sys.argv - !Arg.current));
  (* Ignore OCAMLRUNPARAM=b to be reproducible *)
  Printexc.record_backtrace false;
  List.iter [ "stdlib" ] ~f:(fun s ->
    Topdirs.dir_directory (Filename.concat !repo_root s));
  Toploop.initialize_toplevel_env ();
  Sys.interactive := false;
  process_expect_file fname;
  exit 0

let args =
  Arg.align
    [ "-repo-root", Set_string repo_root,
      "<dir> root of the OCaml repository"
    ; "-principal", Set Clflags.principal,
      " Evaluate the file with -principal set"
    ]

let usage = "Usage: expect_test <options> [script-file [arguments]]\n\
             options are:"

let () =
  Clflags.error_size := 0;
  try
    Arg.parse args main usage;
    Printf.eprintf "expect_test: no input file\n";
    exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
