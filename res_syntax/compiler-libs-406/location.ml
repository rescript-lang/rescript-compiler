(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing

let absname = ref false
    (* This reference should be in Clflags, but it would create an additional
       dependency and make bootstrapping Camlp4 more difficult. *)

type t = Warnings.loc = { loc_start: position; loc_end: position; loc_ghost: bool };;

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)
let set_input_name name =
  if name <> "" then input_name := name
(* Terminal info *)



let num_loc_lines = ref 0 (* number of lines already printed after input *)

(* Print the location in some way or another *)

open Format

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  let file = if file = "_none_"  then !input_name else file in 
  if !absname then absolute_path file else file

let print_filename ppf file =
  Format.fprintf ppf "%s" (show_filename file)

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

let setup_colors () =
  Misc.Color.setup !Clflags.color

let print_loc ppf loc =
  setup_colors ();
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let startchar =  startchar + 1 in 
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  if file = "//toplevel//" then begin
      fprintf ppf "Characters %i-%i"
              loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "%s@{<loc>%a%s%i" msg_file print_filename file msg_line line;
    if startchar >= 0 then
      fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar;
    fprintf ppf "@}"
  end
;;

let default_printer ppf loc =
  setup_colors ();
  fprintf ppf "@{<loc>%a@}%s@," print_loc loc msg_colon
;;

let printer = ref default_printer
let print ppf loc = !printer ppf loc

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_error_prefix ppf =
  setup_colors ();
  fprintf ppf "@{<error>%s@}" error_prefix;
;;

let print_compact ppf loc =
  begin
    let (file, line, startchar) = get_pos_info loc.loc_start in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    fprintf ppf "%a:%i" print_filename file line;
    if startchar >= 0 then fprintf ppf ",%i--%i" startchar endchar
  end
;;

let print_error ppf loc =
  fprintf ppf "%a%t:" print loc print_error_prefix;
;;

let print_error_cur_file ppf () = print_error ppf (in_file !input_name);;

let default_warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active { Warnings. number; message; is_error; sub_locs } ->
    setup_colors ();
    fprintf ppf "@[<v>";
    print ppf loc;
    if is_error
    then
      fprintf ppf "%t (%s %d): %s@," print_error_prefix
           (String.uncapitalize_ascii warning_prefix) number message
    else fprintf ppf "@{<warning>%s@} %d: %s@," warning_prefix number message;
    List.iter
      (fun (loc, msg) ->
         if loc <> none then fprintf ppf "  %a  %s@," print loc msg
      )
      sub_locs;
    fprintf ppf "@]"
;;

let warning_printer = ref default_warning_printer ;;

let print_warning loc ppf w = 
  !warning_printer loc ppf w  
;;

let formatter_for_warnings = ref err_formatter;;
let prerr_warning loc w = 
    print_warning loc !formatter_for_warnings w;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none


type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

let pp_ksprintf ?before k fmt =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Misc.Color.set_color_tag_handling ppf;
  begin match before with
    | None -> ()
    | Some f -> f ppf
  end;
  kfprintf
    (fun _ ->
      pp_print_flush ppf ();
      let msg = Buffer.contents buf in
      k msg)
    ppf fmt

(* Shift the formatter's offset by the length of the error prefix, which
   is always added by the compiler after the message has been formatted *)
let print_phanton_error_prefix ppf =
  Format.pp_print_as ppf (String.length error_prefix + 2 (* ": " *)) ""

let errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") fmt =
  pp_ksprintf
    ~before:print_phanton_error_prefix
    (fun msg -> {loc; msg; sub; if_highlight})
    fmt

let error ?(loc = none) ?(sub = []) ?(if_highlight = "") msg =
  {loc; msg; sub; if_highlight}

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

exception Already_displayed_error = Warnings.Errors

let error_of_exn exn =
  match exn with
  | Already_displayed_error -> Some `Already_displayed
  | _ ->
     let rec loop = function
       | [] -> None
       | f :: rest ->
          match f exn with
          | Some error -> Some (`Ok error)
          | None -> loop rest
     in
     loop !error_of_exn


let rec default_error_reporter ppf ({loc; msg; sub}) =
    fprintf ppf "@[<v>%a %s" print_error loc msg;
    List.iter (Format.fprintf ppf "@,@[<2>%a@]" default_error_reporter) sub;
    fprintf ppf "@]"
    
let error_reporter = ref default_error_reporter

let report_error ppf err =
   !error_reporter ppf err
;;

let error_of_printer loc print x =
  errorf ~loc "%a@?" print x

let error_of_printer_file print x =
  error_of_printer (in_file !input_name) print x

let () =
  register_error_of_exn
    (function
      | Sys_error msg ->
          Some (errorf ~loc:(in_file !input_name)
                "I/O error: %s" msg)

      | Misc.HookExnWrapper {error = e; hook_name;
                             hook_info={Misc.sourcefile}} ->
          let sub = match error_of_exn e with
            | None | Some `Already_displayed -> error (Printexc.to_string e)
            | Some (`Ok err) -> err
          in
          Some
            (errorf ~loc:(in_file sourcefile)
               "In hook %S:" hook_name
               ~sub:[sub])
      | _ -> None
    )

external reraise : exn -> 'a = "%reraise"

let rec report_exception_rec n ppf exn =
  try
    match error_of_exn exn with
    | None -> reraise exn
    | Some `Already_displayed -> ()
    | Some (`Ok err) -> fprintf ppf "@[%a@]@." report_error err
  with exn when n > 0 -> report_exception_rec (n-1) ppf exn

let report_exception ppf exn = report_exception_rec 5 ppf exn


exception Error of error

let () =
  register_error_of_exn
    (function
      | Error e -> Some e
      | _ -> None
    )

let raise_errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") =
  pp_ksprintf
    ~before:print_phanton_error_prefix
    (fun msg -> raise (Error ({loc; msg; sub; if_highlight})))

let deprecated ?(def = none) ?(use = none) loc msg =
  prerr_warning loc (Warnings.Deprecated (msg, def, use))