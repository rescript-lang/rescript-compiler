(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova,                            *)
(*                         INRIA Rocquencourt                          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Printf
open Syntax
open Lexgen


(* To copy the ML code fragments *)

type line_tracker = {
  file : string;
  oc : out_channel;
  ic : in_channel;
  mutable cur_line : int;
};;

let open_tracker file oc = {
  file = file;
  oc = oc;
  ic = open_in_bin file;
  cur_line = 1;
};;

let close_tracker tr = close_in_noerr tr.ic;;

let update_tracker tr =
  fprintf tr.oc "\n";
  flush tr.oc;
  let cr_seen = ref false in
  try while true do
    match input_char tr.ic with
    | '\010' when not !cr_seen -> tr.cur_line <- tr.cur_line + 1;
    | '\013' -> cr_seen := true; tr.cur_line <- tr.cur_line + 1;
    | _ -> cr_seen := false;
  done with End_of_file ->
  fprintf tr.oc "# %d \"%s\"\n" (tr.cur_line+1) tr.file;
;;

let copy_buffer = Bytes.create 1024

let copy_chars_unix ic oc start stop =
  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    output oc copy_buffer 0 m;
    n := !n - m
  done

let copy_chars_win32 ic oc start stop =
  for _i = start to stop - 1 do
    let c = input_char ic in
    if c <> '\r' then output_char oc c
  done

let copy_chars =
  match Sys.os_type with
    "Win32" | "Cygwin" -> copy_chars_win32
  | _       -> copy_chars_unix

let copy_chunk ic oc trl loc add_parens =
  if loc.start_pos < loc.end_pos || add_parens then begin
    fprintf oc "# %d \"%s\"\n" loc.start_line loc.loc_file;
    if add_parens then begin
      for _i = 1 to loc.start_col - 1 do output_char oc ' ' done;
      output_char oc '(';
    end else begin
      for _i = 1 to loc.start_col do output_char oc ' ' done;
    end;
    seek_in ic loc.start_pos;
    copy_chars ic oc loc.start_pos loc.end_pos;
    if add_parens then output_char oc ')';
    update_tracker trl;
  end

(* Various memory actions *)

let output_mem_access oc i = fprintf oc "lexbuf.Lexing.lex_mem.(%d)" i

let output_memory_actions pref oc = function
  | []  -> ()
  | mvs ->
      output_string oc "(* " ;
  fprintf oc "L=%d " (List.length mvs) ;
  List.iter
    (fun mv -> match mv with
    | Copy (tgt, src) ->
        fprintf oc "[%d] <- [%d] ;" tgt src
    | Set tgt ->
        fprintf oc "[%d] <- p ; " tgt)
    mvs ;
  output_string oc " *)\n" ;
  List.iter
    (fun mv -> match mv with
    | Copy (tgt, src) ->
        fprintf oc
          "%s%a <- %a ;\n"
          pref output_mem_access tgt output_mem_access src
    | Set tgt ->
        fprintf oc "%s%a <- lexbuf.Lexing.lex_curr_pos ;\n"
          pref output_mem_access tgt)
    mvs

let output_base_mem oc = function
  | Mem i -> output_mem_access oc i
  | Start -> fprintf oc "lexbuf.Lexing.lex_start_pos"
  | End   -> fprintf oc  "lexbuf.Lexing.lex_curr_pos"

let output_tag_access oc = function
  | Sum (a,0) ->
      output_base_mem oc a
  | Sum (a,i) ->
      fprintf oc "(%a + %d)" output_base_mem a i

let output_env ic oc tr env =
  let pref = ref "let" in
  match env with
  | [] -> ()
  | _  ->
      (* Probably, we are better with variables sorted
         in apparition order *)
      let env =
        List.sort
          (fun ((_,p1),_) ((_,p2),_) ->
            Pervasives.compare p1.start_pos  p2.start_pos)
          env in

      List.iter
        (fun ((x,pos),v) ->
          fprintf oc "%s\n" !pref ;
          copy_chunk ic oc tr pos false ;
          begin match v with
          | Ident_string (o,nstart,nend) ->
              fprintf oc
                "= Lexing.sub_lexeme%s lexbuf %a %a"
                (if o then "_opt" else "")
                output_tag_access nstart output_tag_access nend
          | Ident_char (o,nstart) ->
              fprintf oc
                "= Lexing.sub_lexeme_char%s lexbuf %a"
                (if o then "_opt" else "")
                output_tag_access nstart
          end ;
          pref := "\nand")
        env ;
      fprintf oc " in\n"

(* Output the user arguments *)
let output_args oc args =
  List.iter (fun x -> (output_string oc x; output_char oc ' ')) args

let output_refill_handler ic oc oci = function
  | None -> false
  | Some location ->
    output_string oc "let __ocaml_lex_refill : \
                      (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a) =\n";
    copy_chunk ic oc oci location true;
    true

(* quiet flag *)
let quiet_mode = ref false;;
