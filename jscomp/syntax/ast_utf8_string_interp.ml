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

type error =
  | Invalid_code_point
  | Unterminated_backslash
  | Invalid_escape_code of char
  | Invalid_hex_escape
  | Invalid_unicode_escape
  | Unterminated_variable
  | Unmatched_paren
  | Invalid_syntax_of_var of string

type kind =
  | String
  | Var of int * int
(* [Var (loffset, roffset)]
  For parens it used to be (2,-1)
  for non-parens it used to be (1,0)
*)

(** Note the position is about code point *)
type pos = {
  lnum : int ;
  offset : int ;
  byte_bol : int (* Note it actually needs to be in sync with OCaml's lexing semantics *)
}


type segment = {
  start : pos;
  finish : pos ;
  kind : kind;
  content : string ;
}

type segments = segment list


type cxt = {
  mutable segment_start : pos ;
  buf : Buffer.t ;
  s_len : int ;
  mutable segments : segments;
  mutable pos_bol : int; (* record the abs position of current beginning line *)
  mutable byte_bol : int ;
  mutable pos_lnum : int ; (* record the line number *)
}


type exn += Error of pos *  pos * error

let pp_error fmt err =
  Format.pp_print_string fmt @@  match err with
  | Invalid_code_point -> "Invalid code point"
  | Unterminated_backslash -> "\\ ended unexpectedly"
  | Invalid_escape_code c -> "Invalid escape code: " ^ String.make 1 c
  | Invalid_hex_escape ->
    "Invalid \\x escape"
  | Invalid_unicode_escape -> "Invalid \\u escape"
  | Unterminated_variable -> "$ unterminated"
  | Unmatched_paren -> "Unmatched paren"
  | Invalid_syntax_of_var s -> "`" ^s ^ "' is not a valid syntax of interpolated identifer"
let valid_lead_identifier_char x =
  match x with
  | 'a'..'z' | '_' -> true
  | _ -> false

let valid_identifier_char x =
  match x with
  | 'a'..'z'
  | 'A'..'Z'
  | '0'..'9'
  | '_' | '\''-> true
  | _ -> false
(** Invariant: [valid_lead_identifier] has to be [valid_identifier] *)

let valid_identifier s =
  let s_len = String.length s in
  if s_len = 0 then false
  else
    valid_lead_identifier_char s.[0] &&
    Ext_string.for_all_from s 1  valid_identifier_char


let is_space x =
  match x with
  | ' ' | '\n' | '\t' -> true
  | _ -> false



(**
   FIXME: multiple line offset
   if there is no line offset. Note {|{j||} border will never trigger a new line
*)
let update_position border
    ({lnum ; offset;byte_bol } : pos)
    (pos : Lexing.position)=
  if lnum = 0 then
    {pos with pos_cnum = pos.pos_cnum + border + offset  }
    (** When no newline, the column number is [border + offset] *)
  else
    {
      pos with
      pos_lnum = pos.pos_lnum + lnum ;
      pos_bol = pos.pos_cnum + border + byte_bol;
      pos_cnum = pos.pos_cnum + border + byte_bol + offset;
      (** when newline, the column number is [offset] *)
    }
let update border
    (start : pos)
    (finish : pos) (loc : Location.t) : Location.t =
  let start_pos = loc.loc_start in
  { loc  with
    loc_start =
      update_position  border start start_pos;
    loc_end =
      update_position border finish start_pos
  }


(** Note [Var] kind can not be mpty  *)
let empty_segment {content } =
  Ext_string.is_empty content



let update_newline ~byte_bol loc  cxt =
  cxt.pos_lnum <- cxt.pos_lnum + 1 ;
  cxt.pos_bol <- loc;
  cxt.byte_bol <- byte_bol

let pos_error cxt ~loc error =
  raise (Error
           (cxt.segment_start,
            { lnum = cxt.pos_lnum ; offset = loc - cxt.pos_bol ; byte_bol = cxt.byte_bol}, error))

let add_var_segment cxt loc loffset roffset =
  let content =  Buffer.contents cxt.buf in
  Buffer.clear cxt.buf ;
  let next_loc = {
    lnum = cxt.pos_lnum ; offset = loc - cxt.pos_bol ;
    byte_bol = cxt.byte_bol } in
  if valid_identifier content then
    begin
      cxt.segments <-
        { start = cxt.segment_start;
          finish =  next_loc ;
          kind = Var (loffset, roffset);
          content} :: cxt.segments ;
      cxt.segment_start <- next_loc
    end
  else pos_error cxt ~loc (Invalid_syntax_of_var content)

let add_str_segment cxt loc   =
  let content =  Buffer.contents cxt.buf in
  Buffer.clear cxt.buf ;
  let next_loc = {
    lnum = cxt.pos_lnum ; offset = loc - cxt.pos_bol ;
    byte_bol = cxt.byte_bol } in
  cxt.segments <-
    { start = cxt.segment_start;
      finish =  next_loc ;
      kind = String;
      content} :: cxt.segments ;
  cxt.segment_start <- next_loc





let rec check_and_transform (loc : int )  s byte_offset ({s_len; buf} as cxt : cxt) =
  if byte_offset = s_len then
    add_str_segment cxt loc
  else
    let current_char = s.[byte_offset] in
    match Ext_utf8.classify current_char with
    | Single 92 (* '\\' *) ->
      escape_code (loc + 1)  s (byte_offset+1) cxt
    | Single 34 ->
      Buffer.add_string buf "\\\"";
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt
    | Single 39 ->
      Buffer.add_string buf "\\'";
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt
    | Single 10 ->

      Buffer.add_string buf "\\n";
      let loc = loc + 1 in
      let byte_offset = byte_offset + 1 in
      update_newline ~byte_bol:byte_offset loc cxt ; (* Note variable could not have new-line *)
      check_and_transform loc  s byte_offset cxt
    | Single 13 ->
      Buffer.add_string buf "\\r";
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt
    | Single 36 -> (* $ *)
      add_str_segment cxt loc  ;
      let offset = byte_offset + 1 in
      if offset >= s_len then
        pos_error ~loc cxt  Unterminated_variable
      else
        let cur_char = s.[offset] in
        if cur_char = '(' then
          expect_var_paren  (loc + 2)  s (offset + 1) cxt
        else
          expect_simple_var (loc + 1)  s offset cxt
    | Single _ ->
      Buffer.add_char buf current_char;
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt

    | Invalid
    | Cont _ -> pos_error ~loc cxt Invalid_code_point
    | Leading (n,_) ->
      let i' = Ext_utf8.next s ~remaining:n  byte_offset in
      if i' < 0 then
        pos_error cxt ~loc Invalid_code_point
      else
        begin
          for k = byte_offset to i' do
            Buffer.add_char buf s.[k];
          done;
          check_and_transform (loc + 1 )  s (i' + 1) cxt
        end
(**Lets keep identifier simple, so that we could generating a function easier in the future
   for example
   let f = [%fn{| $x + $y = $x_add_y |}]
*)
and expect_simple_var  loc  s offset ({buf; s_len} as cxt) =
  let v = ref offset in
  (* prerr_endline @@ Ext_pervasives.dump (s, has_paren, (is_space s.[!v]), !v); *)
  if not (offset < s_len  && valid_lead_identifier_char s.[offset]) then
    pos_error cxt ~loc (Invalid_syntax_of_var Ext_string.empty)
  else
    begin
      while !v < s_len && valid_identifier_char s.[!v]  do (* TODO*)
        let cur_char = s.[!v] in
        Buffer.add_char buf cur_char;
        incr v ;
      done;
      let added_length = !v - offset in
      let loc = added_length + loc in
      add_var_segment cxt loc 1 0 ;
      check_and_transform loc  s (added_length + offset) cxt
    end
and expect_var_paren  loc  s offset ({buf; s_len} as cxt) =
  let v = ref offset in
  (* prerr_endline @@ Ext_pervasives.dump (s, has_paren, (is_space s.[!v]), !v); *)
  while !v < s_len &&  s.[!v] <> ')' do
    let cur_char = s.[!v] in
    Buffer.add_char buf cur_char;
    incr v ;
  done;
  let added_length = !v - offset in
  let loc = added_length +  1 + loc  in
  if !v < s_len && s.[!v] = ')' then
    begin
      add_var_segment cxt loc 2 (-1) ;
      check_and_transform loc  s (added_length + 1 + offset) cxt
    end
  else
    pos_error cxt ~loc Unmatched_paren





(* we share the same escape sequence with js *)
and escape_code loc  s offset ({ buf; s_len} as cxt) =
  if offset >= s_len then
    pos_error cxt ~loc Unterminated_backslash
  else
    Buffer.add_char buf '\\';
  let cur_char = s.[offset] in
  match cur_char with
  | '\\'
  | 'b'
  | 't'
  | 'n'
  | 'v'
  | 'f'
  | 'r'
  | '0'
  | '$'
    ->
    begin
      Buffer.add_char buf cur_char ;
      check_and_transform (loc + 1)  s (offset + 1) cxt
    end
  | 'u' ->
    begin
      Buffer.add_char buf cur_char;
      unicode (loc + 1) s (offset + 1) cxt
    end
  | 'x' -> begin
      Buffer.add_char buf cur_char ;
      two_hex (loc + 1)  s (offset + 1) cxt
    end
  | _ -> pos_error cxt ~loc (Invalid_escape_code cur_char)
and two_hex loc  s offset ({buf ; s_len} as cxt) =
  if offset + 1 >= s_len then
    pos_error cxt ~loc Invalid_hex_escape;
  let a, b = s.[offset], s.[offset + 1] in
  if Ext_char.valid_hex a && Ext_char.valid_hex b then
    begin
      Buffer.add_char buf a ;
      Buffer.add_char buf b ;
      check_and_transform (loc + 2)  s (offset + 2) cxt
    end
  else
    pos_error cxt ~loc Invalid_hex_escape


and unicode loc  s offset ({buf ; s_len} as cxt) =
  if offset + 3 >= s_len then
    pos_error cxt ~loc Invalid_unicode_escape
  ;
  let a0,a1,a2,a3 = s.[offset], s.[offset+1], s.[offset+2], s.[offset+3] in
  if
    Ext_char.valid_hex a0 &&
    Ext_char.valid_hex a1 &&
    Ext_char.valid_hex a2 &&
    Ext_char.valid_hex a3 then
    begin
      Buffer.add_char buf a0;
      Buffer.add_char buf a1;
      Buffer.add_char buf a2;
      Buffer.add_char buf a3;
      check_and_transform (loc + 4) s  (offset + 4) cxt
    end
  else
    pos_error cxt ~loc Invalid_unicode_escape
let transform_test s =
  let s_len = String.length s in
  let buf = Buffer.create (s_len * 2) in
  let cxt =
    { segment_start = {lnum = 0; offset = 0; byte_bol = 0};
      buf ;
      s_len;
      segments = [];
      pos_lnum = 0;
      byte_bol = 0;
      pos_bol = 0;

    } in
  check_and_transform 0 s 0 cxt;
  List.rev cxt.segments


(** TODO: test empty var $() $ failure,
    Allow identifers x.A.y *)

open Ast_helper

(** Longident.parse "Pervasives.^" *)
let concat_ident  : Longident.t =
  Ldot (Lident "Pervasives", "^") (* FIXME: remove deps on `Pervasives` *)
   (* JS string concatMany *)
    (* Ldot (Ldot (Lident "Js", "String2"), "concat") *)

(* Longident.parse "Js.String.make"     *)
let to_string_ident : Longident.t =
    Ldot (Ldot (Lident "Js", "String2"), "make")


let escaped_j_delimiter =  "*j" (* not user level syntax allowed *)
let unescaped_j_delimiter = "j"
let unescaped_js_delimiter = "js"

let escaped = Some escaped_j_delimiter

let concat_exp
  (a : Parsetree.expression)
  (b : Parsetree.expression) : Parsetree.expression =
  let loc = Bs_loc.merge a.pexp_loc b.pexp_loc in
  Ast_compatible.apply_simple ~loc
  (Exp.ident { txt =concat_ident; loc})
    [a ;
     b]

let border = String.length "{j|"

let aux loc (segment : segment) =
  match segment with
  | {start ; finish; kind ; content}
    ->
    begin match kind with
      | String ->
        let loc = update border start finish  loc  in
        Ast_compatible.const_exp_string
          content ?delimiter:escaped ~loc
      | Var (soffset, foffset) ->
        let loc = {
          loc with
          loc_start = update_position  (soffset + border) start loc.loc_start ;
          loc_end = update_position (foffset + border) finish loc.loc_start
        } in
        Ast_compatible.apply_simple ~loc
          (Exp.ident ~loc {loc ; txt = to_string_ident })
          [
            Exp.ident ~loc {loc ; txt = Lident content}
          ]
    end


let transform_interp loc s =
  let s_len = String.length s in
  let buf = Buffer.create (s_len * 2 ) in
  try
    let cxt : cxt =
      { segment_start = {lnum = 0; offset = 0; byte_bol = 0};
        buf ;
        s_len;
        segments = [];
        pos_lnum = 0;
        byte_bol = 0;
        pos_bol = 0;

      } in

    check_and_transform 0 s 0 cxt;
    let rev_segments =  cxt.segments in
    match rev_segments with
    | [] ->
      Ast_compatible.const_exp_string ~loc ""  ?delimiter:escaped
    | [ segment] ->
      aux loc segment
    | a::rest ->
      Ext_list.fold_left rest (aux loc a) (fun acc x ->
          concat_exp (aux loc x) acc )        
  with
    Error (start,pos, error)
    ->
    Location.raise_errorf ~loc:(update border start pos loc )
      "%a"  pp_error error


let transform (e : Parsetree.expression) s delim : Parsetree.expression =
    if Ext_string.equal delim unescaped_js_delimiter then
        let js_str = Ast_utf8_string.transform e.pexp_loc s in
        { e with pexp_desc =
                       Pexp_constant (
            Pconst_string
                         (js_str, escaped))}
    else if Ext_string.equal delim unescaped_j_delimiter then
            transform_interp e.pexp_loc s
    else e

let is_unicode_string opt = Ext_string.equal opt escaped_j_delimiter

let is_unescaped s =
  Ext_string.equal s unescaped_j_delimiter
  || Ext_string.equal s unescaped_js_delimiter