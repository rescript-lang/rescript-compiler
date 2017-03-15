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

let rec check_and_transform loc buf s byte_offset s_len =
  if byte_offset = s_len then ()
  else
    let current_char = s.[byte_offset] in
    match Ext_utf8.classify current_char with
    | Single c ->
      if c = 92 (* Char.code '\\' = 92 *)then
        begin
          (* we share the same escape sequence with js *)
          Buffer.add_char buf current_char;
          escape_code loc buf s (byte_offset+1) s_len
        end
      else
        begin
          (if c = 34 (* Char.code '\"' = 34 *) || c = 39 (* Char.code '\'' = 39 *) then
             begin
               Buffer.add_char buf '\\';
               Buffer.add_char buf current_char ;

             end
           else if  c = 10 then begin
             (* Char.code '\n' = 10 *)
             (* we can not just print new line*)
             Buffer.add_string buf "\\n";

             (* seems we don't need
                escape "\b" "\f"
                we need escape "\n" "\r" since
                ocaml multiple-line allows [\n]
                visual input while es5 string
                does not
             *)
           end
           else if c = 13 then begin
             Buffer.add_string buf "\\r"
           end
           else begin
             Buffer.add_char buf current_char;

           end);
          check_and_transform loc buf s (byte_offset + 1) s_len
        end
    | Invalid
    | Cont _ -> Location.raise_errorf ~loc "Not utf8 source string"
    | Leading (n,_) ->
      let i' = Ext_utf8.next s ~remaining:n  byte_offset in
      if i' < 0 then
        Location.raise_errorf ~loc "Not valid utf8 souce string"
      else
        begin
          for k = byte_offset to i' do
            Buffer.add_char buf s.[k];
          done;
          check_and_transform loc buf s (i' + 1) s_len
        end
and escape_code loc buf s offset s_len =
  if offset >= s_len then
    Location.raise_errorf ~loc "\\ is the end of string"
  else
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
        check_and_transform loc buf s (offset + 1) s_len
      end
    | 'u' ->
      begin
        Buffer.add_char buf cur_char;
        unicode loc buf s (offset + 1) s_len
      end
    | 'x' -> begin
        Buffer.add_char buf cur_char ;
        two_hex loc buf s (offset + 1) s_len
      end
    | _ -> Location.raise_errorf ~loc "invalid escape code"
and two_hex loc buf s offset s_len =
  if offset + 1 >= s_len then
    Location.raise_errorf ~loc "\\x need at least two chars";
  let a, b = s.[offset], s.[offset + 1] in
  if Ext_char.valid_hex a && Ext_char.valid_hex b then
    begin
      Buffer.add_char buf a ;
      Buffer.add_char buf b ;
      check_and_transform loc buf s (offset + 2) s_len
    end
  else Location.raise_errorf ~loc "%c%c is not a valid hex code" a b

and unicode loc buf s offset s_len =
  if offset + 3 >= s_len then
    Location.raise_errorf ~loc "\\u need at least four chars";
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
      check_and_transform loc buf s  (offset + 4) s_len
    end
  else
    Location.raise_errorf ~loc "%c%c%c%c is not a valid unicode point"
      a0 a1 a2 a3
(* http://www.2ality.com/2015/01/es6-strings.html
   console.log('\uD83D\uDE80'); (* ES6*)
   console.log('\u{1F680}');
*)

type interpo = Text of string | Delim of string

let consume_text s start_index =
  let rec _consume_text s index last_char new_word =
    if index = String.length s then new_word, String.length s
    else begin
      match s.[index] with
      | '$' -> if last_char = '\\' then _consume_text s (index+1) '$' (Ext_string.append new_word '$')
        else (new_word, index)
      | c -> _consume_text s (index + 1) c (Ext_string.append new_word c)
    end
  in _consume_text s start_index ' ' ""

let consume_delim s start_index =
  let with_par = ref false in
  let rec _consume_delim s index ident =
    if index = String.length s then (if !with_par = true then (None, index) else (Some ident, index))
    else
      match s.[index] with
      | '(' -> (if !with_par = false then (with_par := true; _consume_delim s (index+1) ident) else (None, index))
      | ')' -> (if !with_par = false then (None, index + 1) else (with_par := false; (Some ident, index+1)))
      | '$' -> (_consume_delim s (index+1) ident)
      | c -> if (Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z') ||
                (Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z') ||
                (Char.code c >= Char.code '0' && Char.code c <= Char.code '9') ||
                Char.code c = Char.code '_'
        then _consume_delim s (index+1) (Ext_string.append ident c)
        else if !with_par = false then (Some ident, index) else (None, index + 1)
  in match s with
  | "" -> (Some "", start_index)
  | _ -> if start_index = String.length s then (Some "", start_index)
    else (if s.[start_index] <> '$' then (None, start_index)
          else _consume_delim s start_index "")


let compute_new_loc (loc:Location.t) s = let length = String.length s in
  let new_loc =
    {loc with loc_start = {loc.loc_start with pos_cnum = loc.loc_end.pos_cnum};
              loc_end = {loc.loc_start with pos_cnum = loc.loc_end.pos_cnum + length}}
  in new_loc

let error_reporting_loc (loc:Location.t) start_index end_index =
  let new_loc =
    {loc with loc_start = {loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + start_index};
              loc_end   = {loc.loc_end   with pos_cnum = loc.loc_start.pos_cnum + end_index }} in new_loc

let split_es6_string s loc =
  let rec _split s index nl =
    if index >= String.length s then List.rev nl
    else begin
      match consume_text s index, consume_delim s index with
      | ("" , str_index)  , (None   , err_index) -> let new_loc = error_reporting_loc loc index err_index in Location.raise_errorf ~loc:new_loc "Not a valid es6 template string"
      | (str,  str_index) , (None   , _) -> _split s (str_index) (Text str::nl)
      | ("" , _), (Some "" , par_index) -> let new_loc = error_reporting_loc loc index par_index in Location.raise_errorf ~loc:new_loc "Not a valid es6 template string"
      | ("" , _), (Some par, par_index) -> _split s (par_index) (Delim par::nl)
      | _, _ -> let new_loc = error_reporting_loc loc index index in Location.raise_errorf ~loc:new_loc "Not a valid es6 template string"
    end in _split s 0 []



let rec _transform_individual_expression exp_list loc nl = match exp_list with
  | [] -> List.rev nl
  | exp::rexp -> match exp with
    | Text s -> let new_loc = compute_new_loc loc s in
      let s_len  = String.length s in
      let buf = Buffer.create (s_len * 2) in
      check_and_transform loc buf s 0 s_len;
      let new_exp:Parsetree.expression = {
        pexp_loc = new_loc;
        pexp_desc = Pexp_constant (Const_string (Buffer.contents buf, Some Literals.escaped_j_delimiter));
        pexp_attributes = [];
      } in _transform_individual_expression rexp new_loc (new_exp::nl)

    | Delim p -> let new_loc = compute_new_loc loc p in
      let ident = Parsetree.Pexp_ident { txt = (Longident.Lident p); loc = loc } in
      let js_to_string = Parsetree.Pexp_ident { txt =
                                                  Longident.Ldot (Longident.Ldot ((Longident.Lident "Js"), "String"), "make"); loc = loc } in
      let apply_exp:Parsetree.expression_desc = Parsetree.Pexp_apply ({pexp_desc = js_to_string; pexp_loc = new_loc; pexp_attributes = []},
                                                                      [("", {pexp_desc = ident; pexp_loc = new_loc; pexp_attributes = []} )]) in
      let new_exp:Parsetree.expression = {
        pexp_loc = new_loc;
        pexp_desc = apply_exp;
        pexp_attributes = [];
      } in _transform_individual_expression rexp new_loc (new_exp::nl)

let transform_es6_style_template_string s loc =
  let sub_strs = split_es6_string s loc
  in _transform_individual_expression sub_strs loc []

let rec fold_expression_list_with_string_concat prev (exp_list:Parsetree.expression list) = match exp_list with
  | [] -> prev
  | (e::re) ->
    let string_concat_exp:Parsetree.expression = {e with pexp_desc = Parsetree.Pexp_ident
                                                             {txt = Longident.Ldot (Longident.Lident ("Pervasives"), "^"); loc = e.pexp_loc}} in
    let new_string_exp = {e with pexp_desc = Parsetree.Pexp_apply (string_concat_exp, [("", prev); ("", e)])} in
    fold_expression_list_with_string_concat new_string_exp re


