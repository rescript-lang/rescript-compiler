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

(** {[
    _open -> open 
    _in -> in 
    _MAX_LENGTH -> MAX_LENGTH
    _Capital -> Capital 
    
    _open__ ->  _open
    open__ -> open 
    
    _'x -> 'x 

    _Capital__ -> _Capital 
    _MAX__ -> _MAX
    __ -> __ 
    __x -> __x 
    ___ -> _     
    ____ -> __
    _ -> _  (* error *)   
    

    ]} First we scan '__' from end to start, If found, discard it. Otherwise,
    check if it is [_ + keyword] or followed by capital letter, If so, discard
    [_].

    Limitations: user can not have [_Capital__, _Capital__other] to make it all
    compile to [Capital]. Keyword is fine [open__, open__other]. So we loose
    polymorphism over capital letter. It is okay, otherwise, if [_Captial__] is
    interpreted as [Capital], then there is no way to express [_Capital] *)

(* Copied from [ocaml/parsing/lexer.mll] *)
let key_words =
  String_hash_set.of_array
    [| "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done"
     ; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"
     ; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"
     ; "lazy"; "let"; "match"; "method"; "module"; "mutable"; "new"; "nonrec"
     ; "object"; "of"; "open"; "or"; (* "parser", PARSER; *)
                                     "private"; "rec"; "sig"; "struct"; "then"
     ; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with"
     ; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr" |]

let double_underscore = "__"

(*https://caml.inria.fr/pub/docs/manual-ocaml/lex.html {[

  label-name ::= lowercase-ident ]} *)
let valid_start_char x = match x with '_' | 'a' .. 'z' -> true | _ -> false

let translate ?loc name =
  assert (not @@ Ext_string.is_empty name) ;
  let i = Ext_string.rfind ~sub:double_underscore name in
  if i < 0 then
    let name_len = String.length name in
    if name.[0] = '_' then
      let try_key_word = String.sub name 1 (name_len - 1) in
      if
        name_len > 1
        && ( (not (valid_start_char try_key_word.[0]))
           || String_hash_set.mem key_words try_key_word )
      then try_key_word
      else name
    else name
  else if i = 0 then name
  else String.sub name 0 i
