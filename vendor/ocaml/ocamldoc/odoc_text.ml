(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

exception Text_syntax of int * int * string (* line, char, string *)

open Odoc_types

module Texter =
  struct
    (* builds a text structure from a string. *)
    let text_of_string s =
      let lexbuf = Lexing.from_string s in
      try
        Odoc_text_lexer.init ();
        Odoc_text_parser.main Odoc_text_lexer.main lexbuf
      with
        _ ->
          raise (Text_syntax (!Odoc_text_lexer.line_number,
                              !Odoc_text_lexer.char_number,
                              s)
                )

    let count s c =
      let count = ref 0 in
      for i = 0 to String.length s - 1 do
        if s.[i] = c then incr count
      done;
      !count

    let escape_n s c n =
      let remain = ref n in
      let len = String.length s in
      let b = Buffer.create (len + n) in
      for i = 0 to len - 1 do
        if s.[i] = c && !remain > 0 then
          (
           Printf.bprintf b "\\%c" c;
           decr remain
          )
        else
          Buffer.add_char b s.[i]
      done;
      Buffer.contents b

    let escape_code s =
      let open_brackets = count s '[' in
      let close_brackets = count s ']' in
      if open_brackets > close_brackets then
        escape_n s '[' (open_brackets - close_brackets)
      else
        if close_brackets > open_brackets then
          escape_n s ']' (close_brackets - open_brackets)
        else
          s

    let escape_raw s =
      let len = String.length s in
      let b = Buffer.create len in
      for i = 0 to len - 1 do
        match s.[i] with
          '[' | ']' | '{' | '}' ->
            Printf.bprintf b "\\%c" s.[i]
        | c ->
            Buffer.add_char b c
      done;
      Buffer.contents b

    let p = Printf.bprintf

    let rec p_text b t =
      List.iter (p_text_element b) t

    and p_list b l =
      List.iter
        (fun t -> p b "{- " ; p_text b t ; p b "}\n")
        l

    and p_text_element b = function
      | Raw s -> p b "%s" (escape_raw s)
      | Code s -> p b "[%s]" (escape_code s)
      | CodePre s -> p b "{[%s]}" s
      | Verbatim s -> p b "{v %s v}" s
      | Bold t -> p b "{b " ; p_text b t ; p b "}"
      | Italic t -> p b "{i " ; p_text b t ; p b "}"
      | Emphasize t -> p b "{e " ; p_text b t ; p b "}"
      | Center t -> p b "{C " ; p_text b t ; p b "}"
      | Left t -> p b "{L " ; p_text b t ; p b "}"
      | Right t -> p b "{R " ; p_text b t ; p b "}"
      | List l -> p b "{ul\n"; p_list b l; p b "}"
      | Enum l -> p b "{ol\n"; p_list b l; p b "}"
      | Newline -> p b "\n"
      | Block  t -> p_text b t
      | Title (n, l_opt, t) ->
          p b "{%d%s "
            n
            (match l_opt with
              None -> ""
            | Some s -> ":"^s
            );
          p_text b t ;
          p b "}"
      | Latex s -> p b "{%% %s%%}" s
      | Link (s,t) ->
          p b "{{:%s}" s;
          p_text b t ;
          p b "}"
      | Ref (name, kind_opt, text_opt) ->
        begin
          p b "%s{!%s%s}"
            (match text_opt with None -> "" | Some _ -> "{")
            (match kind_opt with
               None -> ""
             | Some k ->
                 let s =
                   match k with
                     RK_module -> "module"
                   | RK_module_type -> "modtype"
                   | RK_class -> "class"
                   | RK_class_type -> "classtype"
                   | RK_value -> "val"
                   | RK_type -> "type"
                   | RK_extension -> "extension"
                   | RK_exception -> "exception"
                   | RK_attribute -> "attribute"
                   | RK_method -> "method"
                   | RK_section _ -> "section"
                   | RK_recfield -> "recfield"
                   | RK_const -> "const"
                 in
                 s^":"
            )
            name;
          match text_opt with
            None -> ()
          | Some t -> p_text b t; p b "}"
        end
      | Superscript t -> p b "{^" ; p_text b t ; p b "}"
      | Subscript t -> p b "{_" ; p_text b t ; p b "}"
      | Module_list l ->
          p b "{!modules:";
          List.iter (fun s -> p b " %s" s) l;
          p b "}"
      | Index_list ->
          p b "{!indexlist}"
      | Custom (s,t) ->
          p b "{%s " s;
          p_text b t;
          p b "}"
      | Target (target, code) ->
          p b "{%%%s: %s}" target (escape_raw code)

    let string_of_text s =
      let b = Buffer.create 256 in
      p_text b s;
      Buffer.contents b

  end
