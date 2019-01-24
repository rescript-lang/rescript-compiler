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

(** Analysis of comments. *)

open Odoc_types

let print_DEBUG s = print_string s ; print_newline ();;

(** This variable contains the regular expression representing a blank but not a '\n'.*)
let simple_blank = "[ \013\009\012]"

module type Texter =
    sig
      (** Return a text structure from a string. *)
      val text_of_string : string -> text
    end

module Info_retriever =
  functor (MyTexter : Texter) ->
  struct
    let create_see _file s =
      try
        let lexbuf = Lexing.from_string s in
        let (see_ref, s) = Odoc_parser.see_info Odoc_see_lexer.main lexbuf in
        (see_ref, MyTexter.text_of_string s)
      with
      | Odoc_text.Text_syntax (l, c, s) ->
          raise (Failure (Odoc_messages.text_parse_error l c s))
      | _ ->
          raise (Failure ("Unknown error while parsing @see tag: "^s))

    let retrieve_info fun_lex file (s : string) =
      try
        Odoc_comments_global.init ();
        Odoc_lexer.comments_level := 0;
        let lexbuf = Lexing.from_string s in
        match Odoc_parser.main fun_lex lexbuf with
          None ->
            (0, None)
        | Some (desc, remain_opt) ->
            let mem_nb_chars = !Odoc_comments_global.nb_chars in
            begin match remain_opt with
                None ->
                  ()
              | Some s ->
                  (*DEBUG*)print_string ("remain: "^s); print_newline();
                  let lexbuf2 = Lexing.from_string s in
                  Odoc_parser.info_part2 Odoc_lexer.elements lexbuf2
            end;
            (mem_nb_chars,
             Some
               {
                 i_desc = (match desc with "" -> None | _ -> Some (MyTexter.text_of_string desc));
                 i_authors = !Odoc_comments_global.authors;
                 i_version = !Odoc_comments_global.version;
                 i_sees = (List.map (create_see file) !Odoc_comments_global.sees) ;
                 i_since = !Odoc_comments_global.since;
                 i_before = Odoc_merge.merge_before_tags
                     (List.map (fun (n, s) ->
                         (n, MyTexter.text_of_string s)) !Odoc_comments_global.before)
                   ;
                 i_deprecated =
                 (match !Odoc_comments_global.deprecated with
                   None -> None | Some s -> Some (MyTexter.text_of_string s));
                 i_params =
                 (List.map (fun (n, s) ->
                   (n, MyTexter.text_of_string s)) !Odoc_comments_global.params);
                 i_raised_exceptions =
                 (List.map (fun (n, s) ->
                   (n, MyTexter.text_of_string s)) !Odoc_comments_global.raised_exceptions);
                 i_return_value =
                 (match !Odoc_comments_global.return_value with
                   None -> None | Some s -> Some (MyTexter.text_of_string s)) ;
                 i_custom = (List.map
                               (fun (tag, s) -> (tag, MyTexter.text_of_string s))
                               !Odoc_comments_global.customs)
               }
            )
      with e ->
        let (l, c, message) = match e with
          | Failure s -> (!Odoc_lexer.line_number + 1, 0, s)
          | Odoc_text.Text_syntax (l, c, s) -> (l, c, Odoc_messages.text_parse_error l c s)
          | _other -> (0, 0, Odoc_messages.parse_error)
        in begin
          incr Odoc_global.errors;
          prerr_endline (Odoc_messages.error_location file l c ^ message);
          (0, None)
        end


    (** Return true if the given string contains a blank line. *)
    let blank_line s =
      try
        let _ = Str.search_forward (Str.regexp ("['\n']"^simple_blank^"*['\n']")) s 0 in
        (* a blank line was before the comment *)
        true
      with
        Not_found ->
          false

    let retrieve_info_special file (s : string) =
      retrieve_info Odoc_lexer.main file s

    let retrieve_info_simple _file (s : string) =
      Odoc_comments_global.init ();
      Odoc_lexer.comments_level := 0;
      let lexbuf = Lexing.from_string s in
      match Odoc_parser.main Odoc_lexer.simple lexbuf with
        None ->
          (0, None)
      | Some _ ->
          (!Odoc_comments_global.nb_chars, Some Odoc_types.dummy_info)

    (** Return true if the given string contains a blank line outside a simple comment. *)
    let blank_line_outside_simple file s =
      let rec iter s2 =
        match retrieve_info_simple file s2 with
          (_, None) ->
            blank_line s2
        | (len, Some _) ->
            try
              let pos = Str.search_forward (Str.regexp_string "(*") s2 0 in
              let s_before = String.sub s2 0 pos in
              let s_after = String.sub s2 len ((String.length s2) - len) in
              (blank_line s_before) || (iter s_after)
            with
              Not_found ->
                (* we shouldn't get here *)
                false
      in
      iter s

    let all_special file s =
      print_DEBUG ("all_special: "^s);
      let rec iter acc n s2 =
        match retrieve_info_special file s2 with
          (_, None) ->
            (n, acc)
        | (n2, Some i) ->
            print_DEBUG ("all_special: avant String.sub new_s="^s2);
            print_DEBUG ("n2="^(string_of_int n2)) ;
            print_DEBUG ("len(s2)="^(string_of_int (String.length s2))) ;
            let new_s = String.sub s2 n2 ((String.length s2) - n2) in
            print_DEBUG ("all_special: apres String.sub new_s="^new_s);
            iter (acc @ [i]) (n + n2) new_s
      in
      let res = iter [] 0 s in
      print_DEBUG ("all_special: end");
      res

    let just_after_special file s =
      print_DEBUG ("just_after_special: "^s);
      let res = match retrieve_info_special file s with
        (_, None) ->
          (0, None)
      | (len, Some d) ->
          (* we must not have a simple comment or a blank line before. *)
          match retrieve_info_simple file (String.sub s 0 len) with
            (_, None) ->
              (
               try
                 (* if the special comment is the stop comment (**/**),
                    then we must not associate it. *)
                 let pos = Str.search_forward (Str.regexp_string "(**") s 0 in
                 if blank_line (String.sub s 0 pos) ||
                   d.Odoc_types.i_desc = Some [Odoc_types.Raw "/*"]
                 then
                   (0, None)
                 else
                   (len, Some d)
               with
                 Not_found ->
                   (* should not occur *)
                   (0, None)
              )
          | (_, Some _) ->
              (0, None)
      in
      print_DEBUG ("just_after_special:end");
      res

    let first_special file s =
      retrieve_info_special file s

    let get_comments f_create_ele file s =
      let (assoc_com, ele_coms) =
        (* get the comments *)
        let (len, special_coms) =  all_special file s in
        (* if there is no blank line after the special comments, and
           if the last special comment is not the stop special comment, then the
           last special comments must be associated to the element. *)
        match List.rev special_coms with
          [] ->
            (None, [])
        |  h :: q ->
            if (blank_line_outside_simple file
                  (String.sub s len ((String.length s) - len)) )
                || h.Odoc_types.i_desc = Some [Odoc_types.Raw "/*"]
            then
              (None, special_coms)
            else
              (Some h, List.rev q)
      in
      let ele_comments =
        List.fold_left
          (fun acc -> fun sc ->
            match sc.Odoc_types.i_desc with
              None ->
                acc
            | Some t ->
                acc @ [f_create_ele t])
          []
          ele_coms
      in
      (assoc_com, ele_comments)
  end

module Basic_info_retriever = Info_retriever (Odoc_text.Texter)

let info_of_string s =
  let dummy =
    {
      i_desc = None ;
      i_authors = [] ;
      i_version = None ;
      i_sees = [] ;
      i_since = None ;
      i_before = [] ;
      i_deprecated = None ;
      i_params = [] ;
      i_raised_exceptions = [] ;
      i_return_value = None ;
      i_custom = [] ;
    }
  in
  let s2 = Printf.sprintf "(** %s *)" s in
  let (_, i_opt) = Basic_info_retriever.first_special "-" s2 in
  match i_opt with
    None -> dummy
  | Some i -> i

let info_of_comment_file modlist f =
  try
    let s = Odoc_misc.input_file_as_string f in
    let i = info_of_string s in
    Odoc_cross.assoc_comments_info "" modlist i
  with
    Sys_error s ->
      failwith s
