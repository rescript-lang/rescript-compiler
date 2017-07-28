(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

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
    let create_see s =
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
        let _ = Odoc_comments_global.init () in
        Odoc_lexer.comments_level := 0;
        let lexbuf = Lexing.from_string s in
        match Odoc_parser.main fun_lex lexbuf with
          None ->
            (0, None)
        | Some (desc, remain_opt) ->
            let mem_nb_chars = !Odoc_comments_global.nb_chars in
            let _ =
              match remain_opt with
                None ->
                  ()
              | Some s ->
                  (*DEBUG*)print_string ("remain: "^s); print_newline();
                  let lexbuf2 = Lexing.from_string s in
                  Odoc_parser.info_part2 Odoc_lexer.elements lexbuf2
            in
            (mem_nb_chars,
             Some
               {
                 i_desc = (match desc with "" -> None | _ -> Some (MyTexter.text_of_string desc));
                 i_authors = !Odoc_comments_global.authors;
                 i_version = !Odoc_comments_global.version;
                 i_sees = (List.map create_see !Odoc_comments_global.sees) ;
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
               with
                 Failure s ->
                   incr Odoc_global.errors ;
                    Printf.eprintf "File %S, line %d:\n%s\n%!" file (!Odoc_lexer.line_number + 1) s;
                   (0, None)
               | Odoc_text.Text_syntax (l, c, s) ->
                   incr Odoc_global.errors ;
                   prerr_endline (file^" : "^(Odoc_messages.text_parse_error l c s));
                   (0, None)
               | _ ->
                   incr Odoc_global.errors ;
                   prerr_endline (file^" : "^Odoc_messages.parse_error^"\n");
                   (0, None)

    (** This function takes a string where a simple comment may has been found. It returns
       false if there is a blank line or the first comment is a special one, or if there is
       no comment if the string.*)
    let nothing_before_simple_comment s =
      (* get the position of the first "(*" *)
      try
        print_DEBUG ("comment_is_attached: "^s);
        let pos = Str.search_forward (Str.regexp "(\\*") s 0 in
        let next_char = if (String.length s) >= (pos + 1) then s.[pos + 2] else '_' in
        (next_char <> '*') &&
        (
         (* there is no special comment between the constructor and the coment we got *)
         let s2 = String.sub s 0 pos in
         print_DEBUG ("s2="^s2);
         try
           let _ = Str.search_forward (Str.regexp ("['\n']"^simple_blank^"*['\n']")) s2 0 in
           (* a blank line was before the comment *)
           false
         with
           Not_found ->
             true
        )
      with
        Not_found ->
          false

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

    let retrieve_info_simple file (s : string) =
      let _ = Odoc_comments_global.init () in
      Odoc_lexer.comments_level := 0;
      let lexbuf = Lexing.from_string s in
      match Odoc_parser.main Odoc_lexer.simple lexbuf with
        None ->
          (0, None)
      | Some (desc, remain_opt) ->
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

    (** This function returns the first simple comment in
       the given string. If strict is [true] then no
       comment is returned if a blank line or a special
       comment is found before the simple comment. *)
    let retrieve_first_info_simple ?(strict=true) file (s : string) =
      match retrieve_info_simple file s with
        (_, None) ->
          (0, None)
      | (len, Some d) ->
          (* we check if the comment we got was really attached to the constructor,
             i.e. that there was no blank line or any special comment "(**" before *)
          if (not strict) || (nothing_before_simple_comment s) then
            (* ok, we attach the comment to the constructor *)
            (len, Some d)
          else
            (* a blank line or special comment was before the comment,
               so we must not attach this comment to the constructor. *)
            (0, None)

    let retrieve_last_info_simple file (s : string) =
      print_DEBUG ("retrieve_last_info_simple:"^s);
      let rec f cur_len cur_d =
        try
          let s2 = String.sub s cur_len ((String.length s) - cur_len) in
          print_DEBUG ("retrieve_last_info_simple.f:"^s2);
          match retrieve_info_simple file s2 with
            (len, None) ->
              print_DEBUG "retrieve_last_info_simple: None";
              (cur_len + len, cur_d)
          | (len, Some d) ->
              print_DEBUG "retrieve_last_info_simple: Some";
              f (len + cur_len) (Some d)
        with
          _ ->
            print_DEBUG "retrieve_last_info_simple : Erreur String.sub";
            (cur_len, cur_d)
      in
      f 0 None

    let retrieve_last_special_no_blank_after file (s : string) =
      print_DEBUG ("retrieve_last_special_no_blank_after:"^s);
      let rec f cur_len cur_d =
        try
          let s2 = String.sub s cur_len ((String.length s) - cur_len) in
          print_DEBUG ("retrieve_last_special_no_blank_after.f:"^s2);
          match retrieve_info_special file s2 with
            (len, None) ->
              print_DEBUG "retrieve_last_special_no_blank_after: None";
              (cur_len + len, cur_d)
          | (len, Some d) ->
              print_DEBUG "retrieve_last_special_no_blank_after: Some";
              f (len + cur_len) (Some d)
        with
          _ ->
            print_DEBUG "retrieve_last_special_no_blank_after : Erreur String.sub";
            (cur_len, cur_d)
      in
      f 0 None

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
          | (len2, Some d2) ->
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
