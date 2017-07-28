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

open Odoc_info
module Naming = Odoc_html.Naming
open Odoc_info.Value
open Odoc_info.Module

let p = Printf.bprintf
let bp = Printf.bprintf
let bs = Buffer.add_string

module Html =
  (val
   (
   match !Odoc_args.current_generator with
     None -> (module Odoc_html.Generator : Odoc_html.Html_generator)
   | Some (Odoc_gen.Html m) -> m
   | _ ->
       failwith
         "A non-html generator is already set. Cannot install the Todo-list html generator"
  ) : Odoc_html.Html_generator)
;;

module Generator =
struct
class html =
  object (self)
    inherit Html.html as html

    method private html_of_module_comment b text =
      let br1, br2 =
        match text with
          [(Odoc_info.Title (n, l_opt, t))] -> false, false
        | (Odoc_info.Title (n, l_opt, t)) :: _ -> false, true
        | _ -> true, true
      in
      if br1 then p b "<br/>";
      self#html_of_text b text;
      if br2 then p b "<br/><br/>\n"

    method private html_of_Title b n l_opt t =
      let label1 = self#create_title_label (n, l_opt, t) in
      p b "<a name=\"%s\"></a>\n" (Naming.label_target label1);
      p b "<h%d>" n;
      self#html_of_text b t;
      p b "</h%d>" n

    val mutable code_id = 0
    method private code_block b code =
      code_id <- code_id + 1;
      Printf.bprintf b
      "<span class=\"code_expand\" onclick=\"if(document.getElementById('code%d').style.display=='none') {document.getElementById('code%d').style.display='block';} else {document.getElementById('code%d').style.display='none';}\"><img src=\"expand_collapse.png\" alt=\"+/-\"/></span>" code_id code_id code_id;
      Printf.bprintf b "<div id=\"code%d\" class=\"codeblock\">" code_id;
      self#html_of_code b code;
      Printf.bprintf b "</div>"

    (** Print html code for a value. *)
    method private html_of_value b v =
      Odoc_info.reset_type_names ();
      self#html_of_info b v.val_info;
      bs b "<pre>";
      bs b (self#keyword "val");
      bs b " ";
      (* html mark *)
      bp b "<a name=\"%s\"></a>" (Naming.value_target v);
      bs b (self#escape (Name.simple v.val_name));
      bs b " : ";
      self#html_of_type_expr b (Name.father v.val_name) v.val_type;
      bs b "</pre>";
      (
       if !Odoc_html.with_parameter_list then
         self#html_of_parameter_list b (Name.father v.val_name) v.val_parameters
       else
         self#html_of_described_parameter_list b (Name.father v.val_name) v.val_parameters
      );
      (
       match v.val_code with
         None -> ()
       | Some code ->
           self#code_block b code
      )
(*
    (** Print html code for a module. *)
    method private html_of_module b ?(info=true) ?(complete=true) ?(with_link=true) m =
      let (html_file, _) = Naming.html_files m.m_name in
      let father = Name.father m.m_name in
      bs b "<pre>";
      bs b ((self#keyword "module")^" ");
      (
       if with_link then
         bp b "<a href=\"%s\">%s</a>" html_file (Name.simple m.m_name)
       else
         bs b (Name.simple m.m_name)
      );
(*      A remettre quand on compilera avec ocaml 3.10
         (
       match m.m_kind with
         Module_functor _ when !Odoc_info.Args.html_short_functors  ->
           ()

       | _ -> *) bs b ": ";
      (*
      );
      *)
      self#html_of_module_kind b father ~modu: m m.m_kind;
      bs b "</pre>";
      if info && complete then
        self#html_of_info ~indent: false b m.m_info

*)
    initializer
      default_style_options <-
        ["a:visited {color : #416DFF; text-decoration : none; }" ;
          "a:link {color : #416DFF; text-decoration : none;}" ;
          "a:hover {color : Red; text-decoration : none; background-color: #5FFF88}" ;
          "a:active {color : Red; text-decoration : underline; }" ;
          ".keyword { font-weight : bold ; color : Red }" ;
          ".keywordsign { color : #C04600 }" ;
          ".superscript { font-size : 0.6em }" ;
          ".subscript { font-size : 0.6em }" ;
          ".comment { color : Green }" ;
          ".constructor { color : Blue }" ;
          ".type { color : #5C6585 }" ;
          ".string { color : Maroon }" ;
          ".warning { color : Red ; font-weight : bold }" ;
          ".info { margin-top: 8px; }";
          ".param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }" ;
          ".code { color : #465F91 ; }" ;
          "h1 { font-size : 20pt ; text-align: center; }" ;

          "h2 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #90BDFF ;"^
            "padding: 2px; }" ;

          "h3 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #90DDFF ;"^
            "padding: 2px; }" ;

          "h4 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #90EDFF ;"^
            "padding: 2px; }" ;

          "h5 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #90FDFF ;"^
            "padding: 2px; }" ;

          "h6 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #C0FFFF ; "^
            "padding: 2px; }" ;

          "div.h7 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #E0FFFF ; "^
            "padding: 2px; }" ;

          "div.h8 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #F0FFFF ; "^
            "padding: 2px; }" ;

          "div.h9 { font-size : 20pt ; border: 1px solid #000000; "^
            "margin-top: 5px; margin-bottom: 2px;"^
            "text-align: center; background-color: #FFFFFF ; "^
            "padding: 2px; }" ;

          ".typetable { border-style : hidden }" ;
          ".indextable { border-style : hidden }" ;
          ".paramstable { border-style : hidden ; padding: 5pt 5pt}" ;
          "body { background-color : White }" ;
          "tr { background-color : White }" ;
          "td.typefieldcomment { background-color : #FFFFFF ; font-size: smaller ;}" ;
          "pre { margin-bottom: 4px ; margin-left: 1em; "^
            "border-color: #27408b; border-style: solid; "^
            "border-width: 1px 1px 1px 3px; "^
            "padding: 4px; }" ;
          "div.sig_block {margin-left: 2em}" ;

          "div.codeblock { "^
            "margin-left: 2em; margin-right: 1em; padding: 6px; "^
            "margin-bottom: 8px; display: none; "^
            "border-width: 1px 1px 1px 3px; border-style: solid; border-color: grey; }" ;

          "span.code_expand { color: blue; text-decoration: underline; cursor: pointer; "^
          "margin-left: 1em ; } ";
        ];
  end
end

let _ = Odoc_args.set_generator
 (Odoc_gen.Html (module Generator : Odoc_html.Html_generator))
 ;;
