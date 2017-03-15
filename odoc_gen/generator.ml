open Odoc_info
open Value
open Type
open Extension
open Exception
open Class
open Module

let opt = Odoc_info.apply_opt

let new_buf () = Buffer.create 1024
let bp = Printf.bprintf
let bs = Buffer.add_string

let bufferize f =
  let b = new_buf () in
  (fun t -> f b t; Buffer.contents b)

let charset = ref "utf-8"

let html_of_example g t =
  let text = bufferize g#html_of_text t in
  "<pre class=\"example\">" ^ text ^ "</pre>"
(*
module Naming =
  struct
    (** The prefix for types marks. *)
    let mark_type = "TYPE"

    (** The prefix for types elements (record fields or constructors). *)
    let mark_type_elt = "TYPEELT"

    (** The prefix for functions marks. *)
    let mark_function = "FUN"

    (** The prefix for extensions marks. *)
    let mark_extension = "EXTENSION"

    (** The prefix for exceptions marks. *)
    let mark_exception = "EXCEPTION"

    (** The prefix for values marks. *)
    let mark_value = "VAL"

    (** The prefix for attributes marks. *)
    let mark_attribute = "ATT"

    (** The prefix for methods marks. *)
    let mark_method = "METHOD"

    (** The prefix for code files.. *)
    let code_prefix = "code_"

    (** The prefix for type files.. *)
    let type_prefix = "type_"

    (** Return the two html files names for the given module or class name.*)
    let html_files name =
      let qual =
        try
          let i = String.rindex name '.' in
          match name.[i + 1] with
          | 'A'..'Z' -> ""
          | _ -> "-c"
        with Not_found -> ""
      in
      let prefix = name^qual in
      let html_file = prefix^".html" in
      let html_frame_file = prefix^"-frame.html" in
      (html_file, html_frame_file)

    (** Return the target for the given prefix and simple name. *)
    let target pref simple_name = pref^simple_name

    (** Return the complete link target (file#target) for the given prefix string and complete name.*)
    let complete_target pref complete_name =
      let simple_name = Name.simple complete_name in
      let module_name =
        let s = Name.father complete_name in
        if s = "" then simple_name else s
      in
      let (html_file, _) = html_files module_name in
      html_file^"#"^(target pref simple_name)

    (** Return the link target for the given type. *)
    let type_target t = target mark_type (Name.simple t.ty_name)

    (** Return the link target for the given variant constructor. *)
    let const_target t f =
      let name = Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.vc_name in
      target mark_type_elt name

    (** Return the link target for the given record field. *)
    let recfield_target t f = target mark_type_elt
      (Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.rf_name)

    (** Return the link target for the given inline record field. *)
    let inline_recfield_target t c f = target mark_type_elt
      (Printf.sprintf "%s.%s.%s" t c f.rf_name)

    (** Return the link target for the given object field. *)
    let objfield_target t f = target mark_type_elt
      (Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.of_name)

    (** Return the complete link target for the given type. *)
    let complete_type_target t = complete_target mark_type t.ty_name

    let complete_recfield_target name =
      let typ = Name.father name in
      let field = Name.simple name in
      Printf.sprintf "%s.%s" (complete_target mark_type_elt typ) field

    let complete_const_target = complete_recfield_target

    (** Return the link target for the given extension. *)
    let extension_target x = target mark_extension (Name.simple x.xt_name)

    (** Return the complete link target for the given extension. *)
    let complete_extension_target x = complete_target mark_extension x.xt_name

    (** Return the link target for the given exception. *)
    let exception_target e = target mark_exception (Name.simple e.ex_name)

    (** Return the complete link target for the given exception. *)
    let complete_exception_target e = complete_target mark_exception e.ex_name

    (** Return the link target for the given value. *)
    let value_target v = target mark_value (Name.simple v.val_name)

    (** Return the given value name where symbols accepted in infix values
       are replaced by strings, to avoid clashes with the filesystem.*)
    let subst_infix_symbols name =
      let len = String.length name in
      let buf = Buffer.create len in
      let ch c = Buffer.add_char buf c in
      let st s = Buffer.add_string buf s in
      for i = 0 to len - 1 do
        match name.[i] with
        | '|' -> st "_pipe_"
        | '<' -> st "_lt_"
        | '>' -> st "_gt_"
        | '@' -> st "_at_"
        | '^' -> st "_exp_"
        | '&' -> st "_amp_"
        | '+' -> st "_plus_"
        | '-' -> st "_minus_"
        | '*' -> st "_star_"
        | '/' -> st "_slash_"
        | '$' -> st "_dollar_"
        | '%' -> st "_percent_"
        | '=' -> st "_equal_"
        | ':' -> st "_column_"
        | '~' -> st "_tilde_"
        | '!' -> st "_bang_"
        | '?' -> st "_questionmark_"
        | c -> ch c
      done;
      Buffer.contents buf

    (** Return the complete link target for the given value. *)
    let complete_value_target v = complete_target mark_value v.val_name

    (** Return the complete filename for the code of the given value. *)
    let file_code_value_complete_target v =
      code_prefix^mark_value^(subst_infix_symbols v.val_name)^".html"

    (** Return the link target for the given attribute. *)
    let attribute_target a = target mark_attribute (Name.simple a.att_value.val_name)

    (** Return the complete link target for the given attribute. *)
    let complete_attribute_target a = complete_target mark_attribute a.att_value.val_name

    (** Return the complete filename for the code of the given attribute. *)
    let file_code_attribute_complete_target a =
      code_prefix^mark_attribute^a.att_value.val_name^".html"

    (** Return the link target for the given method. *)
    let method_target m = target mark_method (Name.simple m.met_value.val_name)

    (** Return the complete link target for the given method. *)
    let complete_method_target m = complete_target mark_method m.met_value.val_name

    (** Return the complete filename for the code of the given method. *)
    let file_code_method_complete_target m =
      code_prefix^mark_method^m.met_value.val_name^".html"

    (** Return the link target for the given label section. *)
    let label_target l = target "" l

    (** Return the complete link target for the given section label. *)
    let complete_label_target l = complete_target "" l

    (** Return the complete filename for the code of the type of the
       given module or module type name. *)
    let file_type_module_complete_target name =
      type_prefix^name^".html"

    (** Return the complete filename for the code of the
       given module name. *)
    let file_code_module_complete_target name =
      code_prefix^name^".html"

    (** Return the complete filename for the code of the type of the
       given class or class type name. *)
    let file_type_class_complete_target name =
      type_prefix^name^".html"
  end
*)
module Generator (G : Odoc_html.Html_generator) =
struct
  class html =
    object(self)
      inherit G.html

      val mutable doctype = "<!doctype html>\n"

	    method character_encoding () =
	      Printf.sprintf "<meta charset=\"%s\">\n" !charset

      method init_style =
        style <- "
          <link rel=\"stylesheet\" href=\"../../odoc_gen/tomorrow-night.css\">\n
          <link rel=\"stylesheet\" href=\"../../odoc_gen/style.css\" type=\"text/css\">\n
          <script src=\"../../odoc_gen//highlight.pack.js\"></script>\n
          <script>hljs.initHighlightingOnLoad();</script>"; (* nasty hack but who cares! *)

      method html_of_info ?(cls="") ?(indent=true) b info_opt =
        match info_opt with
          None ->
            ()
        | Some info ->
            let module M = Odoc_info in
            if indent then bs b ("<div class=\"info "^cls^"\">\n");
            bs b "<div class=\"not-examples\">\n";
            begin match info.M.i_deprecated with
              None -> ()
            | Some d ->
                bs b "<span class=\"warning\">";
                bs b Odoc_messages.deprecated ;
                bs b "</span>" ;
                self#html_of_text b d;
                bs b "<br>\n"
            end;
            begin match info.M.i_desc with
              None -> ()
            | Some d when d = [Odoc_info.Raw ""] -> ()
            | Some d -> self#html_of_text b d; bs b "<br>\n"
            end;
            self#html_of_author_list b info.M.i_authors;
            self#html_of_version_opt b info.M.i_version;
            self#html_of_before b info.M.i_before;
            self#html_of_since_opt b info.M.i_since;
            self#html_of_raised_exceptions b info.M.i_raised_exceptions;
            self#html_of_return_opt b info.M.i_return_value;
            self#html_of_sees b info.M.i_sees;
            bs b "</div>\n";
            bs b "<div class=\"examples\">\n";
            self#html_of_custom b info.M.i_custom;
            bs b "</div>\n";
            if indent then bs b "</div>\n"

      (** Print html code for the given list of custom tagged texts. *)
      method html_of_custom b l =
        List.iter
          (fun (tag, text) ->
            try
              let f = List.assoc tag tag_functions in
              Buffer.add_string b (f text)
            with
              Not_found ->
                Odoc_info.warning (Odoc_messages.tag_not_handled tag)
          )
          l

      method private output_code' ?(with_pre=true) in_title file code =
        try
          let chanout = open_out file in
          let b = new_buf () in
          bs b "<html>";
          self#print_header b (self#inner_title in_title);
          bs b"<body>\n";
          self#html_of_code ~with_pre b code;
          bs b "</body></html>";
          Buffer.output_buffer chanout b;
          close_out chanout
        with
          Sys_error s ->
            incr Odoc_info.errors ;
            prerr_endline s
(*
      method generate_for_module pre post modu =
        try
          Odoc_info.verbose ("Generate for module "^modu.m_name);
          let (html_file, _) = Naming.html_files modu.m_name in
          let type_file = Naming.file_type_module_complete_target modu.m_name in
          let code_file = Naming.file_code_module_complete_target modu.m_name in
          let chanout = open_out (Filename.concat !Global.target_dir html_file) in
          let b = new_buf () in
          let pre_name = opt (fun m -> m.m_name) pre in
          let post_name = opt (fun m -> m.m_name) post in
          bs b doctype ;
          bs b "<html>\n";
          self#print_header b
            ~nav: (Some (pre_name, post_name, modu.m_name))
            ~comments: (Module.module_comments modu)
            (self#inner_title modu.m_name);
          bs b "<body>\n" ;
          self#print_navbar b pre_name post_name modu.m_name ;
          bs b "<h1>";
          if modu.m_text_only then
            bs b modu.m_name
          else
            (
            bs b
              (
                if Module.module_is_functor modu then
                  Odoc_messages.functo
                else
                  Odoc_messages.modul
              );
            bp b " <a href=\"%s\">%s</a>" type_file modu.m_name;
            (
              match modu.m_code with
                None -> ()
              | Some _ -> bp b " (<a href=\"%s\">.ml</a>)" code_file
            )
            );
          bs b "</h1>\n";

          if not modu.m_text_only then
            self#html_of_module b ~with_link: false modu
          else
            self#html_of_info ~indent:false b modu.m_info;

          (* parameters for functors *)
          self#html_of_module_parameter_list b
            (Name.father modu.m_name)
            (Module.module_parameters modu);

          (* a horizontal line *)
          if not modu.m_text_only then bs b "<hr width=\"100%\">\n";

          (* module elements *)
          List.iter
            (self#html_of_module_element b modu.m_name)
            (Module.module_elements modu);

          bs b "<div class=\"background\"></div>";
          bs b "</body></html>";
          Buffer.output_buffer chanout b;
          close_out chanout;

          (* generate html files for submodules *)
          self#generate_elements  self#generate_for_module (Module.module_modules modu);
          (* generate html files for module types *)
          self#generate_elements  self#generate_for_module_type (Module.module_module_types modu);
          (* generate html files for classes *)
          self#generate_elements  self#generate_for_class (Module.module_classes modu);
          (* generate html files for class types *)
          self#generate_elements  self#generate_for_class_type (Module.module_class_types modu);

          (* generate the file with the complete module type *)
          self#output_module_type
            modu.m_name
            (Filename.concat !Global.target_dir type_file)
            modu.m_type;

          match modu.m_code with
            None -> ()
          | Some code ->
              self#output_code' ~with_pre:false
                modu.m_name
                (Filename.concat !Global.target_dir code_file)
                code
        with
          Sys_error s ->
            raise (Failure s)
*)
      initializer
        tag_functions <- ("example", html_of_example self) :: tag_functions
  end
end
let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor);