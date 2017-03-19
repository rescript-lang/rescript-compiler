open Odoc_info
open Value
open Type
open Extension
open Exception
open Class
open Module

module Naming = Odoc_html.Naming

let opt = Odoc_info.apply_opt

let new_buf () = Buffer.create 1024
let bp = Printf.bprintf
let bs = Buffer.add_string

let bufferize f =
  let b = new_buf () in
  (fun t -> f b t; Buffer.contents b)

let print_concat b sep f =
  let rec iter = function
      [] -> ()
    | [c] -> f c
    | c :: q ->
        f c;
        bs b sep;
        iter q
  in
  iter

let charset = ref "utf-8"

let html_of_example g t =
  let text = bufferize g#html_of_text t in
  "<pre class=\"example\">" ^ text ^ "</pre>"

let wrap_tag f b v =
  bs b "<div class=\"tag\">";
  f b v;
  bs b "</div>"

let wrap_tag_list f b = function
| [] -> ()
| l -> wrap_tag f b l

let wrap_tag_opt f b = function
| None -> ()
| v -> wrap_tag f b v

module Generator (G : Odoc_html.Html_generator) =
struct
  class html =
    object(self)
      inherit G.html as super

      val mutable doctype = "<!doctype html>\n"

	    method character_encoding () =
	      Printf.sprintf "<meta charset=\"%s\">\n" !charset

      method init_style =
        style <- "
          <link rel=\"stylesheet\" href=\"../api_static/tomorrow-night.css\">\n
          <link rel=\"stylesheet\" href=\"../api_static/style.css\" type=\"text/css\">\n
          <script src=\"../api_static//highlight.pack.js\"></script>\n
          <script>hljs.initHighlightingOnLoad();</script>"; (* nasty hack but who cares! *)

      method html_of_author_list = wrap_tag_list super#html_of_author_list
      method html_of_version_opt = wrap_tag_opt super#html_of_version_opt
      method html_of_before = wrap_tag_list super#html_of_before
      method html_of_since_opt = wrap_tag_opt super#html_of_since_opt
      method html_of_raised_exceptions = wrap_tag_list super#html_of_raised_exceptions
      method html_of_return_opt = wrap_tag_opt super#html_of_return_opt
      method html_of_sees = wrap_tag_list super#html_of_sees

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
                bs b "<div class=\"warning\">";
                bs b "<span class=\"label\">";
                bs b Odoc_messages.deprecated ;
                bs b "</span>" ;
                self#html_of_text b d;
                bs b "</div>\n"
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

      (** Print html code for a type. *)
      method html_of_type b t =
        bs b "<div class=\"type-declaration\">";
        Odoc_info.reset_type_names ();
        let father = Name.father t.ty_name in
        let print_field_prefix () =
          bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
          bs b "<code>&nbsp;&nbsp;</code>";
          bs b "</td>\n<td align=\"left\" valign=\"top\" >\n";
          bs b "<code>";
        in
        let print_field_comment = function
          | None -> ()
          | Some t ->
              bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
              bs b "<code>";
              bs b "(*";
              bs b "</code></td>";
              bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
              self#html_of_info b (Some t);
              bs b "</td><td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
              bs b "<code>*)</code></td>"
        in
        bs b
          (match t.ty_manifest, t.ty_kind with
            None, Type_abstract
          | None, Type_open -> "\n<pre>"
          | None, Type_variant _
          | None, Type_record _ -> "\n<pre><code>"
          | Some _, Type_abstract
          | Some _, Type_open -> "\n<pre>"
          | Some _, Type_variant _
          | Some _, Type_record _ -> "\n<pre>"
          );
        bp b "<span id=\"%s\">" (Naming.type_target t);
        bs b ((self#keyword "type")^" ");
        self#html_of_type_expr_param_list b father t;
        (match t.ty_parameters with [] -> () | _ -> bs b " ");
        bs b (Name.simple t.ty_name);
        bs b "</span> ";
        let priv = t.ty_private = Asttypes.Private in
        (
        match t.ty_manifest with
          None -> ()
        | Some (Object_type fields) ->
            bs b "= ";
            if priv then bs b "private ";
            bs b "&lt;</pre>";
            bs b "<table class=\"typetable\">\n" ;
            let print_one f =
              print_field_prefix () ;
              bp b "<span id=\"%s\">%s</span>"
                (Naming.objfield_target t f)
                f.of_name;
              bs b "</td><td>: ";
              self#html_of_type_expr b father f.of_type;
              bs b ";</code></td>\n";
              print_field_comment f.of_text ;
              bs b "\n</tr>"
            in
            print_concat b "\n" print_one fields;
            bs b "</table>\n>\n";
            bs b " "
        | Some (Other typ) ->
            bs b "= ";
            if priv then bs b "private ";
            self#html_of_type_expr b father typ;
            bs b " "
        );
        (match t.ty_kind with
          Type_abstract -> bs b "</pre>"
        | Type_variant l ->
            bs b "= ";
            if priv then bs b "private ";
            bs b
              (
              match t.ty_manifest with
                None -> "</code></pre>"
              | Some _ -> "</pre>"
              );
            bs b "<table class=\"typetable\">\n";
            let print_one constr =
              bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
              bs b "<code>";
              bs b (self#keyword "|");
              bs b "</code></td>\n<td align=\"left\" valign=\"top\" >\n";
              bs b "<code>";
              bp b "<span id=\"%s\">%s</span>"
                (Naming.const_target t constr)
                (self#constructor constr.vc_name);
              (
              match constr.vc_args, constr.vc_ret with
                [], None -> ()
              | l,None ->
                  bs b ("</td><td>" ^ (self#keyword "of") ^ "</td><td>");
                  self#html_of_type_expr_list ~par: false b father " * " l;
              | [],Some r ->
                  bs b ("</td><td>" ^ (self#keyword ":") ^ "</td><td>");
                  self#html_of_type_expr b father r;
              | l,Some r ->
                  bs b ("</td><td>" ^ (self#keyword ":") ^ "</td><td>");
                  self#html_of_type_expr_list ~par: false b father " * " l;
                  bs b ("</td><td>" ^ (self#keyword "->") ^ "</td><td>");
                  self#html_of_type_expr b father r;
              );
              bs b "</code></td>\n";
              (
              match constr.vc_text with
                None -> ()
              | Some t ->
                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                  bs b "<code>";
                  bs b "(*";
                  bs b "</code></td>";
                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                  self#html_of_info b (Some t);
                  bs b "</td>";
                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
                  bs b "<code>";
                  bs b "*)";
                  bs b "</code></td>";
              );
              bs b "\n</tr>"
            in
            print_concat b "\n" print_one l;
            bs b "</table>\n"

        | Type_record l ->
            bs b "= ";
            if priv then bs b "private " ;
            bs b "{";
            bs b
              (
              match t.ty_manifest with
                None -> "</code></pre>"
              | Some _ -> "</pre>"
              );
            bs b "<table class=\"typetable\">\n" ;
            let print_one r =
              bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
              bs b "<code>&nbsp;&nbsp;</code>";
              bs b "</td>\n<td align=\"left\" valign=\"top\" >\n";
              bs b "<code>";
              if r.rf_mutable then bs b (self#keyword "mutable&nbsp;") ;
              bp b "<span id=\"%s\">%s</span>&nbsp;: "
                (Naming.recfield_target t r)
                r.rf_name;
              self#html_of_type_expr b father r.rf_type;
              bs b ";</code></td>\n";
              (
              match r.rf_text with
                None -> ()
              | Some t ->
                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                  bs b "<code>";
                  bs b "(*";
                  bs b "</code></td>";
                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                  self#html_of_info b (Some t);
                  bs b "</td><td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
                  bs b "<code>*)</code></td>";
              );
              bs b "\n</tr>"
            in
            print_concat b "\n" print_one l;
            bs b "</table>\n}\n"
        | Type_open ->
            bs b "= ..";
            bs b "</pre>"
        );
        bs b "\n";
        bs b "</div>\n";
        self#html_of_info b t.ty_info;
        bs b "\n"
 
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