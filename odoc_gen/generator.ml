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

      val! mutable doctype = "<!doctype html>\n"

      method! character_encoding () =
	Printf.sprintf "<meta charset=\"%s\">\n" !charset
   
      method! html_of_author_list = wrap_tag_list super#html_of_author_list
      method! html_of_version_opt = wrap_tag_opt super#html_of_version_opt
      method! html_of_before = wrap_tag_list super#html_of_before
      method! html_of_since_opt = wrap_tag_opt super#html_of_since_opt
      method! html_of_raised_exceptions = wrap_tag_list super#html_of_raised_exceptions
      method! html_of_return_opt = wrap_tag_opt super#html_of_return_opt
      method! html_of_sees = wrap_tag_list super#html_of_sees

      method! html_of_info ?(cls="") ?(indent=true) b info_opt =
        match info_opt with
          None ->
            ()
        | Some info ->
            let module M = Odoc_info in
            bp b {|%a
                   <div class="not-examples">
                   %a 
                   </div>
                   <div class="examples">
                   %a
                   </div>
                   %a
                 |} 
              (fun b indent -> if indent then bp b {|<div class="info %s">|} cls)
              indent
            (* if indent then bs b ("<div class=\"info "^cls^"\">\n"); *)
            (* bs b "<div class=\"not-examples\">\n"; *)
              (fun  b info ->
                 begin match info.M.i_deprecated with
                     None -> ()
                   | Some d ->
                     bp b {|<div class="warning">
                            <span class="label">
                            %s
                            </span>
                            %a
                            </div>
                          |} 
                       Odoc_messages.deprecated
                       (fun b d -> self#html_of_text b d) d 
                       (* bs b "<div class=\"warning\">"; *)
                       (* bs b "<span class=\"label\">"; *)
                       (* bs b Odoc_messages.deprecated ; *)
                       (* bs b "</span>" ; *)
                       (* self#html_of_text b d; *)
                       (* bs b "</div>\n" *)
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
                 self#html_of_sees b info.M.i_sees) info

            (* bs b "</div>\n"; *)
            (* bs b "<div class=\"examples\">\n"; *)
              (fun b info ->  self#html_of_custom b info.M.i_custom) info
            (* bs b "</div>\n"; *)
            (fun b indent -> if indent then bs b "</div>\n") indent 

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

      val mutable navbar_module_index = ""

      method private print_module_index b = 
        bs b "<nav class=\"module-index\">";
        bs b navbar_module_index;
        bs b "</nav>\n";

      (** Html code for navigation bar.
        @param pre optional name for optional previous module/class
        @param post optional name for optional next module/class
        @param name name of current module/class *)
      method print_navbar b pre post name =
        self#print_module_index b; (* not really the appropriate place, but easy *)
        bs b "<div class=\"navbar\">";
        (
        match pre with
          None -> ()
        | Some name ->
            bp b "<a class=\"pre\" href=\"%s\" title=\"%s\">%s</a>\n"
              (fst (Naming.html_files name))
              name
              Odoc_messages.previous
        );
        bs b "&nbsp;";
        let father = Name.father name in
        let href = if father = "" then self#index else fst (Naming.html_files father) in
        let father_name = if father = "" then "Index" else father in
        bp b "<a class=\"up\" href=\"%s\" title=\"%s\">%s</a>\n" href father_name Odoc_messages.up;
        bs b "&nbsp;";
        (
        match post with
          None -> ()
        | Some name ->
            bp b "<a class=\"post\" href=\"%s\" title=\"%s\">%s</a>\n"
              (fst (Naming.html_files name))
              name
              Odoc_messages.next
        );
        bs b "</div>\n"

      method private prepare_navbar_module_index module_list =
        let print_one b m =
          let html_file = fst (Naming.html_files m.m_name) in
          bp b "<li><a href=\"%s\">%s</a></li>" html_file m.m_name 
        in
        navbar_module_index <- bufferize (fun b () ->
          bs b "<ul>";
          print_concat b "\n" (print_one b) module_list;
          bs b "</ul>";
        ) ()

      (** A function to build the header of pages. *)
      method prepare_header module_list =
        let f b ?(nav=None) ?(comments=[]) t  =
          bs b "<head>\n";
          bs b "<link rel=\"stylesheet\" href=\"../api_static/tomorrow-night.css\">\n
                <link rel=\"stylesheet\" href=\"../api_static/style.css\" type=\"text/css\">\n
                <script src=\"../api_static//highlight.pack.js\"></script>\n
                <script src=\"../api_static//script.js\"></script>\n
                <script>hljs.initHighlightingOnLoad();</script>";
          bs b (self#character_encoding ()) ;
          (
          match nav with
            None -> ()
          | Some (pre_opt, post_opt, name) ->
              (match pre_opt with
                None -> ()
              | Some name ->
                  bp b "<link rel=\"previous\" href=\"%s\">\n"
                    (fst (Naming.html_files name));
              );
              (match post_opt with
                None -> ()
              | Some name ->
                  bp b "<link rel=\"next\" href=\"%s\">\n"
                    (fst (Naming.html_files name));
              );
          );
          bs b "<title>";
          bs b t ;
          bs b "</title>\n</head>\n"
        in
        self#prepare_navbar_module_index module_list; (* not really the appopriate place to do this, but it's easy. Easy is good *)
        header <- f

      (** Generate the [<index_prefix>.html] file corresponding to the given module list.
        @raise Failure if an error occurs.*)
      method generate_index module_list =
        try
          let chanout = open_out (Filename.concat !Global.target_dir self#index) in
          let b = new_buf () in
          let title = match !Global.title with None -> "" | Some t -> self#escape t in
          bs b doctype ;
          bs b "<html>\n";
          self#print_header b self#title;
          bs b "<body class=\"index top\">\n";

          bs b "<h1>";
          bs b title;
          bs b "</h1>\n" ;
          let info = Odoc_info.apply_opt
              (Odoc_info.info_of_comment_file module_list)
              !Odoc_info.Global.intro_file
          in
          (
          match info with
            None ->
              self#html_of_Index_list b;
              bs b "<br/>";
              self#html_of_Module_list b
                (List.map (fun m -> m.m_name) module_list);
          | Some i -> self#html_of_info ~indent: false b info
          );
          bs b "</body>\n</html>";
          Buffer.output_buffer chanout b;
          close_out chanout
        with
          Sys_error s ->
            raise (Failure s)


      (** A method to create index files. *)
      method generate_elements_index :
          'a.
          'a list ->
            ('a -> Odoc_info.Name.t) ->
              ('a -> Odoc_info.info option) ->
                ('a -> string) -> string -> string -> unit =
      fun elements name info target title simple_file ->
        try
          let chanout = open_out (Filename.concat !Global.target_dir simple_file) in
          let b = new_buf () in
          bs b "<html>\n";
          self#print_header b (self#inner_title title);
          bs b "<body class=\"index elements-index\">\n";
          self#print_navbar b None None "";
          bs b "<h1>";
          bs b title;
          bs b "</h1>\n" ;

          let sorted_elements = List.sort
              (fun e1 e2 -> compare (Name.simple (name e1)) (Name.simple (name e2)))
              elements
          in
          let groups = Odoc_info.create_index_lists sorted_elements (fun e -> Name.simple (name e)) in
          let f_ele e =
            let simple_name = Name.simple (name e) in
            let father_name = Name.father (name e) in
            bp b "<tr><td><a href=\"%s\">%s</a> " (target e) (self#escape simple_name);
            if simple_name <> father_name && father_name <> "" then
              bp b "[<a href=\"%s\">%s</a>]" (fst (Naming.html_files father_name)) father_name;
            bs b "</td>\n<td>";
            self#html_of_info_first_sentence b (info e);
            bs b "</td></tr>\n";
          in
          let f_group l =
            match l with
              [] -> ()
            | e :: _ ->
                let s =
                  match (Char.uppercase (Name.simple (name e)).[0]) with
                    'A'..'Z' as c -> String.make 1 c
                  | _ -> ""
                in
                bs b "<tr><td align=\"left\"><br>";
                bs b s ;
                bs b "</td></tr>\n" ;
                List.iter f_ele l
          in
          bs b "<table>\n";
          List.iter f_group groups ;
          bs b "</table>\n" ;
          bs b "</body>\n</html>";
          Buffer.output_buffer chanout b;
          close_out chanout
        with
          Sys_error s ->
            raise (Failure s)

      initializer
        tag_functions <- ("example", html_of_example self) :: tag_functions
  end
end
let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor);

(* local variables: *)
(* compile-command: "ocamlc.opt -I +compiler-libs -I +ocamldoc -c generator.mli generator.ml" *)
(* end: *)
