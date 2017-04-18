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

let bufferize f t =
  let b = new_buf () in
  f b t;
  Buffer.contents b

let opt_iter opt f = 
  match opt with 
  | None -> ()
  | Some x -> f x 
let (|?) = opt_iter
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


type tag_function = Odoc_info.text -> string

let html_of_example b g t  =
  bp b {|<pre class="example"> %a  </pre>|} g#html_of_text t 

let wrap_tag f b v =
  bp b {|<div class="tag"> %a </div>|} f  v

let wrap_tag_list f b = function
  | [] -> ()
  | l -> wrap_tag f b l

let wrap_tag_opt f b = function
  | None -> ()
  | v -> wrap_tag f b v
let encoding = 	 {|<meta charset="utf8">|}


module Generator (G : Odoc_html.Html_generator) =
struct
  class html =
    object(self)
      inherit G.html as super

      val! mutable doctype = "<!doctype html>\n"

      method! character_encoding () = encoding

   
      method! html_of_author_list = wrap_tag_list super#html_of_author_list
      method! html_of_version_opt = wrap_tag_opt super#html_of_version_opt
      method! html_of_before = wrap_tag_list super#html_of_before
      method! html_of_since_opt = wrap_tag_opt super#html_of_since_opt
      method! html_of_raised_exceptions = wrap_tag_list super#html_of_raised_exceptions
      method! html_of_return_opt = wrap_tag_opt super#html_of_return_opt
      method! html_of_sees = wrap_tag_list super#html_of_sees

      method! html_of_info_first_sentence b info_opt =
        bp b {| <div class="info">                     
                <div class="not-examples">
                    %a 
                </div>
                </div> |} (fun b () -> 
        info_opt |? fun ({i_deprecated;i_desc} : Odoc_info.info ) ->
          i_deprecated
          |? (fun d ->
              bp b {|<div class="warning">
                     <span class="label">%s</span>
                     %a
                     </div>
                   |} 
                Odoc_messages.deprecated self#html_of_text  d );
          i_desc |?
          (function 
            | [Odoc_info.Raw ""] -> ()
            |  d -> self#html_of_text b d; bs b "<br>\n"
          )) ();


      method! html_of_info ?(cls="") ?(indent=true) b 
          (info_opt : Odoc_info.info option) =
        opt_iter info_opt
          (fun ({
               i_authors ; i_version; i_before ; i_since ; i_raised_exceptions ; 
               i_return_value ; 
               i_sees;
               i_deprecated; 
               i_desc;
               i_custom
             } ) ->
             bp b {|%a
                    <div class="not-examples">
                    %a 
                    </div>
                    %a
                    %a
                  |} 
               (fun b indent -> if indent then bp b {|<div class="info %s">|} cls)
               indent
               (fun  b () ->
                  i_deprecated |?
                    (fun d ->
                       bp b {|<div class="warning">
                              <span class="label">%s</span>
                              %a
                              </div>
                            |} 
                         Odoc_messages.deprecated self#html_of_text  d );
                  i_desc |?
                    (function 
                      | [Odoc_info.Raw ""] -> ()
                      |  d -> self#html_of_text b d; bs b "<br>\n"
                    );
                  self#html_of_author_list b i_authors;
                  self#html_of_version_opt b i_version;
                  self#html_of_before b i_before;
                  self#html_of_since_opt b i_since;
                  self#html_of_raised_exceptions b i_raised_exceptions;
                  self#html_of_return_opt b i_return_value;
                  self#html_of_sees b i_sees)
               ()
               (fun b () ->  
                  if i_custom <> [] then 
                    bp b {| <div class="examples">
                    %a
                    </div>|}
                    self#html_of_custom  i_custom
               ) ()
               (fun b indent -> if indent then bs b "</div>\n") indent )

      (** Print html code for the given list of custom tagged texts. *)
      method! html_of_custom b l =
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


      (** Print html code for a type. *)
      method! html_of_type b t =
        bs b "<div class=\"type-declaration\">";
        Odoc_info.reset_type_names ();
        let father = Name.father t.ty_name in
        let print_field_prefix () =
          bp b {|<tr>
                 
                 <td align="left" valign="top" >
                 <code>&nbsp;&nbsp;</code>
                 </td> 
                 
                 <td align="left" valign="top" >
                 <code>
               |}
        in
        let print_field_comment = function
          | None -> ()
          | Some t ->
            bp b {|

            <td class="typefieldcomment" align="left" valign="top" >
            <code>
            (*
            </code>
            </td>
            
            <td class="typefieldcomment" align="left" valign="top" >
            %a
            </td>

            <td class="typefieldcomment" align="left" valign="bottom" >
            <code>
            *)
            </code>
            </td>
            |} (fun b x -> self#html_of_info b x) (Some t)
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

        bp b 
          {|<span id="%s">|}
          (Naming.type_target t);

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
      method! print_navbar b pre post name =
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
      method! prepare_header module_list =
        let f b ?(nav=None) ?(comments=[]) t  =

          bp b {|<head>
                 <link rel="stylesheet" href="../api_static/tomorrow-night.css">
                 <link rel="stylesheet" href="../api_static/style.css" type="text/css">
                 <script src="../api_static//highlight.pack.js"></script>
                 <script src="../api_static//script.js"></script>
                 <script>hljs.initHighlightingOnLoad();</script> 
                 %s
                 %a
                 <title> %s </title></head> 
               |} 
            encoding

          (fun b nav -> 
            nav |?
            (fun (pre_opt, post_opt, _name) ->
               pre_opt |?
               (fun name ->
                  bp b {|<link rel="previous" href="%s">|}
                    (fst (Naming.html_files name))
               );
               ( post_opt |?
                 (fun name ->
                    bp b {|<link rel="next" href="%s">|}
                      (fst (Naming.html_files name));
                 )))
          )
          nav
          t
        in
        self#prepare_navbar_module_index module_list; 
        (* not really the appopriate place to do this, but it's easy. Easy is good *)
        header <- f

      (** Generate the [<index_prefix>.html] file corresponding to the given module list.
        @raise Failure if an error occurs.*)
      method! generate_index module_list =
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
      method! generate_elements_index :
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
        tag_functions <- ("example", 
                          bufferize (fun b text -> html_of_example b self text))
                         :: tag_functions
  end
end
let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor);

(* local variables: *)
(* compile-command: "ocamlc.opt -I +compiler-libs -I +ocamldoc -c generator.mli generator.ml" *)
(* end: *)
