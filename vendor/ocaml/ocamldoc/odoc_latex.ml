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

(** Generation of LaTeX documentation. *)

let print_DEBUG s = print_string s ; print_newline ()

open Odoc_info
open Parameter
open Value
open Type
open Extension
open Exception
open Class
open Module



let separate_files = ref false

let latex_titles = ref [
  1, "section" ;
  2, "subsection" ;
  3, "subsubsection" ;
  4, "paragraph" ;
  5, "subparagraph" ;
]

let latex_value_prefix = ref Odoc_messages.default_latex_value_prefix
let latex_type_prefix = ref Odoc_messages.default_latex_type_prefix
let latex_type_elt_prefix = ref Odoc_messages.default_latex_type_elt_prefix
let latex_extension_prefix = ref Odoc_messages.default_latex_extension_prefix
let latex_exception_prefix = ref Odoc_messages.default_latex_exception_prefix
let latex_module_prefix = ref Odoc_messages.default_latex_module_prefix
let latex_module_type_prefix = ref Odoc_messages.default_latex_module_type_prefix
let latex_class_prefix = ref Odoc_messages.default_latex_class_prefix
let latex_class_type_prefix = ref Odoc_messages.default_latex_class_type_prefix
let latex_attribute_prefix = ref Odoc_messages.default_latex_attribute_prefix
let latex_method_prefix = ref Odoc_messages.default_latex_method_prefix

let new_buf () = Buffer.create 1024
let new_fmt () =
  let b = new_buf () in
  let fmt = Format.formatter_of_buffer b in
  (fmt,
   fun () ->
    Format.pp_print_flush fmt ();
    let s = Buffer.contents b in
    Buffer.reset b;
    s
  )

let p = Format.fprintf
let ps f s = Format.fprintf f "%s" s


let bp = Printf.bprintf
let bs = Buffer.add_string

let print_concat fmt sep f =
  let rec iter = function
      [] -> ()
    | [c] -> f c
    | c :: q ->
        f c;
        ps fmt sep;
        iter q
  in
  iter

(** Generation of LaTeX code from text structures. *)
class text =
  object (self)
    (** Return latex code to make a sectionning according to the given level,
       and with the given latex code. *)
    method section_style level s =
      try
        let sec = List.assoc level !latex_titles in
        "\\"^sec^"{"^s^"}\n"
      with Not_found -> s

    (** Associations of strings to substitute in latex code. *)
    val subst_strings = List.map (fun (x, y) -> (Str.regexp x, y))
      [
        "\001", "\001\002";
        "\\\\", "\001b";

        "{", "\\\\{";
        "}", "\\\\}";
        "\\$", "\\\\$";
        "\\^", "{\\\\textasciicircum}";
        "\xE0", "\\\\`a";
        "\xE2", "\\\\^a";
        "\xE9", "\\\\'e";
        "\xE8", "\\\\`e";
        "\xEA", "\\\\^e";
        "\xEB", "\\\\\"e";
        "\xE7", "\\\\c{c}";
        "\xF4", "\\\\^o";
        "\xF6", "\\\\\"o";
        "\xEE", "\\\\^i";
        "\xEF", "\\\\\"i";
        "\xF9", "\\\\`u";
        "\xFB", "\\\\^u";
        "%", "\\\\%";
        "_", "\\\\_";
        "~", "\\\\~{}";
        "#", "{\\char35}";
        "->", "$\\\\rightarrow$";
        "<-", "$\\\\leftarrow$";
        ">=", "$\\\\geq$";
        "<=", "$\\\\leq$";
        ">", "$>$";
        "<", "$<$";
        "=", "$=$";
        "|", "{\\\\textbar}";
        "\\.\\.\\.", "$\\\\ldots$";
        "&", "\\\\&";

        "\001b", "{\\\\char92}";
        "\001\002", "\001";
      ]

    val subst_strings_simple = List.map (fun (x, y) -> (Str.regexp x, y))
      [
        "\001", "\001\002";
        "\\\\", "\001b";
        "{", "\001l";

        "}", "{\\\\char125}";
        "'", "{\\\\textquotesingle}";
        "`", "{\\\\textasciigrave}";

        "\001b", "{\\\\char92}";
        "\001l", "{\\\\char123}";
        "\001\002", "\001";
      ]

    val subst_strings_code = List.map (fun (x, y) -> (Str.regexp x, y))
      [
        "\001", "\001\002";
        "\\\\", "\001b";
        "{", "\001l";

        "}", "{\\\\char125}";
        "'", "{\\\\textquotesingle}";
        "`", "{\\\\textasciigrave}";
        "%", "\\\\%";
        "_", "\\\\_";
        "~", "{\\\\char126}";
        "#", "{\\\\char35}";
        "&", "\\\\&";
        "\\$", "\\\\$";
        "\\^", "{\\\\char94}";

        "\001b", "{\\\\char92}";
        "\001l", "{\\\\char123}";
        "\001\002", "\001";
      ]

    method subst l s =
      List.fold_left (fun acc (re, st) -> Str.global_replace re st acc) s l

    (** Escape the strings which would clash with LaTeX syntax. *)
    method escape s = self#subst subst_strings s

    (** Escape the ['\'], ['{'] and ['}'] characters. *)
    method escape_simple s = self#subst subst_strings_simple s

    (** Escape some characters for the code style. *)
    method escape_code s = self#subst subst_strings_code s

    (** Make a correct latex label from a name. *)
    (* The following characters are forbidden in LaTeX \index:
       \ { } $ & # ^ _ % ~ ! " @ | (" to close the double quote)
       The following characters are forbidden in LaTeX \label:
       \ { } $ & # ^ _ % ~
       So we will use characters not forbidden in \index if no_ = true.
    *)
    method label ?(no_=true) name =
      let len = String.length name in
      let buf = Buffer.create len in
      for i = 0 to len - 1 do
        let (s_no_, s) =
          match name.[i] with
          '_' -> ("-underscore", "_")
        | '~' -> ("-tilde", "~")
        | '%' -> ("-percent", "%")
        | '@' -> ("-at", "\"@")
        | '!' -> ("-bang", "\"!")
        | '|' -> ("-pipe", "\"|")
        | '<' -> ("-lt", "<")
        | '>' -> ("-gt", ">")
        | '^' -> ("-exp", "^")
        | '&' -> ("-ampersand", "&")
        | '+' -> ("-plus", "+")
        | '-' -> ("-minus", "-")
        | '*' -> ("-star", "*")
        | '/' -> ("-slash", "/")
        | '$' -> ("-dollar", "$")
        | '=' -> ("-equal", "=")
        | ':' -> ("-colon", ":")
        | c -> (String.make 1 c, String.make 1 c)
        in
        Buffer.add_string buf (if no_ then s_no_ else s)
      done;
      Buffer.contents buf

    (** Make a correct label from a value name. *)
    method value_label ?no_ name = !latex_value_prefix^(self#label ?no_ name)

    (** Make a correct label from an attribute name. *)
    method attribute_label ?no_ name = !latex_attribute_prefix^(self#label ?no_ name)

    (** Make a correct label from a method name. *)
    method method_label ?no_ name = !latex_method_prefix^(self#label ?no_ name)

    (** Make a correct label from a class name. *)
    method class_label ?no_ name = !latex_class_prefix^(self#label ?no_ name)

    (** Make a correct label from a class type name. *)
    method class_type_label ?no_ name = !latex_class_type_prefix^(self#label ?no_ name)

    (** Make a correct label from a module name. *)
    method module_label ?no_ name = !latex_module_prefix^(self#label ?no_ name)

    (** Make a correct label from a module type name. *)
    method module_type_label ?no_ name = !latex_module_type_prefix^(self#label ?no_ name)

    (** Make a correct label from an extension name. *)
    method extension_label ?no_ name = !latex_extension_prefix^(self#label ?no_ name)

    (** Make a correct label from an exception name. *)
    method exception_label ?no_ name = !latex_exception_prefix^(self#label ?no_ name)

    (** Make a correct label from a type name. *)
    method type_label ?no_ name = !latex_type_prefix^(self#label ?no_ name)

    (** Make a correct label from a record field. *)
    method recfield_label ?no_ name = !latex_type_elt_prefix^(self#label ?no_ name)

    (** Make a correct label from a variant constructor. *)
    method const_label ?no_ name = !latex_type_elt_prefix^(self#label ?no_ name)

    (** Return latex code for the label of a given label. *)
    method make_label label = "\\label{"^label^"}"

    (** Return latex code for the ref to a given label. *)
    method make_ref label = "\\ref{"^label^"}"

    (** Print the LaTeX code corresponding to the [text] parameter.*)
    method latex_of_text fmt t =
      List.iter (self#latex_of_text_element fmt) t

    (** Print the LaTeX code for the [text_element] in parameter. *)
    method latex_of_text_element fmt txt =
      match txt with
      | Odoc_info.Raw s -> self#latex_of_Raw fmt s
      | Odoc_info.Code s -> self#latex_of_Code fmt s
      | Odoc_info.CodePre s -> self#latex_of_CodePre fmt s
      | Odoc_info.Verbatim s -> self#latex_of_Verbatim fmt s
      | Odoc_info.Bold t -> self#latex_of_Bold fmt t
      | Odoc_info.Italic t -> self#latex_of_Italic fmt t
      | Odoc_info.Emphasize t -> self#latex_of_Emphasize fmt t
      | Odoc_info.Center t -> self#latex_of_Center fmt t
      | Odoc_info.Left t -> self#latex_of_Left fmt t
      | Odoc_info.Right t -> self#latex_of_Right fmt t
      | Odoc_info.List tl -> self#latex_of_List fmt tl
      | Odoc_info.Enum tl -> self#latex_of_Enum fmt tl
      | Odoc_info.Newline -> self#latex_of_Newline fmt
      | Odoc_info.Block t -> self#latex_of_Block fmt t
      | Odoc_info.Title (n, l_opt, t) -> self#latex_of_Title fmt n l_opt t
      | Odoc_info.Latex s -> self#latex_of_Latex fmt s
      | Odoc_info.Link (s, t) -> self#latex_of_Link fmt s t
      | Odoc_info.Ref (name, ref_opt, text_opt) ->
          self#latex_of_Ref fmt name ref_opt text_opt
      | Odoc_info.Superscript t -> self#latex_of_Superscript fmt t
      | Odoc_info.Subscript t -> self#latex_of_Subscript fmt t
      | Odoc_info.Module_list _ -> ()
      | Odoc_info.Index_list -> ()
      | Odoc_info.Custom (s,t) -> self#latex_of_custom_text fmt s t
      | Odoc_info.Target (target, code) -> self#latex_of_Target fmt ~target ~code

    method latex_of_custom_text fmt s t = ()

    method latex_of_Target fmt ~target ~code =
      if String.lowercase target = "latex" then
        self#latex_of_Latex fmt code
      else
        ()

    method latex_of_Raw fmt s =
      ps fmt (self#escape s)

    method latex_of_Code fmt s =
      let s2 = self#escape_code s in
      let s3 = Str.global_replace (Str.regexp "\n") ("\\\\\n") s2 in
      p fmt "{\\tt{%s}}" s3

    method latex_of_CodePre fmt s =
      ps fmt "\\begin{ocamldoccode}\n";
      ps fmt (self#escape_simple s);
      ps fmt "\n\\end{ocamldoccode}\n"

    method latex_of_Verbatim fmt s =
      ps fmt "\n\\begin{verbatim}\n";
      ps fmt s;
      ps fmt "\n\\end{verbatim}\n"

    method latex_of_Bold fmt t =
      ps fmt "{\\bf ";
      self#latex_of_text fmt t;
      ps fmt "}"

    method latex_of_Italic fmt t =
      ps fmt "{\\it ";
      self#latex_of_text fmt t;
      ps fmt "}"

    method latex_of_Emphasize fmt t =
      ps fmt "{\\em ";
      self#latex_of_text fmt t;
      ps fmt "}"

    method latex_of_Center fmt t =
      ps fmt "\\begin{center}\n";
      self#latex_of_text fmt t;
      ps fmt "\\end{center}\n"

    method latex_of_Left fmt t =
      ps fmt "\\begin{flushleft}\n";
      self#latex_of_text fmt t;
      ps fmt "\\end{flushleft}\n"

    method latex_of_Right fmt t =
      ps fmt "\\begin{flushright}\n";
      self#latex_of_text fmt t;
      ps fmt "\\end{flushright}\n"

    method latex_of_List fmt tl =
      ps fmt "\\begin{itemize}\n";
      List.iter
        (fun t ->
          ps fmt "\\item ";
          self#latex_of_text fmt t;
          ps fmt "\n"
        )
        tl;
      ps fmt "\\end{itemize}\n"

    method latex_of_Enum fmt tl =
      ps fmt "\\begin{enumerate}\n";
      List.iter
        (fun t ->
          ps fmt "\\item ";
          self#latex_of_text fmt t;
          ps fmt "\n"
        )
        tl;
      ps fmt "\\end{enumerate}\n"

    method latex_of_Newline fmt = ps fmt "\n\n"

    method latex_of_Block fmt t =
      ps fmt "\\begin{ocamldocdescription}\n";
      self#latex_of_text fmt t;
      ps fmt "\n\\end{ocamldocdescription}\n"

    method latex_of_Title fmt n label_opt t =
      let (fmt2, flush) = new_fmt () in
      self#latex_of_text fmt2 t;
      let s_title2 = self#section_style n (flush ()) in
      ps fmt s_title2;
      (
       match label_opt with
         None -> ()
       | Some l ->
           ps fmt (self#make_label (self#label ~no_: false l))
      )

    method latex_of_Latex fmt s = ps fmt s

    method latex_of_Link fmt s t =
      self#latex_of_text fmt t ;
      ps fmt "[\\url{";
      ps fmt s ;
      ps fmt "}]"

    method latex_of_Ref fmt name ref_opt text_opt =
      match ref_opt with
        None ->
          self#latex_of_text fmt
          (match text_opt with
             None ->
               [Odoc_info.Code (Odoc_info.use_hidden_modules name)]
           | Some t -> t
           )
      | Some (RK_section _) ->
          self#latex_of_text_element fmt
            (Latex ("["^(self#make_ref (self#label ~no_:false (Name.simple name)))^"]"))
      | Some kind ->
          let f_label =
            match kind with
              Odoc_info.RK_module -> self#module_label
            | Odoc_info.RK_module_type -> self#module_type_label
            | Odoc_info.RK_class -> self#class_label
            | Odoc_info.RK_class_type -> self#class_type_label
            | Odoc_info.RK_value -> self#value_label
            | Odoc_info.RK_type -> self#type_label
            | Odoc_info.RK_extension -> self#extension_label
            | Odoc_info.RK_exception -> self#exception_label
            | Odoc_info.RK_attribute -> self#attribute_label
            | Odoc_info.RK_method -> self#method_label
            | Odoc_info.RK_section _ -> assert false
            | Odoc_info.RK_recfield -> self#recfield_label
            | Odoc_info.RK_const -> self#const_label
          in
          let text =
            match text_opt with
              None -> [Odoc_info.Code (Odoc_info.use_hidden_modules name)]
            | Some t -> t
          in
          self#latex_of_text fmt
             (text @ [Latex ("["^(self#make_ref (f_label name))^"]")])

    method latex_of_Superscript fmt t =
      ps fmt "$^{";
      self#latex_of_text fmt t;
      ps fmt "}$"

    method latex_of_Subscript fmt t =
      ps fmt "$_{";
      self#latex_of_text fmt t;
      ps fmt "}$"

  end

(** A class used to generate LaTeX code for info structures. *)
class virtual info =
  object (self)
    (** The method used to get LaTeX code from a [text]. *)
    method virtual latex_of_text : Format.formatter -> Odoc_info.text -> unit

    (** The method used to get a [text] from an optionel info structure. *)
    method virtual text_of_info : ?block: bool -> Odoc_info.info option -> Odoc_info.text

    (** Print LaTeX code for a description, except for the [i_params] field. *)
    method latex_of_info fmt ?(block=false) info_opt =
      self#latex_of_text fmt
        (self#text_of_info ~block info_opt)
  end

module Generator =
struct
(** This class is used to create objects which can generate a simple LaTeX documentation. *)
class latex =
  object (self)
    inherit text
    inherit Odoc_to_text.to_text as to_text
    inherit info

    (** Get the first sentence and the rest of a description,
       from an optional [info] structure. The first sentence
       can be empty if it would not appear right in a title.
       In the first sentence, the titles and lists has been removed,
       since it is used in LaTeX titles and would make LaTeX complain
       if we has two nested \section commands.
    *)
    method first_and_rest_of_info i_opt =
      match i_opt with
        None -> ([], [])
      | Some i ->
            match i.Odoc_info.i_desc with
              None -> ([], self#text_of_info ~block: true i_opt)
            | Some t ->
                let (first,_) = Odoc_info.first_sentence_and_rest_of_text t in
                let (_, rest) = Odoc_info.first_sentence_and_rest_of_text (self#text_of_info ~block: false i_opt) in
                (Odoc_info.text_no_title_no_list first, rest)

    (** Print LaTeX code for a value. *)
    method latex_of_value fmt v =
      Odoc_info.reset_type_names () ;
      let label = self#value_label v.val_name in
      let latex = self#make_label label in
      self#latex_of_text fmt
        ((Latex latex) ::
         (to_text#text_of_value v))

    (** Print LaTeX code for a class attribute. *)
    method latex_of_attribute fmt a =
      self#latex_of_text fmt
        ((Latex (self#make_label (self#attribute_label a.att_value.val_name))) ::
         (to_text#text_of_attribute a))

    (** Print LaTeX code for a class method. *)
    method latex_of_method fmt m =
      self#latex_of_text fmt
        ((Latex (self#make_label (self#method_label m.met_value.val_name))) ::
         (to_text#text_of_method m))

    (** Print LaTeX code for the parameters of a type. *)
    method latex_of_type_params fmt m_name t =
      let print_one (p, co, cn) =
        ps fmt (Odoc_info.string_of_variance t (co,cn));
        ps fmt (self#normal_type m_name p)
      in
      match t.ty_parameters with
        [] -> ()
      | [(p,co,cn)] -> print_one (p, co, cn)
      | l ->
          ps fmt "(";
          print_concat fmt ", " print_one t.ty_parameters;
          ps fmt ")"

    method latex_of_class_parameter_list fmt father c =
      self#latex_of_text fmt
        (self#text_of_class_params father c)

    (** Print LaTeX code for a type. *)
    method latex_of_type fmt t =
      let s_name = Name.simple t.ty_name in
      let text =
        let (fmt2, flush2) = new_fmt () in
        Odoc_info.reset_type_names () ;
        let mod_name = Name.father t.ty_name in
        Format.fprintf fmt2 "@[<h 2>type ";
        self#latex_of_type_params fmt2 mod_name t;
        (match t.ty_parameters with [] -> () | _ -> ps fmt2 " ");
        ps fmt2 s_name;
        let priv = t.ty_private = Asttypes.Private in
        (
         match t.ty_manifest with
         | Some (Other typ) ->
             p fmt2 " = %s%s" (if priv then "private " else "") (self#normal_type mod_name typ)
         | _ -> ()
        );
        let s_type3 =
          p fmt2
            " %s"
            (
             match t.ty_kind with
               Type_abstract ->
                begin match t.ty_manifest with
                | Some (Object_type _) ->
                  "= " ^ (if priv then "private" else "") ^ " <"
                | _ -> ""
                end
             | Type_variant _ -> "="^(if priv then " private" else "")
             | Type_record _ -> "= "^(if priv then "private " else "")^"{"
             | Type_open -> "= .."
            ) ;
          flush2 ()
        in

        let defs =
          let entry_comment = function
          | None -> []
          | Some t ->
              let s =
                ps fmt2 "\\begin{ocamldoccomment}\n";
                self#latex_of_info fmt2 (Some t);
                ps fmt2 "\n\\end{ocamldoccomment}\n";
                flush2 ()
              in
              [ Latex s]
        in
          match t.ty_kind with
          | Type_abstract ->
             begin match t.ty_manifest with
             | Some (Object_type l) ->
               let fields =
                 List.map (fun r ->
                   let s_field =
                     p fmt2
                       "@[<h 6>  %s :@ %s ;"
                       r.of_name
                       (self#normal_type mod_name r.of_type);
                     flush2 ()
                   in
                   [ CodePre s_field ] @ (entry_comment r.of_text)
                 ) l
               in
               List.flatten fields @ [ CodePre ">" ]

             | None | Some (Other _) -> []
             end
          | Type_variant l ->
             let constructors =
               List.map (fun constr ->
                 let s_cons =
                   p fmt2 "@[<h 6>  | %s" constr.vc_name ;
                   begin match constr.vc_args, constr.vc_ret with
                   | [], None -> ()
                   | l, None ->
                     p fmt2 " of@ %s"
                       (self#normal_type_list ~par: false mod_name " * " l)
                   | [], Some r ->
                     p fmt2 " :@ %s"
                       (self#normal_type mod_name r)
                   | l, Some r ->
                     p fmt2 " :@ %s@ %s@ %s"
                       (self#normal_type_list ~par: false mod_name " * " l)
                       "->"
                       (self#normal_type mod_name r)
                   end ;
                   flush2 ()
                 in
                 [ CodePre s_cons ] @ (entry_comment constr.vc_text)
               ) l
             in
             List.flatten constructors
          | Type_record l ->
             let fields =
               List.map (fun r ->
                 let s_field =
                   p fmt2
                     "@[<h 6>  %s%s :@ %s ;"
                     (if r.rf_mutable then "mutable " else "")
                     r.rf_name
                     (self#normal_type mod_name r.rf_type);
                   flush2 ()
                 in
                 [ CodePre s_field ] @ (entry_comment r.rf_text)
               ) l
             in
             List.flatten fields @ [ CodePre "}" ]
          | Type_open ->
             (* FIXME ? *)
             []
        in
        let defs2 = (CodePre s_type3) :: defs in
        let rec iter = function
            [] -> []
          | [e] -> [e]
          | (CodePre s1) :: (CodePre s2) :: q ->
              iter ((CodePre (s1^"\n"^s2)) :: q)
          | e :: q ->
              e :: (iter q)
        in
        (iter defs2) @
        [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")] @
        (self#text_of_info t.ty_info)
      in
      self#latex_of_text fmt
        ((Latex (self#make_label (self#type_label t.ty_name))) :: text)

    (** Print LaTeX code for a type extension. *)
    method latex_of_type_extension mod_name fmt te =
      let text =
        let (fmt2, flush2) = new_fmt () in
        Odoc_info.reset_type_names () ;
        Format.fprintf fmt2 "@[<h 2>type ";
        (
          match te.te_type_parameters with
              [] -> ()
            | [p] ->
                ps fmt2 (self#normal_type mod_name p);
                ps fmt2 " "
            | l ->
                ps fmt2 "(";
                print_concat fmt2 ", " (fun p -> ps fmt2 (self#normal_type mod_name p)) l;
                ps fmt2 ") "
        );
        ps fmt2 (self#relative_idents mod_name te.te_type_name);
        p fmt2 " +=%s" (if te.te_private = Asttypes.Private then " private" else "") ;
        let s_type3 = flush2 () in
        let defs =
          (List.flatten
             (List.map
                (fun x ->
                   let father = Name.father x.xt_name in
                   let s_cons =
                     p fmt2 "@[<h 6>  | %s" (Name.simple x.xt_name);
                     (
                       match x.xt_args, x.xt_ret with
                           [], None -> ()
                         | l, None ->
                             p fmt2 " %s@ %s"
                               "of"
                               (self#normal_type_list ~par: false father " * " l)
                         | [], Some r ->
                             p fmt2 " %s@ %s"
                               ":"
                               (self#normal_type father r)
                         | l, Some r ->
                             p fmt2 " %s@ %s@ %s@ %s"
                               ":"
                               (self#normal_type_list ~par: false father " * " l)
                               "->"
                               (self#normal_type father r)
                     );
                     (
                       match x.xt_alias with
                           None -> ()
                         | Some xa ->
                             p fmt2 " = %s"
                               (
                                 match xa.xa_xt with
                                     None -> xa.xa_name
                                   | Some x -> x.xt_name
                               )
                     );
                     flush2 ()
                    in
                    [ Latex (self#make_label (self#extension_label x.xt_name));
                      CodePre s_cons ] @
                    (match x.xt_text with
                      None -> []
                    | Some t ->
                        let s =
                          ps fmt2 "\\begin{ocamldoccomment}\n";
                          self#latex_of_info fmt2 (Some t);
                          ps fmt2 "\n\\end{ocamldoccomment}\n";
                          flush2 ()
                        in
                        [ Latex s]
                    )
                  )
                  te.te_constructors
               )
              )
        in
        let defs2 = (CodePre s_type3) :: defs in
        let rec iter = function
            [] -> []
          | [e] -> [e]
          | (CodePre s1) :: (CodePre s2) :: q ->
              iter ((CodePre (s1^"\n"^s2)) :: q)
          | e :: q ->
              e :: (iter q)
        in
        (iter defs2) @
        (self#text_of_info te.te_info)
      in
      self#latex_of_text fmt text

    (** Print LaTeX code for an exception. *)
    method latex_of_exception fmt e =
      Odoc_info.reset_type_names () ;
      self#latex_of_text fmt
        ((Latex (self#make_label (self#exception_label e.ex_name))) ::
         (to_text#text_of_exception e))

    method latex_of_module_parameter fmt m_name p =
      self#latex_of_text fmt
        [
          Code "functor (";
          Code p.mp_name ;
          Code " : ";
        ] ;
      self#latex_of_module_type_kind fmt m_name p.mp_kind;
      self#latex_of_text fmt [ Code ") -> "]


    method latex_of_module_type_kind fmt father kind =
      match kind with
        Module_type_struct eles ->
          self#latex_of_text fmt [Latex "\\begin{ocamldocsigend}\n"];
          List.iter (self#latex_of_module_element fmt father) eles;
          self#latex_of_text fmt [Latex "\\end{ocamldocsigend}\n"]
      | Module_type_functor (p, k) ->
          self#latex_of_module_parameter fmt father p;
          self#latex_of_module_type_kind fmt father k
      | Module_type_alias a ->
          self#latex_of_text fmt
            [Code (self#relative_module_idents father a.mta_name)]
      | Module_type_with (k, s) ->
          self#latex_of_module_type_kind fmt father k;
          self#latex_of_text fmt
            [ Code " ";
              Code (self#relative_idents father s);
            ]
      | Module_type_typeof s ->
          self#latex_of_text fmt
            [ Code "module type of ";
              Code (self#relative_idents father s);
            ]

    method latex_of_module_kind fmt father kind =
      match kind with
        Module_struct eles ->
          self#latex_of_text fmt [Latex "\\begin{ocamldocsigend}\n"];
          List.iter (self#latex_of_module_element fmt father) eles;
          self#latex_of_text fmt [Latex "\\end{ocamldocsigend}\n"]
      | Module_alias a ->
          self#latex_of_text fmt
            [Code (self#relative_module_idents father a.ma_name)]
      | Module_functor (p, k) ->
          self#latex_of_module_parameter fmt father p;
          self#latex_of_module_kind fmt father k
      | Module_apply (k1, k2) ->
          (* TODO: l'application n'est pas correcte dans un .mli.
             Que faire ? -> afficher le module_type du typedtree  *)
          self#latex_of_module_kind fmt father k1;
          self#latex_of_text fmt [Code "("];
          self#latex_of_module_kind fmt father k2;
          self#latex_of_text fmt [Code ")"]
      | Module_with (k, s) ->
          (* TODO: a modifier quand Module_with sera plus detaille *)
          self#latex_of_module_type_kind fmt father k;
          self#latex_of_text fmt
            [ Code " ";
              Code (self#relative_idents father s) ;
            ]
      | Module_constraint (k, tk) ->
          (* TODO: on affiche quoi ? *)
          self#latex_of_module_kind fmt father k
      | Module_typeof s ->
          self#latex_of_text fmt
            [ Code "module type of ";
              Code (self#relative_idents father s);
            ]
      | Module_unpack (s, _) ->
          self#latex_of_text fmt
            [
              Code (self#relative_idents father s);
            ]

    method latex_of_class_kind fmt father kind =
      match kind with
        Class_structure (inh, eles) ->
          self#latex_of_text fmt [Latex "\\begin{ocamldocobjectend}\n"];
          self#generate_inheritance_info fmt inh;
          List.iter (self#latex_of_class_element fmt father) eles;
          self#latex_of_text fmt [Latex "\\end{ocamldocobjectend}\n"]

      | Class_apply capp ->
          (* TODO: afficher le type final a partir du typedtree *)
          self#latex_of_text fmt [Raw "class application not handled yet"]

      | Class_constr cco ->
          (
           match cco.cco_type_parameters with
             [] -> ()
           | l ->
               self#latex_of_text fmt
                 (
                  Code "[" ::
                  (self#text_of_class_type_param_expr_list father l) @
                  [Code "] "]
                 )
          );
          self#latex_of_text fmt
            [Code (self#relative_idents father cco.cco_name)]

      | Class_constraint (ck, ctk) ->
          self#latex_of_text fmt [Code "( "] ;
          self#latex_of_class_kind fmt father ck;
          self#latex_of_text fmt [Code " : "] ;
          self#latex_of_class_type_kind fmt father ctk;
          self#latex_of_text fmt [Code " )"]

    method latex_of_class_type_kind fmt father kind =
      match kind with
        Class_type cta ->
          (
           match cta.cta_type_parameters with
             [] -> ()
           | l ->
               self#latex_of_text fmt
                 (Code "[" ::
                  (self#text_of_class_type_param_expr_list father l) @
                  [Code "] "]
                 )
          );
          self#latex_of_text fmt
            [Code (self#relative_idents father cta.cta_name)]

      | Class_signature (inh, eles) ->
          self#latex_of_text fmt [Latex "\\begin{ocamldocobjectend}\n"];
          self#generate_inheritance_info fmt inh;
          List.iter (self#latex_of_class_element fmt father) eles;
          self#latex_of_text fmt [Latex "\\end{ocamldocobjectend}\n"]

    method latex_for_module_index fmt m =
      let s_name = Name.simple m.m_name in
      self#latex_of_text fmt
        [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^
                (self#label ~no_:false s_name)^"`}\n"
               )
        ]

    method latex_for_module_type_index fmt mt =
      let s_name = Name.simple mt.mt_name in
      self#latex_of_text fmt
        [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^
                (self#label ~no_:false (Name.simple s_name))^"`}\n"
               )
        ]

    method latex_for_module_label fmt m =
      ps fmt (self#make_label (self#module_label m.m_name))

    method latex_for_module_type_label fmt mt =
      ps fmt (self#make_label (self#module_type_label mt.mt_name))


    method latex_for_class_index fmt c =
      let s_name = Name.simple c.cl_name in
      self#latex_of_text fmt
        [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^
                (self#label ~no_:false s_name)^"`}\n"
               )
        ]

    method latex_for_class_type_index fmt ct =
      let s_name = Name.simple ct.clt_name in
      self#latex_of_text fmt
        [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^
                (self#label ~no_:false s_name)^"`}\n"
               )
        ]

    method latex_for_class_label fmt c =
      ps fmt (self#make_label (self#class_label c.cl_name))

    method latex_for_class_type_label fmt ct =
      ps fmt (self#make_label (self#class_type_label ct.clt_name))

    (** Print the LaTeX code for the given module. *)
    method latex_of_module fmt m =
      let father = Name.father m.m_name in
      let t =
        [
          Latex "\\begin{ocamldoccode}\n" ;
          Code "module ";
          Code (Name.simple m.m_name);
          Code " : ";
        ]
      in
      self#latex_of_text fmt t;
      self#latex_of_text fmt [ Latex "\\end{ocamldoccode}\n" ];
      self#latex_for_module_label fmt m;
      self#latex_for_module_index fmt m;
      p fmt "@[<h 4>";
      self#latex_of_module_kind fmt father m.m_kind;
      (
       match Module.module_is_functor m with
         false -> ()
       | true ->
           self#latex_of_text fmt  [Newline];
           (
            match List.filter (fun (_,d) -> d <> None)
                (module_parameters ~trans: false m)
            with
              [] -> ()
            | l ->
                let t =
                  [ Bold [Raw "Parameters: "];
                    List
                      (List.map
                         (fun (p,text_opt) ->
                           let t = match text_opt with None -> [] | Some t -> t in
                           ( Raw p.mp_name :: Raw ": " :: t)
                         )
                         l
                      )
                  ]
           in
           self#latex_of_text fmt t
           );
      );
      self#latex_of_text fmt [Newline];
      self#latex_of_info fmt ~block: true m.m_info;
      p fmt "@]";


    (** Print the LaTeX code for the given module type. *)
    method latex_of_module_type fmt mt =
      let father = Name.father mt.mt_name in
      let t =
        [
          Latex "\\begin{ocamldoccode}\n" ;
          Code "module type " ;
          Code (Name.simple mt.mt_name);
        ]
      in
      self#latex_of_text fmt t;
      (
       match mt.mt_type, mt.mt_kind with
       | Some mtyp, Some kind ->
           self#latex_of_text fmt [ Code " = " ];
           self#latex_of_text fmt [ Latex "\\end{ocamldoccode}\n" ];
           self#latex_for_module_type_label fmt mt;
           self#latex_for_module_type_index fmt mt;
           p fmt "@[<h 4>";
           self#latex_of_module_type_kind fmt father kind
       | _ ->
           self#latex_of_text fmt [ Latex "\\end{ocamldoccode}\n" ];
           self#latex_for_module_type_index fmt mt;
           p fmt "@[<h 4>";
      );
      (
       match Module.module_type_is_functor mt with
         false -> ()
       | true ->
           self#latex_of_text fmt [Newline];
           (
            match List.filter (fun (_,d) -> d <> None)
                (module_type_parameters ~trans: false mt)
            with
              [] -> ()
            | l ->
                let t =
                  [ Bold [Raw "Parameters: "];
                    List
                      (List.map
                         (fun (p,text_opt) ->
                           let t = match text_opt with None -> [] | Some t -> t in
                           ( Raw p.mp_name :: Raw ": " :: t)
                         )
                         l
                      )
                  ]
                in
                self#latex_of_text fmt t
           );
      );
      self#latex_of_text fmt [Newline];
      self#latex_of_info fmt ~block: true mt.mt_info;
      p fmt "@]";

    (** Print the LaTeX code for the given included module. *)
    method latex_of_included_module fmt im =
      self#latex_of_text fmt
        ((Code "include ") ::
         (Code
            (match im.im_module with
              None -> im.im_name
            | Some (Mod m) -> m.m_name
            | Some (Modtype mt) -> mt.mt_name)
         ) ::
         (self#text_of_info im.im_info)
        )

    (** Print the LaTeX code for the given class. *)
    method latex_of_class fmt c =
      Odoc_info.reset_type_names () ;
      let father = Name.father c.cl_name in
      let type_params =
        match c.cl_type_parameters with
          [] -> ""
        | l -> (self#normal_class_type_param_list father l)^" "
      in
      let t =
        [
          Latex "\\begin{ocamldoccode}\n" ;
          Code (Printf.sprintf
                  "class %s%s%s : "
                  (if c.cl_virtual then "virtual " else "")
                  type_params
                  (Name.simple c.cl_name)
               )
        ]
      in
      self#latex_of_text fmt t;
      self#latex_of_class_parameter_list fmt father c;
      (* avoid a big gap if the kind is a consrt *)
      (
       match c.cl_kind with
         Class.Class_constr _ ->
           self#latex_of_class_kind fmt father c.cl_kind
       | _ ->
           ()
      );
      self#latex_of_text fmt [ Latex "\\end{ocamldoccode}\n" ];
      self#latex_for_class_label fmt c;
      self#latex_for_class_index fmt c;
      p fmt "@[<h 4>";
      (match c.cl_kind with
        Class.Class_constr _ -> ()
      |        _ -> self#latex_of_class_kind fmt father c.cl_kind
      );
      self#latex_of_text fmt [Newline];
      self#latex_of_info fmt ~block: true c.cl_info;
      p fmt "@]"

    (** Print the LaTeX code for the given class type. *)
    method latex_of_class_type fmt ct =
      Odoc_info.reset_type_names () ;
      let father = Name.father ct.clt_name in
      let type_params =
        match ct.clt_type_parameters with
          [] -> ""
        | l -> (self#normal_class_type_param_list father l)^" "
      in
      let t =
        [
          Latex "\\begin{ocamldoccode}\n" ;
          Code (Printf.sprintf
                  "class type %s%s%s = "
                  (if ct.clt_virtual then "virtual " else "")
                  type_params
                  (Name.simple ct.clt_name)
               )
        ]
      in
      self#latex_of_text fmt t;

      self#latex_of_text fmt [ Latex "\\end{ocamldoccode}\n" ];
      self#latex_for_class_type_label fmt ct;
      self#latex_for_class_type_index fmt ct;
      p fmt "@[<h 4>";
      self#latex_of_class_type_kind fmt father ct.clt_kind;
      self#latex_of_text fmt [Newline];
      self#latex_of_info fmt ~block: true ct.clt_info;
      p fmt "@]"

    (** Print the LaTeX code for the given class element. *)
    method latex_of_class_element fmt class_name class_ele =
      self#latex_of_text fmt [Newline];
      match class_ele with
        Class_attribute att -> self#latex_of_attribute fmt att
      | Class_method met -> self#latex_of_method fmt met
      | Class_comment t ->
          match t with
          | [] -> ()
          | (Title (_,_,_)) :: _ -> self#latex_of_text fmt t
          | _ -> self#latex_of_text fmt [ Title ((Name.depth class_name) + 2, None, t) ]

    (** Print the LaTeX code for the given module element. *)
    method latex_of_module_element fmt module_name module_ele =
      self#latex_of_text fmt [Newline];
      match module_ele with
        Element_module m -> self#latex_of_module fmt m
      | Element_module_type mt -> self#latex_of_module_type fmt mt
      | Element_included_module im -> self#latex_of_included_module fmt im
      | Element_class c -> self#latex_of_class fmt c
      | Element_class_type ct -> self#latex_of_class_type fmt ct
      | Element_value v -> self#latex_of_value fmt v
      | Element_type_extension te -> self#latex_of_type_extension module_name fmt te
      | Element_exception e -> self#latex_of_exception fmt e
      | Element_type t -> self#latex_of_type fmt t
      | Element_module_comment t -> self#latex_of_text fmt t

    (** Generate the LaTeX code for the given list of inherited classes.*)
    method generate_inheritance_info fmt inher_l =
      let f inh =
        match inh.ic_class with
          None -> (* we can't make the reference *)
            Newline ::
            Code ("inherit "^inh.ic_name) ::
            (match inh.ic_text with
              None -> []
            | Some t -> Newline :: t
            )
        | Some cct ->
            let label =
              match cct with
                Cl _ -> self#class_label inh.ic_name
              | Cltype _ -> self#class_type_label inh.ic_name
            in
            (* we can create the reference *)
            Newline ::
            Odoc_info.Code ("inherit "^inh.ic_name) ::
            (Odoc_info.Latex (" ["^(self#make_ref label)^"]")) ::
            (match inh.ic_text with
              None -> []
            | Some t -> Newline :: t
            )
      in
      List.iter (self#latex_of_text fmt) (List.map f inher_l)

    (** Generate the LaTeX code for the inherited classes of the given class. *)
    method generate_class_inheritance_info fmt cl =
      let rec iter_kind k =
        match k with
          Class_structure ([], _) ->
            ()
        | Class_structure (l, _) ->
            self#generate_inheritance_info fmt l
        | Class_constraint (k, _) ->
            iter_kind k
        | Class_apply _
        | Class_constr _ ->
            ()
      in
      iter_kind cl.cl_kind

    (** Generate the LaTeX code for the inherited classes of the given class type. *)
    method generate_class_type_inheritance_info fmt clt =
      match clt.clt_kind with
        Class_signature ([], _) ->
          ()
      | Class_signature (l, _) ->
          self#generate_inheritance_info fmt l
      | Class_type _ ->
          ()

    (** Generate the LaTeX code for the given top module, in the given buffer. *)
    method generate_for_top_module fmt m =
      let (first_t, rest_t) = self#first_and_rest_of_info m.m_info in
      let text =
        if m.m_text_only then
          [ Title (1, None, [Raw m.m_name]  @
                   (match first_t with
                     [] -> []
                   | t -> (Raw " : ") :: t)
                  ) ;
          ]
        else
          [ Title (1, None,
                   [ Raw (Odoc_messages.modul^" ") ; Code m.m_name ] @
                   (match first_t with
                     [] -> []
                   | t -> (Raw " : ") :: t)) ;
          ]
      in
      self#latex_of_text fmt text;
      self#latex_for_module_label fmt m;
      self#latex_for_module_index fmt m;
      self#latex_of_text fmt rest_t ;

      self#latex_of_text fmt [ Newline ] ;
      if not m.m_text_only then ps fmt "\\ocamldocvspace{0.5cm}\n\n";
      List.iter
        (fun ele ->
          self#latex_of_module_element fmt m.m_name ele;
          ps fmt "\n\n"
        )
        (Module.module_elements ~trans: false m)

    (** Print the header of the TeX document. *)
    method latex_header fmt module_list =
      ps fmt "\\documentclass[11pt]{article} \n";
      ps fmt "\\usepackage[latin1]{inputenc} \n";
      ps fmt "\\usepackage[T1]{fontenc} \n";
      ps fmt "\\usepackage{textcomp}\n";
      ps fmt "\\usepackage{fullpage} \n";
      ps fmt "\\usepackage{url} \n";
      ps fmt "\\usepackage{ocamldoc}\n";
      (
       match !Global.title with
         None -> ()
       | Some s ->
           ps fmt "\\title{";
           ps fmt (self#escape s);
           ps fmt "}\n"
      );
      ps fmt "\\begin{document}\n";
      (match !Global.title with
        None -> () |
        Some _ -> ps fmt "\\maketitle\n"
      );
      if !Global.with_toc then ps fmt "\\tableofcontents\n";
      (
       let info = Odoc_info.apply_opt
           (Odoc_info.info_of_comment_file module_list)
           !Odoc_info.Global.intro_file
       in
       (match info with None -> () | Some _ -> ps fmt "\\vspace{0.2cm}");
       self#latex_of_info fmt info;
       (match info with None -> () | Some _ -> ps fmt "\n\n")
      )


    (** Generate the LaTeX style file, if it does not exists. *)
    method generate_style_file =
      try
        let dir = Filename.dirname !Global.out_file in
        let file = Filename.concat dir "ocamldoc.sty" in
        if Sys.file_exists file then
          Odoc_info.verbose (Odoc_messages.file_exists_dont_generate file)
        else
          (
           let chanout = open_out file in
           output_string chanout Odoc_latex_style.content ;
           flush chanout ;
           close_out chanout;
           Odoc_info.verbose (Odoc_messages.file_generated file)
          )
      with
        Sys_error s ->
          prerr_endline s ;
          incr Odoc_info.errors ;

    (** Generate the LaTeX file from a module list, in the {!Odoc_info.Global.out_file} file. *)
    method generate module_list =
      self#generate_style_file ;
      let main_file = !Global.out_file in
      let dir = Filename.dirname main_file in
      if !separate_files then
        (
         let f m =
           try
             let chanout =
               open_out ((Filename.concat dir (Name.simple m.m_name))^".tex")
             in
             let fmt = Format.formatter_of_out_channel chanout in
             self#generate_for_top_module fmt m ;
             Format.pp_print_flush fmt ();
             close_out chanout
           with
             Failure s
           | Sys_error s ->
               prerr_endline s ;
               incr Odoc_info.errors
         in
         List.iter f module_list
        );

      try
        let chanout = open_out main_file in
        let fmt = Format.formatter_of_out_channel chanout in
        if !Global.with_header then self#latex_header fmt module_list;
        List.iter
          (fun m ->
            if !separate_files then
              ps fmt ("\\input{"^((Name.simple m.m_name))^".tex}\n")
            else
              self#generate_for_top_module fmt m
          )
          module_list ;
        if !Global.with_trailer then ps fmt "\\end{document}";
        Format.pp_print_flush fmt ();
        close_out chanout
      with
        Failure s
      | Sys_error s ->
          prerr_endline s ;
          incr Odoc_info.errors
  end
end

module type Latex_generator = module type of Generator
