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

(** The man pages generator. *)
open Odoc_info
open Parameter
open Value
open Type
open Extension
open Exception
open Class
open Module
open Search

let man_suffix = ref Odoc_messages.default_man_suffix
let man_section = ref Odoc_messages.default_man_section

let man_mini = ref false

let new_buf () = Buffer.create 1024
let bp = Printf.bprintf
let bs = Buffer.add_string

let linebreak = "\n.sp\n";;

(** A class used to get a [text] for info structures. *)
class virtual info =
  object (self)
    (** The list of pairs [(tag, f)] where [f] is a function taking
       the [text] associated to [tag] and returning man code.
       Add a pair here to handle a tag.*)
    val mutable tag_functions = ([] : (string * (Odoc_info.text -> string)) list)

    (** Return man code for a [text]. *)
    method virtual man_of_text : Buffer.t -> Odoc_info.text -> unit

    method str_man_of_text t =
      let b = Buffer.create 256 in
      self#man_of_text b t ;
      Buffer.contents b

    (** Print groff string for an author list. *)
    method str_man_of_author_list l =
      match l with
        [] -> ""
      | _ ->
          let b = Buffer.create 256 in
          bs b ".B \"";
          bs b Odoc_messages.authors;
          bs b "\"\n:\n";
          bs b (String.concat ", " l);
          bs b "\n";
          (*bs b "\n.sp\n"*)
          Buffer.contents b

    (** Print groff string for the given optional version information.*)
    method str_man_of_version_opt v_opt =
      match v_opt with
        None -> ""
      | Some v ->
          let b = Buffer.create 256 in
          bs b ".B \"";
          bs b Odoc_messages.version;
          bs b "\"\n:\n";
          bs b v;
          bs b "\n";
          (*".sp\n"*)
          Buffer.contents b

    (** Printf groff string for the \@before information. *)
    method str_man_of_before = function
      [] -> ""
    | l ->
        let b = Buffer.create 256 in
        let rec iter = function
          [] -> ()
        | (v, text) :: q ->
             bp b ".B \"%s" Odoc_messages.before;
             bs b v;
             bs b "\"\n";
             self#man_of_text b text;
             bs b "\n";
             bs b "\n";
             match q with
               [] -> ()
             | _ -> bs b linebreak ; iter q
        in
        iter l;
        Buffer.contents b

    (** Print groff string for the given optional since information.*)
    method str_man_of_since_opt s_opt =
      match s_opt with
        None -> ""
      | Some s ->
          let b = Buffer.create 256 in
          bs b ".B \"";
          bs b Odoc_messages.since;
          bs b "\"\n";
          bs b s;
          bs b "\n";(*".sp\n"*)
          Buffer.contents b

    (** Print groff string for the given list of raised exceptions.*)
    method str_man_of_raised_exceptions l =
      match l with
        [] -> ""
      | _ ->
          let b = Buffer.create 256 in
          let rec iter = function
            [] -> ()
          | (s, t) :: q ->
              bs b ".B \"";
              bs b Odoc_messages.raises;
              bs b (" "^s^"\"\n");
              self#man_of_text b t;
              bs b "\n";
              match q with
                [] -> ()
              | _ -> bs b linebreak; iter q
          in
          iter l;
          Buffer.contents b

    (** Print groff string for the given "see also" reference. *)
    method str_man_of_see (see_ref, t)  =
      let t_ref =
        match see_ref with
          Odoc_info.See_url s -> [ Odoc_info.Link (s, t) ]
        | Odoc_info.See_file s -> (Odoc_info.Code s) :: (Odoc_info.Raw " ") :: t
        | Odoc_info.See_doc s -> (Odoc_info.Italic [Odoc_info.Raw s]) :: (Odoc_info.Raw " ") :: t
      in
      self#str_man_of_text t_ref

    (** Print groff string for the given list of "see also" references.*)
    method str_man_of_sees l =
      match l with
        [] -> ""
      | _ ->
          let b = Buffer.create 256 in
          let rec iter = function
            [] -> ()
          | see :: q ->
              bs b ".B \"";
              bs b Odoc_messages.see_also;
              bs b "\"\n";
              bs b (self#str_man_of_see see);
              bs b "\n";
              match q with
                [] -> ()
              | _ -> bs b linebreak; iter q
          in
          iter l;
          Buffer.contents b

    (** Print groff string for the given optional return information.*)
    method str_man_of_return_opt return_opt =
      match return_opt with
        None -> ""
      | Some s ->
          let b = Buffer.create 256 in
          bs b ".B ";
          bs b Odoc_messages.returns;
          bs b "\n";
          self#man_of_text b s;
          bs b "\n";
          Buffer.contents b

    (** Print man code for the given list of custom tagged texts. *)
    method str_man_of_custom l =
      List.fold_left
        (fun acc (tag, text) ->
           try
             let f = List.assoc tag tag_functions in
             let buf = Buffer.create 50 in
             Buffer.add_string buf (f text);
             (Buffer.contents buf) :: acc
          with
            Not_found ->
              Odoc_info.warning (Odoc_messages.tag_not_handled tag);
              acc
        )
        [] l

    (** Print the groff string to display an optional info structure. *)
    method man_of_info ?(margin=0) b info_opt =
        match info_opt with
        None -> ()
      | Some info ->
          let module M = Odoc_info in
          let l =
            (
           match info.M.i_deprecated with
             None -> []
           | Some d ->
               let b = Buffer.create 256 in
               bs b ".B \"";
               bs b Odoc_messages.deprecated;
               bs b "\"\n";
               self#man_of_text b d;
               bs b "\n";
               [ Buffer.contents b ]
            ) @
              (
               match info.M.i_desc with
                 None -> []
               | Some d when d = [Odoc_info.Raw ""] -> []
               | Some d ->
                   [ (self#str_man_of_text d)^"\n" ]
              ) @
              [
                self#str_man_of_author_list info.M.i_authors;
                self#str_man_of_version_opt info.M.i_version;
                self#str_man_of_before info.M.i_before;
                self#str_man_of_since_opt info.M.i_since;
                self#str_man_of_raised_exceptions info.M.i_raised_exceptions;
                self#str_man_of_return_opt info.M.i_return_value;
                self#str_man_of_sees info.M.i_sees;
              ] @
                (self#str_man_of_custom info.M.i_custom)
          in
          let l = List.filter ((<>) "") l in
          Buffer.add_string b (String.concat "\n.sp\n" l)
  end

module Generator =
struct

(** This class is used to create objects which can generate a simple html documentation. *)
class man =
  let re_slash = Str.regexp_string "/" in
  object (self)
    inherit info

    (** Get a file name from a complete name. *)
    method file_name name =
      let s = Printf.sprintf "%s.%s" name !man_suffix in
      Str.global_replace re_slash "slash" s

    (** Escape special sequences of characters in a string. *)
    method escape (s : string) =
      let len = String.length s in
      let b = Buffer.create len in
      for i = 0 to len - 1 do
        match s.[i] with
          '\\' -> Buffer.add_string b "\\(rs"
        | '.' -> Buffer.add_string b "\\&."
        | '\'' -> Buffer.add_string b "\\&'"
        | '-' -> Buffer.add_string b "\\-"
        | c -> Buffer.add_char b c
      done;
      Buffer.contents b

    (** Open a file for output. Add the target directory.*)
    method open_out file =
      let f = Filename.concat !Global.target_dir file in
      open_out f

    (** Print groff string for a text, without correction of blanks. *)
    method private man_of_text2 b t =
      List.iter (self#man_of_text_element b) t

    (** Print the groff string for a text, with blanks corrected. *)
    method man_of_text b t =
      let b2 = new_buf () in
      self#man_of_text2 b2 t ;
      let s = Buffer.contents b2 in
      let s2 = Str.global_replace (Str.regexp "\n[ ]*") "\n" s in
      bs b (Str.global_replace (Str.regexp "\n\n") "\n" s2)

    (** Return the given string without no newlines. *)
    method remove_newlines s =
      Str.global_replace (Str.regexp "[ ]*\n[ ]*") " " s

    (** Print the groff string for a text element. *)
    method man_of_text_element b txt =
      match txt with
      | Odoc_info.Raw s -> bs b (self#escape s)
      | Odoc_info.Code s ->
          bs b "\n.B ";
          bs b ((Str.global_replace (Str.regexp "\n") "\n.B " (self#escape s))^"\n")
      | Odoc_info.CodePre s ->
          bs b "\n.B ";
          bs b ((Str.global_replace (Str.regexp "\n") "\n.B " (self#escape s))^"\n")
      | Odoc_info.Verbatim s ->
          bs b (self#escape s)
      | Odoc_info.Bold t
      | Odoc_info.Italic t
      | Odoc_info.Emphasize t
      | Odoc_info.Center t
      | Odoc_info.Left t
      | Odoc_info.Right t ->
          self#man_of_text2 b t
      | Odoc_info.List tl ->
          List.iter
            (fun t -> bs b "\n.sp\n \\-"; self#man_of_text2 b t; bs b "\n")
            tl;
          bs b "\n"
      | Odoc_info.Enum tl ->
          List.iter
            (fun t -> bs b "\n.sp\n \\-"; self#man_of_text2 b t; bs b "\n")
            tl;
          bs b "\n"
      | Odoc_info.Newline ->
          bs b "\n.sp\n"
      | Odoc_info.Block t ->
          bs b "\n.sp\n";
          self#man_of_text2 b t;
          bs b "\n.sp\n"
      | Odoc_info.Title (n, l_opt, t) ->
          self#man_of_text2 b [Odoc_info.Code (Odoc_info.string_of_text t)]
      | Odoc_info.Latex _ ->
          (* don't care about LaTeX stuff in HTML. *)
          ()
      | Odoc_info.Link (s, t) ->
          self#man_of_text2 b t
      | Odoc_info.Ref (name, _, _) ->
          self#man_of_text_element b
            (Odoc_info.Code (Odoc_info.use_hidden_modules name))
      | Odoc_info.Superscript t ->
          bs b "^{"; self#man_of_text2 b t
      | Odoc_info.Subscript t ->
          bs b "_{"; self#man_of_text2 b t
      | Odoc_info.Module_list _ ->
          ()
      | Odoc_info.Index_list ->
          ()
      | Odoc_info.Custom (s,t) -> self#man_of_custom_text b s t
      | Odoc_info.Target (target, code) -> self#man_of_Target b ~target ~code

    method man_of_custom_text b s t = ()

    method man_of_Target b ~target ~code =
      if String.lowercase target = "man" then bs b code else ()

    (** Print groff string to display code. *)
    method man_of_code b s = self#man_of_text b [ Code s ]

    (** Take a string and return the string where fully qualified idents
       have been replaced by idents relative to the given module name.*)
    method relative_idents m_name s =
      let f str_t =
        let match_s = Str.matched_string str_t in
        Odoc_info.apply_if_equal
          Odoc_info.use_hidden_modules
          match_s
          (Name.get_relative m_name match_s)
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Print groff string to display a [Types.type_expr].*)
    method man_of_type_expr b m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_print.string_of_type_expr t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      bs b "\n.B ";
      bs b (self#relative_idents m_name s2);
      bs b "\n"

    (** Print groff string to display a [Types.class_type].*)
    method man_of_class_type_expr b m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_print.string_of_class_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      bs b "\n.B ";
      bs b (self#relative_idents m_name s2);
      bs b "\n"

    (** Print groff string to display a [Types.type_expr list].*)
    method man_of_type_expr_list ?par b m_name sep l =
      let s = Odoc_str.string_of_type_list ?par sep l in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      bs b "\n.B ";
      bs b (self#relative_idents m_name s2);
      bs b "\n"

    (** Print groff string to display the parameters of a type.*)
    method man_of_type_expr_param_list b m_name t =
      match t.ty_parameters with
        [] -> ()
      | l ->
          let s = Odoc_str.string_of_type_param_list t in
          let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
          bs b "\n.B ";
          bs b (self#relative_idents m_name s2);
          bs b "\n"

    (** Print groff string to display a [Types.module_type]. *)
    method man_of_module_type b m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_print.string_of_module_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      bs b "\n.B ";
      bs b (self#relative_idents m_name s2);
      bs b "\n"

    (** Print groff string code for a value. *)
    method man_of_value b v =
      Odoc_info.reset_type_names () ;
      bs b "\n.I val ";
      bs b (Name.simple v.val_name);
      bs b " \n: ";
      self#man_of_type_expr b (Name.father v.val_name) v.val_type;
      bs b ".sp\n";
      self#man_of_info b v.val_info;
      bs b "\n.sp\n"

    (** Print groff string code for a type extension. *)
    method man_of_type_extension b m_name te =
      Odoc_info.reset_type_names () ;
      bs b ".I type ";
      (
        match te.te_type_parameters with
            [] -> ()
          | l ->
              let s = Odoc_str.string_of_type_extension_param_list te in
              let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
                bs b "\n.B ";
                bs b (self#relative_idents m_name s2);
                bs b "\n";
                bs b ".I "
      );
      bs b (self#relative_idents m_name te.te_type_name);
      bs b " \n";
      bs b "+=";
      if te.te_private = Asttypes.Private then bs b " private";
      bs b "\n ";
      List.iter
        (fun x ->
           let father = Name.father x.xt_name in
           bs b ("| "^(Name.simple x.xt_name));
           (
             match x.xt_args, x.xt_ret with
               | [], None -> bs b "\n"
               | l, None ->
                   bs b "\n.B of ";
                   self#man_of_type_expr_list ~par: false b father " * " l;
               | [], Some r ->
                   bs b "\n.B : ";
                   self#man_of_type_expr b father r;
               | l, Some r ->
                   bs b "\n.B : ";
                   self#man_of_type_expr_list ~par: false b father " * " l;
                   bs b ".B -> ";
                   self#man_of_type_expr b father r;
           );
           (
             match x.xt_alias with
                 None -> ()
               | Some xa ->
                   bs b ".B = ";
                   bs b
                     (
                       match xa.xa_xt with
                           None -> xa.xa_name
                         | Some x -> x.xt_name
                     );
                   bs b "\n"
           );
           (
             match x.xt_text with
                 None ->
                   bs b " "
               | Some t ->
                   bs b ".I \"  \"\n";
                   bs b "(* ";
                   self#man_of_info b (Some t);
                   bs b " *)\n "
           )
        )
        te.te_constructors;
      bs b "\n.sp\n";
      self#man_of_info b te.te_info;
      bs b "\n.sp\n"

    (** Print groff string code for an exception. *)
    method man_of_exception b e =
      Odoc_info.reset_type_names () ;
      bs b "\n.I exception ";
      bs b (Name.simple e.ex_name);
      bs b " \n";
      (
        match e.ex_args, e.ex_ret with
        | [], None -> ()
        | l, None ->
           bs b ".B of ";
           self#man_of_type_expr_list
             ~par: false
             b (Name.father e.ex_name) " * " e.ex_args
        | [], Some r ->
            bs b ".B : ";
            self#man_of_type_expr b (Name.father e.ex_name) r
        | l, Some r ->
            bs b ".B : ";
            self#man_of_type_expr_list
                   ~par: false
                   b (Name.father e.ex_name) " * " l;
            bs b ".B -> ";
            self#man_of_type_expr b (Name.father e.ex_name) r
      );
      (
       match e.ex_alias with
         None -> ()
       | Some ea ->
           bs b " = ";
           bs b
             (
              match ea.ea_ex with
                None -> ea.ea_name
              | Some e -> e.ex_name
             )
      );
      bs b "\n.sp\n";
      self#man_of_info b e.ex_info;
      bs b "\n.sp\n"

    (** Print groff string for a type. *)
    method man_of_type b t =
      Odoc_info.reset_type_names () ;
      let father = Name.father t.ty_name in
      let field_comment = function
        | None -> ()
        | Some t ->
          bs b "  (* ";
          self#man_of_info b (Some t);
          bs b " *) "
      in
      bs b ".I type ";
      self#man_of_type_expr_param_list b father t;
      (
       match t.ty_parameters with
         [] -> ()
       | _ -> bs b ".I "
      );
      bs b (Name.simple t.ty_name);
      bs b " \n";
      let priv = t.ty_private = Asttypes.Private in
      (
       match t.ty_manifest with
         None -> ()
       | Some (Object_type l) ->
          bs b "= ";
          if priv then bs b "private ";
          bs b "<";
          List.iter (fun r ->
            bs b (r.of_name^" : ");
            self#man_of_type_expr b father r.of_type;
            bs b ";";
            field_comment r.of_text ;
          ) l;
          bs b "\n >\n"
       | Some (Other typ) ->
           bs b "= ";
           if priv then bs b "private ";
           self#man_of_type_expr b father typ
      );
      (
       match t.ty_kind with
        Type_abstract -> ()
      | Type_variant l ->
         bs b "=";
         if priv then bs b " private";
         bs b "\n ";
         List.iter (fun constr ->
           bs b ("| "^constr.vc_name);
           let print_text t =
             bs b "  (* ";
             self#man_of_info b (Some t);
             bs b " *)\n "
           in
           match constr.vc_args, constr.vc_text,constr.vc_ret with
           | [], None, None -> bs b "\n "
           | [], (Some t), None ->
             print_text t
           | l, None, None ->
             bs b "\n.B of ";
             self#man_of_type_expr_list ~par: false b father " * " l;
             bs b " "
           | l, (Some t), None ->
             bs b "\n.B of ";
             self#man_of_type_expr_list ~par: false b father " * " l;
             bs b ".I \"  \"\n";
             print_text t
           | [], None, Some r ->
             bs b "\n.B : ";
             self#man_of_type_expr b father r;
             bs b " "
           | [], (Some t), Some r ->
             bs b "\n.B : ";
             self#man_of_type_expr b father r;
             bs b ".I \"  \"\n";
             print_text t
           | l, None, Some r ->
             bs b "\n.B : ";
             self#man_of_type_expr_list ~par: false b father " * " l;
             bs b ".B -> ";
             self#man_of_type_expr b father r;
             bs b " "
           | l, (Some t), Some r ->
             bs b "\n.B of ";
             self#man_of_type_expr_list ~par: false b father " * " l;
             bs b ".B -> ";
             self#man_of_type_expr b father r;
             bs b ".I \"  \"\n";
             print_text t
         ) l

      | Type_record l ->
          bs b "= ";
          if priv then bs b "private ";
          bs b "{";
           List.iter (fun r ->
             bs b (if r.rf_mutable then "\n\n.B mutable \n" else "\n ");
             bs b (r.rf_name^" : ");
             self#man_of_type_expr b father r.rf_type;
             bs b ";";
             field_comment r.rf_text ;
           ) l;
          bs b "\n }\n"
      | Type_open ->
          bs b "= ..";
          bs b "\n"
      );
      bs b "\n.sp\n";
      self#man_of_info b t.ty_info;
      bs b "\n.sp\n"

    (** Print groff string for a class attribute. *)
    method man_of_attribute b a =
      bs b ".I val ";
      if a.att_virtual then bs b ("virtual ");
      if a.att_mutable then bs b (Odoc_messages.mutab^" ");
      bs b ((Name.simple a.att_value.val_name)^" : ");
      self#man_of_type_expr b (Name.father a.att_value.val_name) a.att_value.val_type;
      bs b "\n.sp\n";
      self#man_of_info b a.att_value.val_info;
      bs b "\n.sp\n"

    (** Print groff string for a class method. *)
    method man_of_method b m =
      bs b ".I method ";
      if m.met_private then bs b "private ";
      if m.met_virtual then bs b "virtual ";
      bs b ((Name.simple m.met_value.val_name)^" : ");
      self#man_of_type_expr b
        (Name.father m.met_value.val_name) m.met_value.val_type;
      bs b "\n.sp\n";
      self#man_of_info b m.met_value.val_info;
      bs b "\n.sp\n"

    (** Groff for a list of parameters. *)
    method man_of_parameter_list b m_name l =
      match l with
        [] -> ()
      | _ ->
          bs b "\n.B ";
          bs b Odoc_messages.parameters;
          bs b ": \n";
          List.iter
            (fun p ->
              bs b ".sp\n";
              bs b "\"";
              bs b (Parameter.complete_name p);
              bs b "\"\n";
              self#man_of_type_expr b m_name (Parameter.typ p);
              bs b "\n";
              self#man_of_parameter_description b p;
              bs b "\n"
            )
            l;
          bs b "\n"

    (** Groff for the description of a function parameter. *)
    method man_of_parameter_description b p =
      match Parameter.names p with
        [] -> ()
      | name :: [] ->
          (
           (* Only one name, no need for label for the description. *)
           match Parameter.desc_by_name p name with
             None -> ()
           | Some t -> bs b "\n "; self#man_of_text b t
          )
      | l ->
          (*  A list of names, we display those with a description. *)
          List.iter
            (fun n ->
              match Parameter.desc_by_name p n with
                None -> ()
              | Some t ->
                  self#man_of_code b (n^" : ");
                  self#man_of_text b t
            )
            l

    (** Print groff string for a list of module parameters. *)
    method man_of_module_parameter_list b m_name l =
      match l with
        [] -> ()
      | _ ->
          bs b ".B \"";
          bs b Odoc_messages.parameters;
          bs b ":\"\n";
          List.iter
            (fun (p, desc_opt) ->
              bs b ".sp\n";
              bs b ("\""^p.mp_name^"\"\n");
              Misc.may (self#man_of_module_type b m_name) p.mp_type;
              bs b "\n";
              (
               match desc_opt with
                 None -> ()
               | Some t -> self#man_of_text b t
              );
              bs b "\n"
            )
            l;
          bs b "\n\n"

    (** Print groff string for a class. *)
    method man_of_class b c =
      Odoc_info.reset_type_names () ;
      let father = Name.father c.cl_name in
      bs b  ".I class ";
      if c.cl_virtual then bs b "virtual ";
      (
       match c.cl_type_parameters with
         [] -> ()
       | l ->
           bs b (Odoc_str.string_of_class_type_param_list l);
           bs b " "
      );
      bs b (Name.simple c.cl_name);
      bs b " : " ;
      self#man_of_class_type_expr b father c.cl_type;
      bs b "\n.sp\n";
      self#man_of_info b c.cl_info;
      bs b "\n.sp\n"

    (** Print groff string for a class type. *)
    method man_of_class_type b ct =
      Odoc_info.reset_type_names () ;
      bs b ".I class type ";
      if ct.clt_virtual then bs b "virtual " ;
      (
       match ct.clt_type_parameters with
        [] -> ()
      | l ->
          bs b (Odoc_str.string_of_class_type_param_list l);
          bs b " "
      );
      bs b (Name.simple ct.clt_name);
      bs b  " = " ;
      self#man_of_class_type_expr b (Name.father ct.clt_name) ct.clt_type;
      bs b  "\n.sp\n";
      self#man_of_info b ct.clt_info;
      bs b "\n.sp\n"

    (** Print groff string for a module. *)
    method man_of_module b m =
      bs b ".I module ";
      bs b (Name.simple m.m_name);
      bs b " : ";
      self#man_of_module_type b (Name.father m.m_name) m.m_type;
      bs b "\n.sp\n";
      self#man_of_info b m.m_info;
      bs b "\n.sp\n"

    (** Print groff string for a module type. *)
    method man_of_modtype b mt =
      bs b ".I module type ";
      bs b (Name.simple mt.mt_name);
      bs b " = ";
      (match mt.mt_type with
        None -> ()
      | Some t ->
          self#man_of_module_type b (Name.father mt.mt_name) t
      );
      bs b "\n.sp\n";
      self#man_of_info b mt.mt_info;
      bs b "\n.sp\n"

    (** Print groff string for a module comment.*)
    method man_of_module_comment b text =
      bs b "\n.PP\n";
      self#man_of_text b [Code ("=== "^(Odoc_misc.string_of_text text)^" ===")];
      bs b "\n.PP\n"

    (** Print groff string for a class comment.*)
    method man_of_class_comment b text =
      bs b "\n.PP\n";
      self#man_of_text b [Code ("=== "^(Odoc_misc.string_of_text text)^" ===")];
      bs b "\n.PP\n"

    method man_of_recfield b modname f =
      bs b ".I ";
      if f.rf_mutable then bs b (Odoc_messages.mutab^" ");
      bs b (f.rf_name^" : ");
      self#man_of_type_expr b modname f.rf_type;
      bs b "\n.sp\n";
      self#man_of_info b f.rf_text;
      bs b "\n.sp\n"

    method man_of_const b modname c =
      bs b ".I ";
      bs b (c.vc_name^" ");
      (match c.vc_args with
         [] -> ()
       | h::q ->
         bs b "of ";
         self#man_of_type_expr b modname h;
         List.iter
           (fun ty ->
              bs b " * ";
              self#man_of_type_expr b modname ty)
            q
      );
      bs b "\n.sp\n";
      self#man_of_info b c.vc_text;
      bs b "\n.sp\n"

    (** Print groff string for an included module. *)
    method man_of_included_module b m_name im =
      bs b ".I include ";
      (
       match im.im_module with
         None -> bs b im.im_name
       | Some mmt ->
           let name =
             match mmt with
               Mod m -> m.m_name
             | Modtype mt -> mt.mt_name
           in
           bs b (self#relative_idents m_name name)
      );
      bs b "\n.sp\n";
      self#man_of_info b im.im_info;
      bs b "\n.sp\n"

    (** Generate the man page for the given class.*)
    method generate_for_class cl =
      Odoc_info.reset_type_names () ;
      let date = Unix.time () in
      let file = self#file_name cl.cl_name in
      try
        let chanout = self#open_out file in
        let b = new_buf () in
        bs b (".TH \""^cl.cl_name^"\" ");
        bs b !man_section ;
        bs b (" "^(Odoc_misc.string_of_date ~hour: false date)^" ");
        bs b "OCamldoc ";
        bs b ("\""^(match !Global.title with Some t -> t | None -> "")^"\"\n");

        let abstract =
          match cl.cl_info with
            None | Some { i_desc = None } -> "no description"
          | Some { i_desc = Some t } ->
              let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
              self#remove_newlines s
        in

        bs b ".SH NAME\n";
        bs b (cl.cl_name^" \\- "^abstract^"\n");
        bs b (".SH "^Odoc_messages.clas^"\n");
        bs b (Odoc_messages.clas^"   "^cl.cl_name^"\n");
        bs b (".SH "^Odoc_messages.documentation^"\n");
        bs b ".sp\n";
        self#man_of_class b cl;

        (* parameters *)
        self#man_of_parameter_list b "" cl.cl_parameters;
        (* a large blank *)
        bs b  "\n.sp\n.sp\n";

(*
        (* class inheritance *)
        self#generate_class_inheritance_info chanout cl;
*)
        (* the various elements *)
        List.iter
          (fun element ->
            match element with
              Class_attribute a ->
                self#man_of_attribute b a
            | Class_method m ->
                self#man_of_method b m
            | Class_comment t ->
                self#man_of_class_comment b t
          )
          (Class.class_elements cl);

        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** Generate the man page for the given class type.*)
    method generate_for_class_type ct =
      Odoc_info.reset_type_names () ;
      let date = Unix.time () in
      let file = self#file_name ct.clt_name in
      try
        let chanout = self#open_out file in
        let b = new_buf () in
        bs b (".TH \""^ct.clt_name^"\" ");
        bs b !man_section ;
        bs b (" "^(Odoc_misc.string_of_date ~hour: false date)^" ");
        bs b "OCamldoc ";
        bs b ("\""^(match !Global.title with Some t -> t | None -> "")^"\"\n");

        let abstract =
          match ct.clt_info with
            None | Some { i_desc = None } -> "no description"
          | Some { i_desc = Some t } ->
              let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
              self#remove_newlines s
        in

        bs b ".SH NAME\n";
        bs b (ct.clt_name^" \\- "^abstract^"\n");
        bs b (".SH "^Odoc_messages.class_type^"\n");
        bs b (Odoc_messages.class_type^"   "^ct.clt_name^"\n");
        bs b (".SH "^Odoc_messages.documentation^"\n");
        bs b ".sp\n";

        self#man_of_class_type b ct;

        (* a large blank *)
        bs b "\n.sp\n.sp\n";
(*
        (* class inheritance *)
        self#generate_class_inheritance_info chanout cl;
*)
        (* the various elements *)
        List.iter
          (fun element ->
            match element with
              Class_attribute a ->
                self#man_of_attribute b a
            | Class_method m ->
                self#man_of_method b m
            | Class_comment t ->
                self#man_of_class_comment b t
          )
          (Class.class_type_elements ct);

        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    method man_of_module_type_body b mt =
      self#man_of_info b mt.mt_info;
      bs b "\n.sp\n";

      (* parameters for functors *)
      self#man_of_module_parameter_list b "" (Module.module_type_parameters mt);
      (* a large blank *)
      bs b "\n.sp\n.sp\n";

      (* module elements *)
      List.iter
        (fun ele ->
          match ele with
            Element_module m ->
              self#man_of_module b m
          | Element_module_type mt ->
              self#man_of_modtype b mt
          | Element_included_module im ->
              self#man_of_included_module b mt.mt_name im
          | Element_class c ->
              self#man_of_class b c
          | Element_class_type ct ->
              self#man_of_class_type b ct
          | Element_value v ->
              self#man_of_value b v
          | Element_type_extension te ->
              self#man_of_type_extension b mt.mt_name te
          | Element_exception e ->
              self#man_of_exception b e
          | Element_type t ->
              self#man_of_type b t
          | Element_module_comment text ->
              self#man_of_module_comment b text
        )
        (Module.module_type_elements mt);

    (** Generate the man file for the given module type.
       @raise Failure if an error occurs.*)
    method generate_for_module_type mt =
      let date = Unix.time () in
      let file = self#file_name mt.mt_name in
      try
        let chanout = self#open_out file in
        let b = new_buf () in
        bs b (".TH \""^mt.mt_name^"\" ");
        bs b !man_section ;
        bs b (" "^(Odoc_misc.string_of_date ~hour: false date)^" ");
        bs b "OCamldoc ";
        bs b ("\""^(match !Global.title with Some t -> t | None -> "")^"\"\n");

        let abstract =
          match mt.mt_info with
            None | Some { i_desc = None } -> "no description"
          | Some { i_desc = Some t } ->
              let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
              self#remove_newlines s
        in
        bs b ".SH NAME\n";
        bs b (mt.mt_name^" \\- "^abstract^"\n");
        bs b (".SH "^Odoc_messages.module_type^"\n");
        bs b (Odoc_messages.module_type^"   "^mt.mt_name^"\n");
        bs b (".SH "^Odoc_messages.documentation^"\n");
        bs b ".sp\n";
        bs b (Odoc_messages.module_type^"\n");
        bs b (".BI \""^(Name.simple mt.mt_name)^"\"\n");
        bs b " = ";
        (
         match mt.mt_type with
           None -> ()
         | Some t ->
             self#man_of_module_type b (Name.father mt.mt_name) t
        );
        bs b "\n.sp\n";
        self#man_of_module_type_body b mt;

        Buffer.output_buffer chanout b;
        close_out chanout

      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    method man_of_module_body b m =
      self#man_of_info b m.m_info;
      bs b "\n.sp\n";

      (* parameters for functors *)
      self#man_of_module_parameter_list b "" (Module.module_parameters m);
      (* a large blank *)
      bs b "\n.sp\n.sp\n";

      (* module elements *)
      List.iter
        (fun ele ->
          match ele with
            Element_module m ->
              self#man_of_module b m
          | Element_module_type mt ->
              self#man_of_modtype b mt
          | Element_included_module im ->
              self#man_of_included_module b m.m_name im
          | Element_class c ->
              self#man_of_class b c
          | Element_class_type ct ->
              self#man_of_class_type b ct
          | Element_value v ->
              self#man_of_value b v
          | Element_type_extension te ->
              self#man_of_type_extension b m.m_name te
          | Element_exception e ->
              self#man_of_exception b e
          | Element_type t ->
              self#man_of_type b t
          | Element_module_comment text ->
              self#man_of_module_comment b text
        )
        (Module.module_elements m);

    (** Generate the man file for the given module.
       @raise Failure if an error occurs.*)
    method generate_for_module m =
      let date = Unix.time () in
      let file = self#file_name m.m_name in
      try
        let chanout = self#open_out file in
        let b = new_buf () in
        bs b (".TH \""^m.m_name^"\" ");
        bs b !man_section ;
        bs b (" "^(Odoc_misc.string_of_date ~hour: false date)^" ");
        bs b "OCamldoc ";
        bs b ("\""^(match !Global.title with Some t -> t | None -> "")^"\"\n");

        let abstract =
          match m.m_info with
            None | Some { i_desc = None } -> "no description"
          | Some { i_desc = Some t } ->
              let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
              self#remove_newlines s
        in

        bs b ".SH NAME\n";
        bs b (m.m_name^" \\- "^abstract^"\n");
        bs b (".SH "^Odoc_messages.modul^"\n");
        bs b (Odoc_messages.modul^"   "^m.m_name^"\n");
        bs b (".SH "^Odoc_messages.documentation^"\n");
        bs b ".sp\n";
        bs b (Odoc_messages.modul^"\n");
        bs b (".BI \""^(Name.simple m.m_name)^"\"\n");
        bs b " : ";
        self#man_of_module_type b (Name.father m.m_name) m.m_type;
        bs b "\n.sp\n";
        self#man_of_module_body b m;
        Buffer.output_buffer chanout b;
        close_out chanout

      with
        Sys_error s ->
          raise (Failure s)

    (** Create the groups of elements to generate pages for. *)
    method create_groups mini module_list =
      let name res_ele =
        match res_ele with
          Res_module m -> m.m_name
        | Res_module_type mt -> mt.mt_name
        | Res_class c -> c.cl_name
        | Res_class_type ct -> ct.clt_name
        | Res_value v -> Name.simple v.val_name
        | Res_type t -> Name.simple t.ty_name
        | Res_extension x -> Name.simple x.xt_name
        | Res_exception e -> Name.simple e.ex_name
        | Res_attribute a -> Name.simple a.att_value.val_name
        | Res_method m -> Name.simple m.met_value.val_name
        | Res_section _ -> assert false
        | Res_recfield (_,f) -> f.rf_name
        | Res_const (_,f) -> f.vc_name
      in
      let all_items_pre = Odoc_info.Search.search_by_name module_list (Str.regexp ".*")  in
      let all_items = List.filter
          (fun r ->
             match r with
               Res_section _ -> false
             | Res_module _ | Res_module_type _
             | Res_class _ | Res_class_type _ -> true
             | _ -> not mini
          )
          all_items_pre
      in
      let sorted_items = List.sort (fun e1 -> fun e2 -> compare (name e1) (name e2)) all_items in
      let rec f acc1 acc2 l =
        match l with
          [] -> acc2 :: acc1
        | h :: q ->
            match acc2 with
              [] -> f acc1 [h] q
            | h2 :: q2 ->
                if (name h) = (name h2) then
                  if List.mem h acc2 then
                    f acc1 acc2 q
                  else
                    f acc1 (acc2 @ [h]) q
                else
                  f (acc2 :: acc1) [h] q
      in
      f [] [] sorted_items

    (** Generate a man page for a group of elements with the same name.
       A group must not be empty.*)
    method generate_for_group l =
     let name =
       Name.simple
         (
          match List.hd l with
            Res_module m -> m.m_name
          | Res_module_type mt -> mt.mt_name
          | Res_class c -> c.cl_name
          | Res_class_type ct -> ct.clt_name
          | Res_value v -> v.val_name
          | Res_type t -> t.ty_name
          | Res_extension x -> x.xt_name
          | Res_exception e -> e.ex_name
          | Res_attribute a -> a.att_value.val_name
          | Res_method m -> m.met_value.val_name
          | Res_section (s,_) -> s
          | Res_recfield (_,f) -> f.rf_name
          | Res_const (_,f) -> f.vc_name
         )
     in
     let date = Unix.time () in
      let file = self#file_name name in
      try
        let chanout = self#open_out file in
        let b = new_buf () in
        bs b (".TH \""^name^"\" ");
        bs b !man_section ;
        bs b (" "^(Odoc_misc.string_of_date ~hour: false date)^" ");
        bs b "OCamldoc ";
        bs b ("\""^(match !Global.title with Some t -> t | None -> "")^"\"\n");
        bs b ".SH NAME\n";
        bs b (name^" \\- all "^name^" elements\n\n");

        let f ele =
          match ele with
            Res_value v ->
              bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father v.val_name)^"\n");
              self#man_of_value b v
          | Res_type t ->
              bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father t.ty_name)^"\n");
              self#man_of_type b t
          | Res_extension x ->
              bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father x.xt_name)^"\n");
              self#man_of_type_extension b (Name.father x.xt_name) x.xt_type_extension
          | Res_exception e ->
              bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father e.ex_name)^"\n");
              self#man_of_exception b e
          | Res_attribute a ->
              bs b ("\n.SH "^Odoc_messages.clas^" "^(Name.father a.att_value.val_name)^"\n");
              self#man_of_attribute b a
          | Res_method m ->
              bs b ("\n.SH "^Odoc_messages.clas^" "^(Name.father m.met_value.val_name)^"\n");
              self#man_of_method b m
          | Res_class c ->
              bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father c.cl_name)^"\n");
              self#man_of_class b c
          | Res_class_type ct ->
              bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father ct.clt_name)^"\n");
              self#man_of_class_type b ct
          | Res_recfield (ty,f) ->
               bs b ("\n.SH Type "^(ty.ty_name)^"\n");
              self#man_of_recfield b (Name.father ty.ty_name) f
          | Res_const (ty,c) ->
               bs b ("\n.SH Type "^(ty.ty_name)^"\n");
              self#man_of_const b (Name.father ty.ty_name) c
          | Res_module m ->
              if Name.father m.m_name <> "" then
                begin
                  bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father m.m_name)^"\n");
                  bs b (Odoc_messages.modul^"\n");
                  bs b (".BI \""^(Name.simple m.m_name)^"\"\n");
                  bs b " : ";
                  self#man_of_module_type b (Name.father m.m_name) m.m_type;
                end
              else
                begin
                  bs b ("\n.SH "^Odoc_messages.modul^" "^m.m_name^"\n");
                  bs b " : ";
                  self#man_of_module_type b (Name.father m.m_name) m.m_type;
                end;
              bs b "\n.sp\n";
              self#man_of_module_body b m

          | Res_module_type mt ->
              bs b ("\n.SH "^Odoc_messages.modul^" "^(Name.father mt.mt_name)^"\n");
              bs b (Odoc_messages.module_type^"\n");
              bs b (".BI \""^(Name.simple mt.mt_name)^"\"\n");
              bs b " = ";
              (
               match mt.mt_type with
                None -> ()
              | Some t ->
                 self#man_of_module_type b (Name.father mt.mt_name) t
              );
              bs b "\n.sp\n";
              self#man_of_module_type_body b mt

          | Res_section _ ->
              (* normalement on ne peut pas avoir de module ici. *)
              ()
        in
        List.iter f l;
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** Generate all the man pages from a module list. *)
    method generate module_list =
      let sorted_module_list = List.sort (fun m1 m2 -> compare m1.m_name m2.m_name) module_list in
      let groups = self#create_groups !man_mini sorted_module_list in
      let f group =
        match group with
          [] ->
            ()
        | [Res_module m] -> self#generate_for_module m
        | [Res_module_type mt] -> self#generate_for_module_type mt
        | [Res_class cl] -> self#generate_for_class cl
        | [Res_class_type ct] -> self#generate_for_class_type ct
        | l -> self#generate_for_group l
      in
      List.iter f groups
  end
end

module type Man_generator = module type of Generator
