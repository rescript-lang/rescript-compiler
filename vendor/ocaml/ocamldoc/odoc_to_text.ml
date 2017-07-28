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

(** Text generation.

   This module contains the class [to_text] with methods used to transform
   information about elements to a [text] structure.*)

open Odoc_info
open Exception
open Type
open Value
open Module
open Class
open Parameter

(** A class used to get a [text] for info structures. *)
class virtual info =
  object (self)
    (** The list of pairs [(tag, f)] where [f] is a function taking
       the [text] associated to [tag] and returning a [text].
       Add a pair here to handle a tag.*)
    val mutable tag_functions = ([] : (string * (Odoc_info.text -> Odoc_info.text)) list)

    (** @return [etxt] value for an authors list. *)
    method text_of_author_list l =
      match l with
        [] ->
          []
      | _ ->
          [ Bold [Raw (Odoc_messages.authors^": ")] ;
            Raw (String.concat ", " l) ;
            Newline
          ]

    (** @return [text] value for the given optional version information.*)
    method text_of_version_opt v_opt =
      match v_opt with
        None -> []
      | Some v -> [ Bold [Raw (Odoc_messages.version^": ")] ;
                    Raw v ;
                    Newline
                  ]

    (** @return [text] value for the given optional since information.*)
    method text_of_since_opt s_opt =
      match s_opt with
        None -> []
      | Some s -> [ Bold [Raw (Odoc_messages.since^": ")] ;
                    Raw s ;
                    Newline
                  ]

    (** @return [text] value to represent the list of "before" information. *)
    method text_of_before = function
      [] -> []
    | l ->
        let f (v, text) =
          (Bold [Raw (Printf.sprintf "%s %s " Odoc_messages.before v) ]) ::
            text @
            [Newline]
        in
        List.flatten (List.map f l)

    (** @return [text] value for the given list of raised exceptions.*)
    method text_of_raised_exceptions l =
      match l with
        [] -> []
      | (s, t) :: [] ->
          [ Bold [ Raw Odoc_messages.raises ] ;
            Raw " " ;
            Code s ;
            Raw " "
          ]
          @ t
          @ [ Newline ]
      | _ ->
          [ Bold [ Raw Odoc_messages.raises ] ;
            Raw " " ;
            List
              (List.map
                (fun (ex, desc) ->(Code ex) :: (Raw " ") :: desc )
                 l
              ) ;
            Newline
          ]

    (** Return [text] value for the given "see also" reference. *)
    method text_of_see (see_ref, t)  =
      let t_ref =
        match see_ref with
          Odoc_info.See_url s -> [ Odoc_info.Link (s, t) ]
        | Odoc_info.See_file s -> (Odoc_info.Code s) :: (Odoc_info.Raw " ") :: t
        | Odoc_info.See_doc s -> (Odoc_info.Italic [Odoc_info.Raw s]) :: (Odoc_info.Raw " ") :: t
      in
      t_ref

    (** Return [text] value for the given list of "see also" references.*)
    method text_of_sees l =
      match l with
        [] -> []
      | see :: [] ->
          (Bold [ Raw Odoc_messages.see_also ]) ::
          (Raw " ") ::
          (self#text_of_see see) @ [ Newline ]
      | _ ->
          (Bold [ Raw Odoc_messages.see_also ]) ::
          [ List
              (List.map
                 (fun see -> self#text_of_see see)
                 l
              );
            Newline
          ]

    (** @return [text] value for the given optional return information.*)
    method text_of_return_opt return_opt =
      match return_opt with
        None -> []
      | Some t -> (Bold [Raw (Odoc_messages.returns^" ")]) :: t @ [ Newline ]

    (** Return a [text] for the given list of custom tagged texts. *)
    method text_of_custom l =
      List.fold_left
        (fun acc -> fun (tag, text) ->
          try
            let f = List.assoc tag tag_functions in
            match acc with
              [] -> f text
            | _ -> acc @ (Newline :: (f text))
          with
            Not_found ->
              Odoc_info.warning (Odoc_messages.tag_not_handled tag) ;
              acc
        )
        []
        l

    (** @return [text] value for a description, except for the i_params field. *)
    method text_of_info ?(block=true) info_opt =
      match info_opt with
        None ->
          []
      | Some info ->
          let t =
            (match info.i_deprecated with
              None -> []
            | Some t -> ( Italic [Raw (Odoc_messages.deprecated^" ")] ) :: t
             ) @
            (match info.i_desc with
              None -> []
            | Some t when t = [Odoc_info.Raw ""] -> []
            | Some t -> t @ [ Newline ]
            ) @
            (self#text_of_author_list info.i_authors) @
            (self#text_of_version_opt info.i_version) @
            (self#text_of_before info.i_before) @
            (self#text_of_since_opt info.i_since) @
            (self#text_of_raised_exceptions info.i_raised_exceptions) @
            (self#text_of_return_opt info.i_return_value) @
            (self#text_of_sees info.i_sees) @
            (self#text_of_custom info.i_custom)
          in
          if block then
            [Block t]
          else
            t
  end

(** This class defines methods to generate a [text] structure from elements. *)
class virtual to_text =
  object (self)
    inherit info

    method virtual label : ?no_: bool -> string -> string

    (** Take a string and return the string where fully qualified idents
       have been replaced by idents relative to the given module name.
       Also remove the "hidden modules".*)
    method relative_idents m_name s =
      let f str_t =
        let match_s = Str.matched_string str_t in
        let rel = Name.get_relative m_name match_s in
        Odoc_info.apply_if_equal Odoc_info.use_hidden_modules match_s rel
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Take a string and return the string where fully qualified idents
       have been replaced by idents relative to the given module name.
       Also remove the "hidden modules".*)
    method relative_module_idents m_name s =
      let f str_t =
        let match_s = Str.matched_string str_t in
        let rel = Name.get_relative m_name match_s in
        Odoc_info.apply_if_equal Odoc_info.use_hidden_modules match_s rel
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([A-Z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Get a string for a [Types.class_type] where all idents are relative. *)
    method normal_class_type m_name t =
      self#relative_idents m_name (Odoc_info.string_of_class_type t)

    (** Get a string for a [Types.module_type] where all idents are relative. *)
    method normal_module_type ?code m_name t =
      self#relative_module_idents m_name (Odoc_info.string_of_module_type ?code t)

    (** Get a string for a type where all idents are relative. *)
    method normal_type m_name t =
      self#relative_idents m_name (Odoc_info.string_of_type_expr t)

    (** Get a string for a list of types where all idents are relative. *)
    method normal_type_list ?par m_name sep t =
      self#relative_idents m_name (Odoc_info.string_of_type_list ?par sep t)

    (** Get a string for a list of class or class type type parameters
       where all idents are relative. *)
    method normal_class_type_param_list m_name t =
      self#relative_idents m_name (Odoc_info.string_of_class_type_param_list t)

    (** Get a string for the parameters of a class (with arrows) where all idents are relative. *)
    method normal_class_params m_name c =
      let s = Odoc_info.string_of_class_params c in
      self#relative_idents m_name
        (Odoc_info.remove_ending_newline s)

    (** @return [text] value to represent a [Types.type_expr].*)
    method text_of_type_expr module_name t =
      let t = List.flatten
          (List.map
             (fun s -> [Code s ; Newline ])
             (Str.split (Str.regexp "\n")
                (self#normal_type module_name t))
          )
      in
      t

    (** Return [text] value for a given short [Types.type_expr].*)
    method text_of_short_type_expr module_name t =
      [ Code (self#normal_type module_name t) ]

    (** Return [text] value or the given list of [Types.type_expr], with
       the given separator. *)
    method text_of_type_expr_list module_name sep l =
      [ Code (self#normal_type_list module_name sep l) ]

    (** Return [text] value or the given list of [Types.type_expr],
       as type parameters of a class of class type. *)
    method text_of_class_type_param_expr_list module_name l =
      [ Code (self#normal_class_type_param_list module_name l) ]

    (** @return [text] value to represent parameters of a class (with arraows).*)
    method text_of_class_params module_name c =
      let t = Odoc_info.text_concat
          [Newline]
          (List.map
             (fun s -> [Code s])
             (Str.split (Str.regexp "\n")
                (self#normal_class_params module_name c))
          )
      in
      t

    (** @return [text] value to represent a [Types.module_type]. *)
    method text_of_module_type t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_info.string_of_module_type t))
      in
      [ Code s ]

    (** @return [text] value for a value. *)
    method text_of_value v =
      let name = v.val_name in
      let s_name = Name.simple name in
      let s =
        Format.fprintf Format.str_formatter "@[<hov 2>val %s :@ %s"
          s_name
          (self#normal_type (Name.father v.val_name) v.val_type);
        Format.flush_str_formatter ()
      in
      [ CodePre s ] @
      [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")] @
      (self#text_of_info v.val_info)

    (** @return [text] value for a class attribute. *)
    method text_of_attribute a =
      let s_name = Name.simple a.att_value.val_name in
      let mod_name = Name.father a.att_value.val_name in
      let s =
        Format.fprintf Format.str_formatter "@[<hov 2>val %s%s%s :@ %s"
          (if a.att_virtual then "virtual " else "")
          (if a.att_mutable then "mutable " else "")
          s_name
          (self#normal_type mod_name a.att_value.val_type);
        Format.flush_str_formatter ()
      in
      (CodePre s) ::
      [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")] @
      (self#text_of_info a.att_value.val_info)

    (** @return [text] value for a class method. *)
    method text_of_method m =
      let s_name = Name.simple m.met_value.val_name in
      let mod_name = Name.father m.met_value.val_name in
      let s =
        Format.fprintf Format.str_formatter "@[<hov 2>method %s%s%s :@ %s"
          (if m.met_private then "private " else "")
          (if m.met_virtual then "virtual " else "")
          s_name
          (self#normal_type mod_name m.met_value.val_type);
        Format.flush_str_formatter ()
      in
      (CodePre s) ::
      [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")] @
      (self#text_of_info m.met_value.val_info)


    (** @return [text] value for an exception. *)
    method text_of_exception e =
      let s_name = Name.simple e.ex_name in
      let father = Name.father e.ex_name in
      Format.fprintf Format.str_formatter "@[<hov 2>exception %s" s_name ;
      (match e.ex_args, e.ex_ret with
         [], None -> ()
       | l, None ->
           Format.fprintf Format.str_formatter " %s@ %s"
             "of"
             (self#normal_type_list ~par: false father " * " l)
       | [], Some r ->
           Format.fprintf Format.str_formatter " %s@ %s"
             ":"
             (self#normal_type father r)
       | l, Some r ->
           Format.fprintf Format.str_formatter " %s@ %s@ %s@ %s"
             ":"
             (self#normal_type_list ~par: false father " * " l)
             "->"
             (self#normal_type father r)
      );
      (match e.ex_alias with
         None -> ()
       | Some ea ->
           Format.fprintf Format.str_formatter " = %s"
            (
              match ea.ea_ex with
                None -> ea.ea_name
              | Some e -> e.ex_name
            )
      );
      let s2 = Format.flush_str_formatter () in
      [ CodePre s2 ] @
      [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")] @
      (self#text_of_info e.ex_info)

    (** Return [text] value for the description of a function parameter. *)
    method text_of_parameter_description p =
      match Parameter.names p with
        [] -> []
      | name :: [] ->
          (
           (* Only one name, no need for label for the description. *)
           match Parameter.desc_by_name p name with
             None -> []
           | Some t -> t
          )
      | l ->
          (*  A list of names, we display those with a description. *)
          let l2 = List.filter (fun n -> (Parameter.desc_by_name p n) <> None) l in
          match l2 with
            [] -> []
          | _ ->
              [List
                  (List.map
                     (fun n ->
                       match Parameter.desc_by_name p n with
                         None -> [] (* should not occur *)
                       | Some t -> [Code (n^" ") ; Raw ": "] @ t
                     )
                     l2
                  )
              ]


    (** Return [text] value for a list of parameters. *)
    method text_of_parameter_list m_name l =
      match l with
        [] ->
          []
      | _ ->
          [ Bold [Raw Odoc_messages.parameters] ;
            Raw ":" ;
            List
              (List.map
                 (fun p ->
                   (match Parameter.complete_name p with
                     "" -> Code "?"
                   | s -> Code s
                   ) ::
                   [Code " : "] @
                   (self#text_of_short_type_expr m_name (Parameter.typ p)) @
                   [Newline] @
                   (self#text_of_parameter_description p)
                 )
                 l
              )
          ]

    (** Return [text] value for a list of module parameters. *)
    method text_of_module_parameter_list l =
      match l with
        [] ->
          []
      | _ ->
          [ Newline ;
            Bold [Raw Odoc_messages.parameters] ;
            Raw ":" ;
            List
              (List.map
                 (fun (p, desc_opt) ->
                   begin match p.mp_type with None -> [Raw ""]
                   | Some mty ->
                       [Code (p.mp_name^" : ")] @
                       (self#text_of_module_type mty)
                   end @
                   (match desc_opt with
                     None -> []
                   | Some t -> (Raw " ") :: t)
                 )
                 l
              )
          ]

(**/**)

    (** Return [text] value for the given [class_kind].*)
    method text_of_class_kind father ckind =
      match ckind with
        Class_structure _ ->
          [Code Odoc_messages.object_end]

      | Class_apply capp ->
          [Code
              (
               (
                match capp.capp_class with
                  None -> capp.capp_name
                | Some cl -> cl.cl_name
               )^
               " "^
               (String.concat " "
                  (List.map
                     (fun s -> "("^s^")")
                     capp.capp_params_code))
              )
          ]

      | Class_constr cco ->
          (
           match cco.cco_type_parameters with
             [] -> []
           | l ->
               (Code "[")::
               (self#text_of_type_expr_list father ", " l)@
               [Code "] "]
          )@
          [Code (
            match cco.cco_class with
              None -> cco.cco_name
            | Some (Cl cl) -> Name.get_relative father cl.cl_name
            | Some (Cltype (clt,_)) -> Name.get_relative father clt.clt_name
           )
          ]

      | Class_constraint (ck, ctk) ->
          [Code "( "] @
          (self#text_of_class_kind father ck) @
          [Code " : "] @
          (self#text_of_class_type_kind father ctk) @
          [Code " )"]


    (** Return [text] value for the given [class_type_kind].*)
    method text_of_class_type_kind father ctkind =
      match ctkind with
        Class_type cta ->
          (
           match cta.cta_type_parameters with
             [] -> []
           | l ->
               (Code "[") ::
               (self#text_of_class_type_param_expr_list father l) @
               [Code "] "]
          ) @
          (
           match cta.cta_class with
             None -> [ Code cta.cta_name ]
           | Some (Cltype (clt, _)) ->
               let rel = Name.get_relative father clt.clt_name in
               [Code rel]
           | Some (Cl cl) ->
               let rel = Name.get_relative father cl.cl_name in
               [Code rel]
          )
      | Class_signature _ ->
          [Code Odoc_messages.object_end]

    (** Return [text] value for a [module_kind]. *)
    method text_of_module_kind ?(with_def_syntax=true) k =
      match k with
        Module_alias m_alias ->
          (match m_alias.ma_module with
            None ->
              [Code ((if with_def_syntax then " = " else "")^m_alias.ma_name)]
          | Some (Mod m) ->
              [Code ((if with_def_syntax then " = " else "")^m.m_name)]
          | Some (Modtype mt) ->
              [Code ((if with_def_syntax then " = " else "")^mt.mt_name)]
          )
      | Module_apply (k1, k2) ->
          (if with_def_syntax then [Code " = "] else []) @
          (self#text_of_module_kind ~with_def_syntax: false k1) @
          [Code " ( "] @
          (self#text_of_module_kind ~with_def_syntax: false k2) @
          [Code " ) "]

      | Module_with (tk, code) ->
          (if with_def_syntax then [Code " : "] else []) @
          (self#text_of_module_type_kind ~with_def_syntax: false tk) @
          [Code code]

      | Module_constraint (k, tk) ->
          (if with_def_syntax then [Code " : "] else []) @
          [Code "( "] @
          (self#text_of_module_kind ~with_def_syntax: false k) @
          [Code " : "] @
          (self#text_of_module_type_kind ~with_def_syntax: false tk) @
          [Code " )"]

      | Module_struct _ ->
          [Code ((if with_def_syntax then " : " else "")^
                 Odoc_messages.struct_end^" ")]

      | Module_functor (p, k)  ->
          (if with_def_syntax then [Code " : "] else []) @
          [Code "functor ... "] @
          [Code " -> "] @
          (self#text_of_module_kind ~with_def_syntax: false k)

      | Module_typeof s ->
          let code = Printf.sprintf "%smodule type of %s"
            (if with_def_syntax then " : " else "")
            s
          in
          [Code code]
      | Module_unpack (code, _) ->
          let code = Printf.sprintf "%s%s"
            (if with_def_syntax then " : " else "")
            code
          in
          [Code code]

    (** Return html code for a [module_type_kind].*)
    method text_of_module_type_kind ?(with_def_syntax=true) tk =
      match tk with
      | Module_type_struct _ ->
          [Code ((if with_def_syntax then " = " else "")^Odoc_messages.sig_end)]

      | Module_type_functor (p, k) ->
          let t1 =
            [Code ("("^p.mp_name^" : ")] @
            (self#text_of_module_type_kind p.mp_kind) @
            [Code ") -> "]
          in
          let t2 = self#text_of_module_type_kind ~with_def_syntax: false k in
          (if with_def_syntax then [Code " = "] else []) @ t1 @ t2

      | Module_type_with (tk2, code) ->
          let t = self#text_of_module_type_kind ~with_def_syntax: false tk2 in
          (if with_def_syntax then [Code " = "] else []) @
          t @ [Code code]

      | Module_type_alias mt_alias ->
          [Code ((if with_def_syntax then " = " else "")^
                 (match mt_alias.mta_module with
                   None -> mt_alias.mta_name
                 | Some mt -> mt.mt_name))
          ]

      | Odoc_module.Module_type_typeof s ->
          let code = Printf.sprintf "%smodule type of %s"
            (if with_def_syntax then " = " else "") s
          in
          [ Code code ]
  end
