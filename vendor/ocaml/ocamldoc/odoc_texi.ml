(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*      Olivier Andrieu, base sur du code de Maxence Guesdon           *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Generation of Texinfo documentation. *)

open Odoc_info
open Parameter
open Value
open Type
open Extension
open Exception
open Class
open Module

let esc_8bits = ref false

let info_section = ref "OCaml"

let info_entry = ref []

(** {2 Some small helper functions} *)

let puts_nl chan s =
  output_string chan s ;
  output_char chan '\n'
let puts chan s =
  output_string chan s
let nl chan =
  output_char chan '\n'

let is = function
  | None -> false
  | Some _ -> true

let pad_to n s =
  let len = String.length s in
  if len < n then s ^ String.make (n - len) ' ' else s

let indent nb_sp s =
  let c = ref 0 in
  let len = pred (String.length s) in
  for i = 0 to len do if s.[i] = '\n' then incr c done ;
  let s' = Bytes.make (succ len + (succ !c) * nb_sp ) ' ' in
  c := nb_sp ;
  for i = 0 to len do
    Bytes.set s' !c s.[i] ;
    if s.[i] = '\n' then c := !c + nb_sp ;
    incr c
  done ;
  Bytes.to_string s'

type subparts = [
  | `Module of Odoc_info.Module.t_module
  | `Module_type of Odoc_info.Module.t_module_type
  | `Class of Odoc_info.Class.t_class
  | `Class_type of Odoc_info.Class.t_class_type
  ]

type menu_data = [
  | subparts
  | `Blank
  | `Comment of string
  | `Texi of string
  | `Index of string
] list

let nothing = Verbatim ""

let module_subparts =
  let rec iter acc = function
    | [] -> List.rev acc
    (* skip aliases *)
    | Element_module { m_kind = Module_alias _ } :: n ->
        iter acc n
    | Element_module_type { mt_kind = Some (Module_type_alias _) } :: n ->
        iter acc n
    (* keep modules, module types, classes and class types *)
    | Element_module m :: n ->
        iter (`Module m :: acc) n
    | Element_module_type mt :: n ->
        iter (`Module_type mt :: acc) n
    | Element_class c :: n ->
        iter (`Class c :: acc) n
    | Element_class_type ct :: n ->
        iter (`Class_type ct :: acc) n
    (* forget the rest *)
    | _ :: n -> iter acc n
  in
  iter []

type indices = [
  | `Type
  | `Extension
  | `Exception
  | `Value
  | `Class_att
  | `Method
  | `Class
  | `Class_type
  | `Module
  | `Module_type
]

let indices = function
  | `Type        -> "ty"
  | `Extension   -> "xt"
  | `Exception   -> "ex"
  | `Value       -> "va"
  | `Class_att   -> "ca"
  | `Method      -> "me"
  | `Class       -> "cl"
  | `Class_type  -> "ct"
  | `Module      -> "mo"
  | `Module_type -> "mt"

let indices_names = [
  "Types"           , "ty" ;
  "Extensions"      , "xt" ;
  "Exceptions"      , "ex" ;
  "Values"          , "va" ;
  "Class attributes", "ca" ;
  "Methods"         , "me" ;
  "Classes"         , "cl" ;
  "Class types"     , "ct" ;
  "Modules"         , "mo" ;
  "Module types"    , "mt" ; ]



(** Module for generating various Texinfo things (menus, xrefs, ...) *)
module Texi =
struct
  (** Associations of strings to subsitute in Texinfo code. *)
  let subst_strings = [
    (Str.regexp "@", "@@") ;
    (Str.regexp "{", "@{") ;
    (Str.regexp "}", "@}") ;
    (Str.regexp "\\.\\.\\.", "@dots{}") ;
  ] @
    (if !esc_8bits
    then [
    (Str.regexp "\xE0", "@`a") ;
    (Str.regexp "\xE2", "@^a") ;
    (Str.regexp "\xE9", "@'e") ;
    (Str.regexp "\xE8", "@`e") ;
    (Str.regexp "\xEA", "@^e") ;
    (Str.regexp "\xEB", "@\"e") ;
    (Str.regexp "\xF7", "@,{c}") ;
    (Str.regexp "\xF4", "@^o") ;
    (Str.regexp "\xF6", "@\"o") ;
    (Str.regexp "\xEE", "@^i") ;
    (Str.regexp "\xEF", "@\"i") ;
    (Str.regexp "\xF9", "@`u") ;
    (Str.regexp "\xFB", "@^u") ;
    (Str.regexp "\xE6", "@ae{}" ) ;
    (Str.regexp "\xC6", "@AE{}" ) ;
    (Str.regexp "\xDF", "@ss{}" ) ;
    (Str.regexp "\xA9", "@copyright{}" ) ;
    ]
    else [])

  (** Escape the strings which would clash with Texinfo syntax. *)
  let escape s =
    List.fold_left
      (fun acc (p, r) -> Str.global_replace p r acc)
      s subst_strings

  (** Removes dots (no good for a node name). *)
  let fix_nodename s =
    Str.global_replace (Str.regexp "\\.") "/" (escape s)

  (** Generates a Texinfo menu. *)
  let generate_menu chan subpart_list =
    if subpart_list <> []
    then begin
      let menu_line part_qual name =
        let sname = Name.simple name in
        if sname = name
        then (
          puts chan (pad_to 35
                       ("* " ^ sname ^ ":: ")) ;
          puts_nl chan part_qual )
        else (
          puts chan (pad_to 35
                       ("* " ^ sname ^ ": " ^ (fix_nodename name) ^ ". " )) ;
          puts_nl chan part_qual )
      in
      puts_nl chan "@menu" ;
      List.iter
        (function
        | `Module { m_name = name } ->
            menu_line Odoc_messages.modul name
        | `Module_type { mt_name = name } ->
            menu_line Odoc_messages.module_type name
        | `Class { cl_name = name } ->
            menu_line Odoc_messages.clas name
        | `Class_type { clt_name = name } ->
            menu_line Odoc_messages.class_type name
        | `Blank -> nl chan
        | `Comment c -> puts_nl chan (escape c)
        | `Texi t -> puts_nl chan t
        | `Index ind -> Printf.fprintf chan "* %s::\n" ind)
      subpart_list ;
    puts_nl chan "@end menu"
  end

  (** cross reference to node [name] *)
  let xref ?xname name =
    "@xref{" ^ (fix_nodename name) ^
    (match xname with | None -> "" | Some s -> "," ^ s) ^
    "}."

  (** enclose the string between [\@ifinfo] tags *)
  let ifinfo s =
    String.concat "\n"
      [ "@ifinfo" ; s ; "@end ifinfo" ; "" ]

  (** [install-info] information *)
  let dirsection sec =
    "@dircategory " ^ (escape sec)

  let direntry ent =
    [ "@direntry" ] @
      (List.map escape ent) @
    [ "@end direntry" ]
end





(** {2 Generation of Texinfo code} *)

(** This class generates Texinfo code from text structures *)
class text =
  object(self)

  (** Associations between a title number and texinfo code. *)
    val titles = [
      1, "@chapter " ;
      2, "@section " ;
      3, "@subsection " ;
      4, "@subsubsection " ;
    ]

    val fallback_title =
      "@unnumberedsubsubsec "

    val headings = [
      1, "@majorheading " ;
      2, "@heading " ;
      3, "@subheading " ;
      4, "@subsubheading " ;
    ]

    val fallback_heading =
      "@subsubheading "

    method escape =
      Texi.escape

    (** this method is not used here but is virtual
        in a class we will inherit later *)
    method label ?(no_ : bool option) (_ : string) : string =
      failwith "gni"

    (** Return the Texinfo code corresponding to the [text] parameter.*)
    method texi_of_text t =
      String.concat ""
        (List.map self#texi_of_text_element t)


    (** {3 Conversion methods}
       [texi_of_????] converts a [text_element] to a Texinfo string. *)

    (** Return the Texinfo code for the [text_element] in parameter. *)
    method texi_of_text_element = function
      | Verbatim s | Latex s -> self#texi_of_Verbatim s
      | Raw s -> self#texi_of_Raw s
      | Code s -> self#texi_of_Code s
      | CodePre s -> self#texi_of_CodePre s
      | Bold t -> self#texi_of_Bold t
      | Italic t -> self#texi_of_Italic t
      | Emphasize t -> self#texi_of_Emphasize t
      | Center t -> self#texi_of_Center t
      | Left t -> self#texi_of_Left t
      | Right t -> self#texi_of_Right t
      | List tl -> self#texi_of_List tl
      | Enum tl -> self#texi_of_Enum tl
      | Newline -> self#texi_of_Newline
      | Block t -> self#texi_of_Block t
      | Title (n, _, t) -> self#texi_of_Title n t
      | Link (s, t) -> self#texi_of_Link s t
      | Ref (name, kind, _) ->self#texi_of_Ref name kind
      | Superscript t -> self#texi_of_Superscript t
      | Subscript t -> self#texi_of_Subscript t
      | Odoc_info.Module_list _ -> ""
      | Odoc_info.Index_list -> ""
      | Odoc_info.Custom (s,t) -> self#texi_of_custom_text s t
      | Odoc_info.Target (target, code) -> self#texi_of_Target ~target ~code

    method texi_of_custom_text s t = ""

    method texi_of_Target ~target ~code =
      if String.lowercase target = "texi" then code else ""

    method texi_of_Verbatim s = s
    method texi_of_Raw s = self#escape s
    method texi_of_Code s = "@code{" ^ (self#escape s) ^ "}"
    method texi_of_CodePre s =
      String.concat "\n"
        [ "" ;  "@example" ; self#escape s ; "@end example" ; "" ]
    method texi_of_Bold t = "@strong{" ^ (self#texi_of_text t) ^ "}"
    method texi_of_Italic t = "@i{" ^ (self#texi_of_text t) ^ "}"
    method texi_of_Emphasize t = "@emph{" ^ (self#texi_of_text t) ^ "}"
    method texi_of_Center t =
      let sl = Str.split (Str.regexp "\n") (self#texi_of_text t) in
      String.concat ""
        ((List.map (fun s -> "\n@center "^s) sl) @ [ "\n" ])
    method texi_of_Left t =
      String.concat "\n"
        [ "" ; "@flushleft" ; self#texi_of_text t ; "@end flushleft" ; "" ]
    method texi_of_Right t =
      String.concat "\n"
        [ "" ; "@flushright" ; self#texi_of_text t ; "@end flushright"; "" ]
    method texi_of_List tl =
      String.concat "\n"
        ( [ "" ; "@itemize" ] @
          (List.map (fun t -> "@item\n" ^ (self#texi_of_text t)) tl) @
          [ "@end itemize"; "" ] )
    method texi_of_Enum tl =
      String.concat "\n"
        ( [ "" ; "@enumerate" ] @
          (List.map (fun t -> "@item\n" ^ (self#texi_of_text t)) tl) @
          [ "@end enumerate"; "" ] )
    method texi_of_Newline = "\n"
    method texi_of_Block t =
      String.concat "\n"
        [ "@format" ; self#texi_of_text t ; "@end format" ; "" ]
    method texi_of_Title n t =
      let t_begin =
        try List.assoc n titles
        with Not_found -> fallback_title in
      t_begin ^ (self#texi_of_text t) ^ "\n"
    method texi_of_Link s t =
      String.concat ""
        [ "@uref{" ; s ;  "," ; self#texi_of_text t ; "}" ]
    method texi_of_Ref name kind =
      let xname =
        match kind with
        | Some RK_module ->
            Odoc_messages.modul ^ " " ^ (Name.simple name)
        | Some RK_module_type ->
            Odoc_messages.module_type ^ " " ^ (Name.simple name)
        | Some RK_class ->
            Odoc_messages.clas ^ " " ^ (Name.simple name)
        | Some RK_class_type ->
            Odoc_messages.class_type ^ " " ^ (Name.simple name)
        | _ -> ""
      in
      if xname = "" then self#escape name else Texi.xref ~xname name
    method texi_of_Superscript t =
      "^@{" ^ (self#texi_of_text t) ^ "@}"
    method texi_of_Subscript t =
      "_@{" ^ (self#texi_of_text t) ^ "@}"

    method heading n t =
      let f =
        try List.assoc n headings
        with Not_found -> fallback_heading
      in
      f ^ (self#texi_of_text t) ^ "\n"

    method fixedblock t =
      Block ( ( Verbatim "@t{" :: t ) @ [ Verbatim "}" ] )

  end

exception Aliased_node

module Generator =
struct

(** This class is used to create objects which can generate a simple
    Texinfo documentation. *)
class texi =
  object (self)
    inherit text as to_texi
    inherit Odoc_to_text.to_text as to_text

    (** {3 Small helper stuff.} *)

    val maxdepth = 4

    val bullet = Verbatim " @bullet{} "
    val minus  = Verbatim " @minus{} "
    val linebreak =  Verbatim "@*\n"

    val mutable indices_to_build = [ `Module ]

    (** Keep a set of nodes we create. If we try to create one
        a second time, that means it is some kind of alias, so
        don't do it, just link to the previous one *)
    val node_tbl = Hashtbl.create 37

    method node depth name =
      if Hashtbl.mem node_tbl name
      then raise Aliased_node ;
      Hashtbl.add node_tbl name () ;
      if depth <= maxdepth
      then Verbatim ("@node " ^ (Texi.fix_nodename name) ^ ",\n")
      else nothing

    method index (ind : indices) ent =
      Verbatim
        (if !Global.with_index
        then (assert(List.mem ind indices_to_build) ;
              String.concat ""
                [ "@" ; indices ind ; "index " ;
                  Texi.escape (Name.simple ent) ; "\n" ])
        else "")


    (** Two hacks to fix linebreaks in the descriptions.*)
    method private fix_linebreaks =
      let re = Str.regexp "\n[ \t]*" in
      fun t ->
        List.map
          (function
            | Newline -> Raw "\n"
            | Raw s -> Raw (Str.global_replace re "\n" s)
            | List tel -> List (List.map self#fix_linebreaks tel)
            | Enum tel -> Enum (List.map self#fix_linebreaks tel)
            | txt -> txt) t

    method private soft_fix_linebreaks =
      let re = Str.regexp "\n[ \t]*" in
      fun ind t ->
        let rep = "\n" ^ String.make ind ' ' in
        List.map
          (function
            | Raw s -> Raw (Str.global_replace re rep s)
            | txt -> txt) t

    (** {3 [text] values generation}
       Generates [text] values out of description parts.
       Redefines some of methods of {! Odoc_to_text.to_text}. *)

    method text_of_desc = function
      | None -> []
      | Some [ Raw "" ] -> []
      | Some t -> (self#fix_linebreaks t) @ [ Newline ]

    method text_of_sees_opt see_l =
      List.concat
        (List.map
           (function
             | (See_url s, t) ->
                 [ linebreak ; Bold [ Raw Odoc_messages.see_also ] ;
                   Raw " " ; Link (s, t) ; Newline ]
             | (See_file s, t)
             | (See_doc s, t)  ->
                 [ linebreak ; Bold [ Raw Odoc_messages.see_also ] ;
                   Raw " " ; Raw s ] @ t @ [ Newline ])
           see_l)

    method text_of_before l =
      List.flatten
      (List.map
        (fun x -> linebreak :: (to_text#text_of_before [x])) l)

    method text_of_params params_list =
        List.concat
          (List.map
             (fun (s, t) ->
               [ linebreak ;
                 Bold [ Raw Odoc_messages.parameters ] ;
                 Raw " " ; Raw s ; Raw ": " ] @ t @ [ Newline ] )
             params_list)

    method! text_of_raised_exceptions = function
      | [] -> []
      | (s, t) :: [] ->
          [ linebreak ;
            Bold [ Raw Odoc_messages.raises ] ;
            Raw " " ; Code s ; Raw " " ]
          @ t @ [ Newline ]
      | l ->
          [ linebreak ;
            Bold [ Raw Odoc_messages.raises ] ;
            Raw " :" ;
            List
              (List.map
                 (fun (ex, desc) ->(Code ex) :: (Raw " ") :: desc ) l ) ;
            Newline ]

    method! text_of_return_opt = function
      | None -> []
      | Some t ->
          (Bold [Raw Odoc_messages.returns ]) :: Raw " " :: t @ [ Newline ]

    method! text_of_custom c_l =
      List.flatten
        (List.rev
           (List.fold_left
              (fun acc -> fun (tag, text) ->
                try
                  let f = List.assoc tag tag_functions in
                  ( linebreak :: (f text) @ [ Newline ] ) :: acc
                with
               Not_found ->
                 Odoc_info.warning (Odoc_messages.tag_not_handled tag) ;
                 acc
              ) [] c_l))

    method! text_of_info ?(block=false) = function
      | None -> []
      | Some info ->
          let t =
            List.concat
                 [ ( match info.i_deprecated with
                 | None -> []
                 | Some t ->
                     (Raw (Odoc_messages.deprecated ^ " ")) ::
                     (self#fix_linebreaks t)
                     @ [ Newline ; Newline ] ) ;
                   self#text_of_desc info.i_desc ;
                   if info.i_authors <> []
                   then ( linebreak ::
                          self#text_of_author_list info.i_authors )
                   else [] ;
                   if is info.i_version
                   then ( linebreak ::
                          self#text_of_version_opt info.i_version )
                   else [] ;
                   self#text_of_sees_opt info.i_sees ;
                   self#text_of_before info.i_before ;
                   if is info.i_since
                   then ( linebreak ::
                          self#text_of_since_opt info.i_since )
                   else [] ;
                   self#text_of_params info.i_params ;
                   self#text_of_raised_exceptions info.i_raised_exceptions ;
                   if is info.i_return_value
                   then ( linebreak ::
                          self#text_of_return_opt info.i_return_value )
                   else [] ;
                   self#text_of_custom info.i_custom ;
                 ] in
          if block
          then [ Block t ]
          else (t @ [ Newline ] )

    method texi_of_info i =
      self#texi_of_text (self#text_of_info i)

    (** {3 Conversion of [module_elements] into Texinfo strings}
       The following functions convert [module_elements] and their
       description to [text] values then to Texinfo strings using the
       functions above. *)

    method text_el_of_type_expr m_name typ =
      Raw (indent 5
             (self#relative_idents m_name
                (Odoc_info.string_of_type_expr typ)))

    method! text_of_short_type_expr m_name typ =
      [ Raw (self#normal_type m_name typ) ]

    (** Return Texinfo code for a value. *)
    method texi_of_value v =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock
                  [ Newline ; minus ;
                    Raw ("val " ^ (Name.simple v.val_name) ^ " :\n") ;
                    self#text_el_of_type_expr
                      (Name.father v.val_name) v.val_type ] ;
                self#index `Value v.val_name ; Newline  ] @
                (self#text_of_info v.val_info) in
      self#texi_of_text t


    (** Return Texinfo code for a class attribute. *)
    method texi_of_attribute a =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock
                  [ Newline ; minus ;
                    Raw "val " ;
                    Raw (if a.att_virtual then "virtual " else "") ;
                    Raw (if a.att_mutable then "mutable " else "") ;
                    Raw (Name.simple a.att_value.val_name) ;
                    Raw " :\n" ;
                    self#text_el_of_type_expr
                      (Name.father a.att_value.val_name)
                      a.att_value.val_type ] ;
                self#index `Class_att a.att_value.val_name  ; Newline ] @
        (self#text_of_info a.att_value.val_info) in
      self#texi_of_text t


    (** Return Texinfo code for a class method. *)
    method texi_of_method m =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock
                  [ Newline ; minus ; Raw "method " ;
                    Raw (if m.met_private then "private " else "") ;
                    Raw (if m.met_virtual then "virtual " else "") ;
                    Raw (Name.simple m.met_value.val_name) ;
                    Raw " :\n" ;
                    self#text_el_of_type_expr
                      (Name.father m.met_value.val_name)
                      m.met_value.val_type ] ;
                self#index `Method m.met_value.val_name ; Newline ] @
        (self#text_of_info m.met_value.val_info) in
      self#texi_of_text t


    method string_of_type_parameters t =
      let f (tp, co, cn) =
        Printf.sprintf "%s%s"
          (Odoc_info.string_of_variance t (co, cn))
          (Odoc_info.string_of_type_expr tp)
      in
      match t.ty_parameters with
      | [] -> ""
      | [ (tp, co, cn) ] ->
          (f (tp, co, cn))^" "
      | l ->
          Printf.sprintf "(%s) "
            (String.concat ", " (List.map f l))

    method string_of_type_args (args:Types.type_expr list) (ret:Types.type_expr option) =
      match args, ret with
      | [], None -> ""
      | args, None -> " of " ^ (Odoc_info.string_of_type_list " * " args)
      | [], Some r -> " : " ^ (Odoc_info.string_of_type_expr r)
      | args, Some r -> " : " ^ (Odoc_info.string_of_type_list " * " args) ^
                                " -> " ^ (Odoc_info.string_of_type_expr r)

    (** Return Texinfo code for a type. *)
    method texi_of_type ty =
      Odoc_info.reset_type_names () ;
      let entry_doc = function
        | None -> [ Newline ]
        | Some t ->
          (Raw (indent 5 "\n(*\n") :: (self#soft_fix_linebreaks 8 (self#text_of_info (Some t))))
          @ [ Raw " *)" ; Newline ]
      in
      let t =
        [ self#fixedblock (
          [ Newline ; minus ; Raw "type " ;
            Raw (self#string_of_type_parameters ty) ;
            Raw (Name.simple ty.ty_name) ] @
          let priv = ty.ty_private = Asttypes.Private in
          ( match ty.ty_manifest with
          | None -> []
          | Some (Other typ) ->
              (Raw " = ") ::
              (Raw (if priv then "private " else "")) ::
              (self#text_of_short_type_expr (Name.father ty.ty_name) typ)
          | Some (Object_type l) ->
               (Raw (" = "^(if priv then "private " else "")^"{\n")) ::
               (List.flatten
                  (List.map
                     (fun r ->
                       [ Raw ("  " ^ r.of_name ^ " : ") ] @
                       (self#text_of_short_type_expr
                          (Name.father r.of_name)
                          r.of_type) @
                       [ Raw " ;" ] @
                       (entry_doc r.of_text))
                     l ) )
               @  [ Raw " }" ]
          ) @
          (
           match ty.ty_kind with
           | Type_abstract -> [ Newline ]
           | Type_variant l ->
               (Raw (" ="^(if priv then " private" else "")^"\n")) ::
               (List.flatten
                  (List.map
                     (fun constr ->
                       (Raw ("  | " ^ constr.vc_name)) ::
                       (Raw (self#string_of_type_args
                               constr.vc_args constr.vc_ret)) ::
                         (entry_doc constr.vc_text)
                         ) l ) )
           | Type_record l ->
               (Raw (" = "^(if priv then "private " else "")^"{\n")) ::
               (List.flatten
                  (List.map
                     (fun r ->
                       [ Raw ("  " ^ r.rf_name ^ " : ") ] @
                       (self#text_of_short_type_expr
                          (Name.father r.rf_name)
                          r.rf_type) @
                       [ Raw " ;" ] @
                        (entry_doc r.rf_text)
                        )
                     l ) )
               @  [ Raw " }" ]
           | Type_open -> [ Raw " = .." ; Newline ]
          ) ) ;
          self#index `Type ty.ty_name ; Newline ] @
        (self#text_of_info ty.ty_info) in
      self#texi_of_text t

    (** Return Texinfo code for a type extension. *)
    method texi_of_type_extension m_name te =
      Odoc_info.reset_type_names () ;
      let t =
        ( self#fixedblock (
            [ Newline ; minus ;
              Raw "type " ;
              Raw (match te.te_type_parameters with
                     | [] -> ""
                     | [ tp ] ->
                         Printf.sprintf "%s "
                           (Odoc_info.string_of_type_expr tp)
                     | l ->
                         Printf.sprintf "(%s) "
                           (String.concat ", "
                              (List.map Odoc_info.string_of_type_expr l))) ;
              Raw (self#relative_idents m_name te.te_type_name) ;
              Raw (" +=" ^
                      (if te.te_private = Asttypes.Private
                       then " private" else "")^"\n") ] @
              (List.flatten
                 (List.map
                    (fun x ->
                       (Raw ("  | " ^ (Name.simple x.xt_name))) ::
                         (Raw (self#string_of_type_args
                                 x.xt_args x.xt_ret)) ::
                         (match x.xt_alias with
                            | None -> []
                            | Some xa ->
                                [ Raw " = " ;
                                  Raw ( match xa.xa_xt with
                                          | None -> xa.xa_name
                                          | Some x -> x.xt_name ) ]) @
                         (match x.xt_text with
                            | None -> [ Newline ]
                            | Some t ->
                                (Raw (indent 5 "\n(* ") ::
                                   self#soft_fix_linebreaks 8
                                     (self#text_of_info (Some t))) @
                                  [ Raw " *)" ; Newline ] ) @
                         [self#index `Extension x.xt_name ] )
                    te.te_constructors ) ) ) ) ::
          (self#text_of_info te.te_info) in
        self#texi_of_text t

    (** Return Texinfo code for an exception. *)
    method texi_of_exception e =
      Odoc_info.reset_type_names () ;
      let t =
        [ self#fixedblock
            ( [ Newline ; minus ; Raw "exception " ;
                Raw (Name.simple e.ex_name) ;
                Raw (self#string_of_type_args e.ex_args e.ex_ret) ] @
              (match e.ex_alias with
              | None -> []
              | Some ea -> [ Raw " = " ; Raw
                               ( match ea.ea_ex with
                               | None -> ea.ea_name
                               | Some e -> e.ex_name ) ; ]
              ) ) ;
          self#index `Exception e.ex_name ; Newline ] @
        (self#text_of_info e.ex_info) in
      self#texi_of_text t


    (** Return the Texinfo code for the given module. *)
    method texi_of_module m =
      let is_alias = function
        | { m_kind = Module_alias _ } -> true
        | _ -> false in
      let is_alias_there = function
        | { m_kind = Module_alias { ma_module = None } } -> false
        | _ -> true in
      let resolve_alias_name = function
        | { m_kind = Module_alias { ma_name = name } } -> name
        | { m_name = name } -> name in
      let t =
        [ [ self#fixedblock
              [ Newline ; minus ; Raw "module " ;
                Raw (Name.simple m.m_name) ;
                Raw (if is_alias m
                then " = " ^ (resolve_alias_name m)
                else "" ) ] ] ;
          ( if is_alias_there m
          then [ Ref (resolve_alias_name m, Some RK_module, None) ;
                 Newline ; ]
          else [] ) ;
          ( if is_alias m
          then [ self#index `Module m.m_name ; Newline ]
          else [ Newline ] ) ;
          self#text_of_info m.m_info ]
      in
      self#texi_of_text (List.flatten t)

    (** Return the Texinfo code for the given module type. *)
    method texi_of_module_type mt =
      let is_alias = function
        | { mt_kind = Some (Module_type_alias _) } -> true
        | _ -> false in
      let is_alias_there = function
        | { mt_kind = Some (Module_type_alias { mta_module = None }) } -> false
        | _ -> true in
      let resolve_alias_name = function
        | { mt_kind = Some (Module_type_alias { mta_name = name }) } -> name
        | { mt_name = name } -> name in
      let t =
        [ [ self#fixedblock
              [ Newline ; minus ; Raw "module type " ;
                Raw (Name.simple mt.mt_name) ;
                Raw (if is_alias mt
                then " = " ^ (resolve_alias_name mt)
                else "" ) ] ] ;
          ( if is_alias_there mt
          then [ Ref (resolve_alias_name mt, Some RK_module_type, None) ;
                 Newline ; ]
          else [] ) ;
          ( if is_alias mt
          then [ self#index `Module_type mt.mt_name ; Newline ]
          else [ Newline ] ) ;
          self#text_of_info mt.mt_info ]
      in
      self#texi_of_text (List.flatten t)

    (** Return the Texinfo code for the given included module. *)
    method texi_of_included_module im =
      let t = [ self#fixedblock
                  ( Newline :: minus :: (Raw "include ") ::
                    ( match im.im_module with
                    | None ->
                        [ Raw im.im_name ]
                    | Some (Mod { m_name = name }) ->
                        [ Raw name ; Raw "\n     " ;
                          Ref (name, Some RK_module, None) ]
                    | Some (Modtype { mt_name = name }) ->
                        [ Raw name ; Raw "\n     " ;
                          Ref (name, Some RK_module_type, None) ]
                    ) @
                   [ Newline ] @
                   (self#text_of_info im.im_info)
                  )
              ]
      in
      self#texi_of_text t

    (** Return the Texinfo code for the given class. *)
    method texi_of_class c =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock
                  [ Newline ; minus ; Raw "class " ;
                    Raw (Name.simple c.cl_name) ] ;
                Ref (c.cl_name, Some RK_class, None) ; Newline ;
                Newline ] @ (self#text_of_info c.cl_info) in
      self#texi_of_text t

    (** Return the Texinfo code for the given class type. *)
    method texi_of_class_type ct =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock
                  [ Newline ; minus ; Raw "class type " ;
                    Raw (Name.simple ct.clt_name) ] ;
                Ref (ct.clt_name, Some RK_class_type, None) ; Newline ;
                Newline ] @ (self#text_of_info ct.clt_info) in
      self#texi_of_text t

    (** Return the Texinfo code for the given class element. *)
    method texi_of_class_element class_name class_ele =
      match class_ele with
      | Class_attribute att -> self#texi_of_attribute att
      | Class_method met -> self#texi_of_method met
      | Class_comment t -> self#texi_of_text t

    (** Return the Texinfo code for the given module element. *)
    method texi_of_module_element module_name module_ele =
      (match module_ele with
      | Element_module m -> self#texi_of_module m
      | Element_module_type mt -> self#texi_of_module_type mt
      | Element_included_module im -> self#texi_of_included_module im
      | Element_class c -> self#texi_of_class c
      | Element_class_type ct -> self#texi_of_class_type ct
      | Element_value v -> self#texi_of_value v
      | Element_type_extension te -> self#texi_of_type_extension module_name te
      | Element_exception e -> self#texi_of_exception e
      | Element_type t -> self#texi_of_type t
      | Element_module_comment t ->
          self#texi_of_text (Newline :: t @ [Newline])
      )

    (** {3 Generating methods }
       These methods write Texinfo code to an [out_channel] *)

    (** Generate the Texinfo code for the given list of inherited classes.*)
    method generate_inheritance_info chanout inher_l =
      let f inh =
        match inh.ic_class with
        | None -> (* we can't make the reference *)
            (Code inh.ic_name) ::
            (match inh.ic_text with
            | None -> []
            | Some t -> Newline :: t)
        | Some cct -> (* we can create the reference *)
            let kind =
              match cct with
              | Cl _ -> Some RK_class
              | Cltype _ -> Some RK_class_type in
            (Code inh.ic_name) ::
            (Ref (inh.ic_name, kind, None)) ::
            ( match inh.ic_text with
            | None -> []
            | Some t -> Newline :: t)
      in
      let text = [
        Bold [ Raw Odoc_messages.inherits ] ;
        List (List.map f inher_l) ; Newline ]
      in
      puts chanout (self#texi_of_text text)



    (** Generate the Texinfo code for the inherited classes
       of the given class. *)
    method generate_class_inheritance_info chanout cl =
      let rec iter_kind = function
        | Class_structure ([], _) -> ()
        | Class_structure (l, _) ->
            self#generate_inheritance_info chanout l
        | Class_constraint (k, _) -> iter_kind k
        | Class_apply _
        | Class_constr _ -> ()
      in
      iter_kind cl.cl_kind



    (** Generate the Texinfo code for the inherited classes
       of the given class type. *)
    method generate_class_type_inheritance_info chanout clt =
      match clt.clt_kind with
      | Class_signature ([], _) ->
          ()
      | Class_signature (l, _) ->
          self#generate_inheritance_info chanout l
      | Class_type _ ->
          ()

    (** Generate the Texinfo code for the given class,
       in the given out channel. *)
    method generate_for_class chanout c =
     try
      Odoc_info.reset_type_names () ;
      let depth = Name.depth c.cl_name in
      let title = [
        self#node depth c.cl_name ;
        Title (depth, None, [ Raw (Odoc_messages.clas ^ " ") ;
                                    Code c.cl_name ]) ;
        self#index `Class c.cl_name ] in
      puts chanout (self#texi_of_text title) ;

      if is c.cl_info
      then begin
        let descr = [ Title (succ depth, None,
                             [ Raw Odoc_messages.description ]) ] in
        puts chanout (self#texi_of_text descr) ;
        puts chanout (self#texi_of_info c.cl_info)
      end ;

      let intf = [ Title (succ depth, None,
                          [ Raw Odoc_messages.interface]) ] in
      puts chanout (self#texi_of_text intf);
      self#generate_class_inheritance_info chanout c ;
      List.iter
        (fun ele -> puts chanout
            (self#texi_of_class_element c.cl_name ele))
        (Class.class_elements ~trans:false c)
     with Aliased_node -> ()


    (** Generate the Texinfo code for the given class type,
       in the given out channel. *)
    method generate_for_class_type chanout ct =
     try
      Odoc_info.reset_type_names () ;
      let depth = Name.depth ct.clt_name in
      let title = [
        self#node depth ct.clt_name ;
        Title (depth, None, [ Raw (Odoc_messages.class_type ^ " ") ;
                                    Code ct.clt_name ]) ;
        self#index `Class_type ct.clt_name ] in
      puts chanout (self#texi_of_text title) ;

      if is ct.clt_info
      then begin
        let descr = [ Title (succ depth, None,
                             [ Raw Odoc_messages.description ]) ] in
        puts chanout (self#texi_of_text descr) ;
        puts chanout (self#texi_of_info ct.clt_info)
      end ;

      let intf = [ Title (succ depth, None,
                          [ Raw Odoc_messages.interface ]) ] in
      puts chanout (self#texi_of_text intf) ;
      self#generate_class_type_inheritance_info chanout ct;
      List.iter
        (fun ele -> puts chanout
            (self#texi_of_class_element ct.clt_name ele))
        (Class.class_type_elements ~trans:false ct)
     with Aliased_node -> ()


    (** Generate the Texinfo code for the given module type,
       in the given out channel. *)
    method generate_for_module_type chanout mt =
     try
      let depth = Name.depth mt.mt_name in
      let title = [
        self#node depth mt.mt_name ;
        Title (depth, None, [ Raw (Odoc_messages.module_type ^ " ") ;
                              Code mt.mt_name ]) ;
        self#index `Module_type mt.mt_name ; Newline ] in
      puts chanout (self#texi_of_text title) ;

      if is mt.mt_info
      then begin
        let descr = [ Title (succ depth, None,
                             [ Raw Odoc_messages.description ]) ] in
        puts chanout (self#texi_of_text descr) ;
        puts chanout (self#texi_of_info mt.mt_info)
      end ;

      let mt_ele = Module.module_type_elements ~trans:true mt in
      let subparts = module_subparts mt_ele in
      if depth < maxdepth && subparts <> []
      then begin
        let menu = Texi.ifinfo
            ( self#heading (succ depth) [ Raw "Subparts" ]) in
        puts chanout menu ;
        Texi.generate_menu chanout (subparts :> menu_data)
      end ;

      let intf = [ Title (succ depth, None,
                          [ Raw Odoc_messages.interface ]) ] in
      puts chanout (self#texi_of_text intf) ;
      List.iter
        (fun ele -> puts chanout
            (self#texi_of_module_element mt.mt_name ele))
        mt_ele ;

      (* create sub parts for modules, module types, classes and class types *)
      List.iter
        (function
          | `Module m -> self#generate_for_module chanout m
          | `Module_type mt -> self#generate_for_module_type chanout mt
          | `Class c -> self#generate_for_class chanout c
          | `Class_type ct -> self#generate_for_class_type chanout ct)
        subparts
     with Aliased_node -> ()

    (** Generate the Texinfo code for the given module,
       in the given out channel. *)
    method generate_for_module chanout m =
     try
      Odoc_info.verbose ("Generate for module " ^ m.m_name) ;
      let depth = Name.depth m.m_name in
      let title = [
        self#node depth m.m_name ;
        Title (depth, None,
               if m.m_text_only then
                 [ Raw m.m_name ]
               else
                 [ Raw (Odoc_messages.modul ^ " ") ;
                   Code m.m_name ]
              ) ;
        self#index `Module m.m_name ; Newline ] in
      puts chanout (self#texi_of_text title) ;

      if is m.m_info
      then begin
        let descr = [ Title (succ depth, None,
                             [ Raw Odoc_messages.description ]) ] in
        puts chanout (self#texi_of_text descr) ;
        puts chanout (self#texi_of_info m.m_info)
      end ;

      let m_ele = Module.module_elements ~trans:true m in
      let subparts = module_subparts m_ele in
      if depth < maxdepth && subparts <> []
      then begin
        let menu = Texi.ifinfo
            ( self#heading (succ depth) [ Raw "Subparts" ]) in
        puts chanout menu ;
        Texi.generate_menu chanout (subparts :> menu_data)
      end ;

      let intf = [ Title (succ depth, None,
                          [ Raw Odoc_messages.interface]) ] in
      puts chanout (self#texi_of_text intf) ;

      List.iter
        (fun ele -> puts chanout
            (self#texi_of_module_element m.m_name ele))
        m_ele ;

      (* create sub nodes for modules, module types, classes and class types *)
      List.iter
        (function
          | `Module m -> self#generate_for_module chanout m
          | `Module_type mt -> self#generate_for_module_type chanout mt
          | `Class c -> self#generate_for_class chanout c
          | `Class_type ct -> self#generate_for_class_type chanout ct )
        subparts
     with Aliased_node -> ()


    (** Writes the header of the TeXinfo document. *)
    method generate_texi_header chan texi_filename m_list =
      let title = match !Global.title with
      | None -> ""
      | Some s -> self#escape s in
      let filename =
        if texi_filename <> "ocamldoc.texi"
        then
          let fn = Filename.basename texi_filename in
          (if Filename.check_suffix fn ".texi"
          then Filename.chop_suffix fn ".texi"
          else fn) ^ ".info"
        else
          if title <> ""
          then title ^ ".info"
          else "doc.info"
      in
      (* write a standard Texinfo header *)
      List.iter
        (puts_nl chan)
        (List.flatten
           [ [ "\\input texinfo   @c -*-texinfo-*-" ;
               "@c %**start of header" ;
               "@setfilename " ^ filename ;
               "@settitle " ^ title ;
               "@c %**end of header" ; ] ;

             (if !Global.with_index then
               List.map
                 (fun ind ->
                   "@defcodeindex " ^ (indices ind))
                 indices_to_build
             else []) ;

             [ Texi.dirsection !info_section ] ;

             Texi.direntry
               (if !info_entry <> []
               then !info_entry
               else [ Printf.sprintf "* %s: (%s)."
                        title
                        (Filename.chop_suffix filename ".info") ]) ;

             [ "@ifinfo" ;
               "This file was generated by Ocamldoc using the Texinfo generator." ;
               "@end ifinfo" ;

               "@c no titlepage." ;

               "@node Top, , , (dir)" ;
               "@top "^ title ; ]
           ] ) ;

      (* insert the intro file *)
      begin
        match !Odoc_info.Global.intro_file with
        | None when title <> "" ->
            puts_nl chan "@ifinfo" ;
            puts_nl chan ("Documentation for " ^ title) ;
            puts_nl chan "@end ifinfo"
        | None ->
            puts_nl chan "@c no title given"
        | Some f ->
            nl chan ;
            puts_nl chan
              (self#texi_of_info
                 (Some (Odoc_info.info_of_comment_file m_list f)))
      end ;

      (* write a top menu *)
      Texi.generate_menu chan
        ((List.map (fun m -> `Module m) m_list) @
         (if !Global.with_index then
           let indices_names_to_build = List.map indices indices_to_build in
           List.rev
             (List.fold_left
                (fun acc ->
                  function (longname, shortname)
                      when List.mem shortname indices_names_to_build ->
                        (`Index (longname ^ " index")) :: acc
                    | _ -> acc)
                [ `Comment "Indices :" ; `Blank ]
                indices_names )
         else [] ))


    (** Writes the trailer of the TeXinfo document. *)
    method generate_texi_trailer chan =
      nl chan ;
      if !Global.with_index
      then
        let indices_names_to_build = List.map indices indices_to_build in
        List.iter (puts_nl chan)
          (List.flatten
             (List.map
                (fun (longname, shortname) ->
                  if List.mem shortname indices_names_to_build
                  then [ "@node " ^ longname ^ " index," ;
                         "@unnumbered " ^ longname ^ " index" ;
                         "@printindex " ^ shortname ; ]
                  else [])
                indices_names )) ;
      if !Global.with_toc
      then puts_nl chan "@contents" ;
      puts_nl chan "@bye"


    method do_index it =
      if not (List.mem it indices_to_build)
      then indices_to_build <- it :: indices_to_build

   (** Scan the whole module information to know which indices need to be build *)
    method scan_for_index : subparts -> unit = function
      | `Module m ->
          let m_ele = Module.module_elements ~trans:true m in
          List.iter self#scan_for_index_in_mod m_ele
      | `Module_type mt ->
          let m_ele = Module.module_type_elements ~trans:true mt in
          List.iter self#scan_for_index_in_mod m_ele
      | `Class c ->
          let c_ele = Class.class_elements ~trans:true c in
          List.iter self#scan_for_index_in_class c_ele
      | `Class_type ct ->
          let c_ele = Class.class_type_elements ~trans:true ct in
          List.iter self#scan_for_index_in_class c_ele

    method scan_for_index_in_mod = function
        (* no recursion *)
      | Element_value _ -> self#do_index `Value
      | Element_type_extension _ -> self#do_index `Extension
      | Element_exception _ -> self#do_index `Exception
      | Element_type _ -> self#do_index `Type
      | Element_included_module _
      | Element_module_comment _ -> ()
         (* recursion *)
      | Element_module m -> self#do_index `Module ;
          self#scan_for_index (`Module m)
      | Element_module_type mt -> self#do_index `Module_type ;
          self#scan_for_index (`Module_type mt)
      | Element_class c -> self#do_index `Class ;
          self#scan_for_index (`Class c)
      | Element_class_type ct -> self#do_index `Class_type ;
          self#scan_for_index (`Class_type ct)

    method scan_for_index_in_class = function
      | Class_attribute _ -> self#do_index `Class_att
      | Class_method _ -> self#do_index `Method
      | Class_comment _ -> ()


    (** Generate the Texinfo file from a module list,
       in the {!Odoc_info.Global.out_file} file. *)
    method generate module_list =
      Hashtbl.clear node_tbl ;
      let filename =
        if !Global.out_file = Odoc_messages.default_out_file
        then "ocamldoc.texi"
        else !Global.out_file in
      if !Global.with_index
      then List.iter self#scan_for_index
          (List.map (fun m -> `Module m) module_list) ;
      try
        let chanout = open_out
            (Filename.concat !Global.target_dir filename) in
        if !Global.with_header
        then self#generate_texi_header chanout filename module_list ;
        List.iter
          (self#generate_for_module chanout)
          module_list ;
        if !Global.with_trailer
        then self#generate_texi_trailer chanout ;
        close_out chanout
      with
      | Failure s
      | Sys_error s ->
          prerr_endline s ;
          incr Odoc_info.errors
  end
end

module type Texi_generator = module type of Generator
