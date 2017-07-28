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

(** Interface to the information collected in source files. *)

(** The differents kinds of element references. *)
type ref_kind = Odoc_types.ref_kind =
    RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_extension
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section of text
  | RK_recfield
  | RK_const

and text_element = Odoc_types.text_element =
  | Raw of string (** Raw text. *)
  | Code of string (** The string is source code. *)
  | CodePre of string (** The string is pre-formatted source code. *)
  | Verbatim of string (** String 'as is'. *)
  | Bold of text (** Text in bold style. *)
  | Italic of text (** Text in italic. *)
  | Emphasize of text (** Emphasized text. *)
  | Center of text (** Centered text. *)
  | Left of text (** Left alignment. *)
  | Right of text (** Right alignment. *)
  | List of text list (** A list. *)
  | Enum of text list (** An enumerated list. *)
  | Newline   (** To force a line break. *)
  | Block of text (** Like html's block quote. *)
  | Title of int * string option * text
             (** Style number, optional label, and text. *)
  | Latex of string (** A string for latex. *)
  | Link of string * text (** A reference string and the link text. *)
  | Ref of string * ref_kind option * text option
       (** A reference to an element. Complete name and kind.
        An optional text can be given to display this text instead
        of the element name.*)
  | Superscript of text (** Superscripts. *)
  | Subscript of text (** Subscripts. *)
  | Module_list of string list
       (** The table of the given modules with their abstract. *)
  | Index_list (** The links to the various indexes (values, types, ...) *)
  | Custom of string * text (** to extend \{foo syntax *)
  | Target of string * string (** (target, code) : to specify code specific to a target format *)

(** A text is a list of [text_element]. The order matters. *)
and text = text_element list

(** The different forms of references in \@see tags. *)
type see_ref = Odoc_types.see_ref =
    See_url of string
  | See_file of string
  | See_doc of string

(** Raised when parsing string to build a {!Odoc_info.text}
   structure. [(line, char, string)] *)
exception Text_syntax of int * int * string

(** The information in a \@see tag. *)
type see = see_ref * text

(** Parameter name and description. *)
type param = (string * text)

(** Raised exception name and description. *)
type raised_exception = (string * text)

(** Information in a special comment
@before 3.12.0 \@before information was not present.
*)
type info = Odoc_types.info = {
    i_desc : text option; (** The description text. *)
    i_authors : string list; (** The list of authors in \@author tags. *)
    i_version : string option; (** The string in the \@version tag. *)
    i_sees : see list; (** The list of \@see tags. *)
    i_since : string option; (** The string in the \@since tag. *)
    i_before : (string * text) list ; (** the version number and text in \@before tag *)
    i_deprecated : text option; (** The of the \@deprecated tag. *)
    i_params : param list; (** The list of parameter descriptions. *)
    i_raised_exceptions : raised_exception list; (** The list of raised exceptions. *)
    i_return_value : text option; (** The description text of the return value. *)
    i_custom : (string * text) list ; (** A text associated to a custom @-tag. *)
  }

(** Location of elements in implementation and interface files. *)
type location = Odoc_types.location = {
    loc_impl : Location.t option ; (** implementation location *)
    loc_inter : Location.t option ; (** interface location *)
  }

(** A dummy location. *)
val dummy_loc : location

(** Representation of element names. *)
module Name :
    sig
      type t = string

      (** Access to the simple name. *)
      val simple : t -> t

      (** [concat t1 t2] returns the concatenation of [t1] and [t2].*)
      val concat : t -> t -> t

      (** Return the depth of the name, i.e. the numer of levels to the root.
         Example : [depth "Toto.Tutu.name"] = [3]. *)
      val depth : t -> int

      (** Take two names n1 and n2 = n3.n4 and return n4 if n3=n1 or else n2. *)
      val get_relative : t -> t -> t

      (** Return the name of the 'father' (like [dirname] for a file name).*)
      val father : t -> t
    end

(** Representation and manipulation of method / function / class / module parameters.*)
module Parameter :
  sig
    (** {3 Types} *)
    (** Representation of a simple parameter name *)
    type simple_name = Odoc_parameter.simple_name =
        {
          sn_name : string ;
          sn_type : Types.type_expr ;
          mutable sn_text : text option ;
        }

    (** Representation of parameter names. We need it to represent parameter names in tuples.
       The value [Tuple ([], t)] stands for an anonymous parameter.*)
    type param_info = Odoc_parameter.param_info =
        Simple_name of simple_name
      | Tuple of param_info list * Types.type_expr

    (** A parameter is just a param_info.*)
    type parameter = param_info

    (** {3 Functions} *)
    (** Acces to the name as a string. For tuples, parenthesis and commas are added. *)
    val complete_name : parameter -> string

    (** Access to the complete type. *)
    val typ : parameter -> Types.type_expr

    (** Access to the list of names ; only one for a simple parameter, or
       a list for a tuple. *)
    val names : parameter -> string list

    (** Access to the description of a specific name.
       @raise Not_found if no description is associated to the given name. *)
    val desc_by_name : parameter -> string -> text option

    (** Access to the type of a specific name.
       @raise Not_found if no type is associated to the given name. *)
    val type_by_name : parameter -> string -> Types.type_expr
  end

(** Representation and manipulation of extensions. *)
module Extension :
  sig
    type private_flag = Odoc_extension.private_flag =
      Private | Public

    (** Used when the extension is a rebind of another extension,
       when we have [extension Xt = Target_xt].*)
    type extension_alias = Odoc_extension.extension_alias =
        {
          xa_name : Name.t ; (** The complete name of the target extension. *)
          mutable xa_xt : t_extension_constructor option ; (** The target extension, if we found it.*)
        }

    and t_extension_constructor = Odoc_extension.t_extension_constructor =
        {
          xt_name : Name.t ;
          xt_args: Types.type_expr list ; (** the types of the parameters *)
          xt_ret: Types.type_expr option ; (** the optional return type of the extension *)
          xt_type_extension: t_type_extension ; (** the type extension containing this constructor *)
          xt_alias: extension_alias option ; (** [None] when the extension is not a rebind. *)
          mutable xt_loc: Odoc_types.location ;
          mutable xt_text: Odoc_types.info option ; (** optional user description *)
        }

    and t_type_extension = Odoc_extension.t_type_extension =
        {
          mutable te_info : info option ; (** Information found in the optional associated comment. *)
          te_type_name : Name.t ; (** The type of the extension *)
          te_type_parameters : Types.type_expr list;
          te_private : private_flag ;
          mutable te_constructors: t_extension_constructor list;
          mutable te_loc : location ;
          mutable te_code : string option ;
        }

    (** Access to the extensions in a group. *)
    val extension_constructors : t_type_extension -> t_extension_constructor list

  end

(** Representation and manipulation of exceptions. *)
module Exception :
  sig
    (** Used when the exception is a rebind of another exception,
       when we have [exception Ex = Target_ex].*)
    type exception_alias = Odoc_exception.exception_alias =
        {
          ea_name : Name.t ; (** The complete name of the target exception. *)
          mutable ea_ex : t_exception option ; (** The target exception, if we found it.*)
        }

    and t_exception = Odoc_exception.t_exception =
        {
          ex_name : Name.t ;
          mutable ex_info : info option ; (** Information found in the optional associated comment. *)
          ex_args : Types.type_expr list ; (** The types of the parameters. *)
          ex_ret : Types.type_expr option ; (** The the optional return type of the exception. *)
          ex_alias : exception_alias option ; (** [None] when the exception is not a rebind. *)
          mutable ex_loc : location ;
          mutable ex_code : string option ;
        }
  end

(** Representation and manipulation of types.*)
module Type :
  sig
    type private_flag = Odoc_type.private_flag =
      Private | Public

    (** Description of a variant type constructor. *)
    type variant_constructor = Odoc_type.variant_constructor =
        {
          vc_name : string ; (** Name of the constructor. *)
          vc_args : Types.type_expr list ; (** Arguments of the constructor. *)
          vc_ret : Types.type_expr option ;
          mutable vc_text : info option ; (** Optional description in the associated comment. *)
        }

    (** Description of a record type field. *)
    type record_field = Odoc_type.record_field =
        {
          rf_name : string ; (** Name of the field. *)
          rf_mutable : bool ; (** [true] if mutable. *)
          rf_type : Types.type_expr ; (** Type of the field. *)
          mutable rf_text : info option ; (** Optional description in the associated comment.*)
        }

    (** The various kinds of a type. *)
    type type_kind = Odoc_type.type_kind =
        Type_abstract (** Type is abstract, for example [type t]. *)
      | Type_variant of variant_constructor list
                   (** constructors *)
      | Type_record of record_field list
                   (** fields *)
      | Type_open (** Type is open *)

    type object_field = Odoc_type.object_field = {
      of_name : string ;
      of_type : Types.type_expr ;
      mutable of_text : Odoc_types.info option ; (** optional user description *)
    }

    type type_manifest = Odoc_type.type_manifest =
      | Other of Types.type_expr (** Type manifest directly taken from Typedtre. *)
      | Object_type of object_field list

    (** Representation of a type. *)
    type t_type = Odoc_type.t_type =
        {
          ty_name : Name.t ; (** Complete name of the type. *)
          mutable ty_info : info option ; (** Information found in the optional associated comment. *)
          ty_parameters : (Types.type_expr * bool * bool) list ;
                    (** type parameters: (type, covariant, contravariant) *)
          ty_kind : type_kind; (** Type kind. *)
          ty_private : private_flag; (** Private or public type. *)
          ty_manifest : type_manifest option ;
          mutable ty_loc : location ;
          mutable ty_code : string option;
        }

  end

(** Representation and manipulation of values, class attributes and class methods. *)
module Value :
  sig
    (** Representation of a value. *)
    type t_value = Odoc_value.t_value =
        {
          val_name : Name.t ; (** Complete name of the value. *)
          mutable val_info : info option ; (** Information found in the optional associated comment. *)
          val_type : Types.type_expr ; (** Type of the value. *)
          val_recursive : bool ; (** [true] if the value is recursive. *)
          mutable val_parameters : Odoc_parameter.parameter list ; (** The parameters, if any. *)
          mutable val_code : string option ; (** The code of the value, if we had the only the implementation file. *)
          mutable val_loc : location ;
        }

    (** Representation of a class attribute. *)
    type t_attribute = Odoc_value.t_attribute =
        {
          att_value : t_value ; (** an attribute has almost all the same information as a value *)
          att_mutable : bool ;  (** [true] if the attribute is mutable. *)
          att_virtual : bool ;  (** [true] if the attribute is virtual. *)
        }

    (** Representation of a class method. *)
    type t_method = Odoc_value.t_method =
        {
          met_value : t_value ; (** a method has almost all the same information as a value *)
          met_private : bool ;  (** [true] if the method is private.*)
          met_virtual : bool ;  (** [true] if the method is virtual. *)
        }

    (** Return [true] if the value is a function, i.e. it has a functional type. *)
    val is_function : t_value -> bool

    (** Access to the description associated to the given parameter name.*)
    val value_parameter_text_by_name : t_value -> string -> text option
  end

(** Representation and manipulation of classes and class types.*)
module Class :
  sig
    (** {3 Types} *)
    (** To keep the order of elements in a class. *)
    type class_element = Odoc_class.class_element =
        Class_attribute of Value.t_attribute
      | Class_method of Value.t_method
      | Class_comment of text

    (** Used when we can reference a t_class or a t_class_type. *)
    type cct = Odoc_class.cct =
        Cl of t_class
      | Cltype of t_class_type * Types.type_expr list (** Class type and type parameters. *)

    and inherited_class = Odoc_class.inherited_class =
        {
          ic_name : Name.t ; (** Complete name of the inherited class. *)
          mutable ic_class : cct option ; (** The associated t_class or t_class_type. *)
          ic_text : text option ; (** The inheritance description, if any. *)
        }

    and class_apply = Odoc_class.class_apply =
        {
          capp_name : Name.t ; (** The complete name of the applied class. *)
          mutable capp_class : t_class option;  (** The associated t_class if we found it. *)
          capp_params : Types.type_expr list; (** The type of expressions the class is applied to. *)
          capp_params_code : string list ; (** The code of these exprssions. *)
        }

    and class_constr = Odoc_class.class_constr =
        {
          cco_name : Name.t ; (** The complete name of the applied class. *)
          mutable cco_class : cct option;
              (** The associated class or class type if we found it. *)
          cco_type_parameters : Types.type_expr list; (** The type parameters of the class, if needed. *)
        }

    and class_kind = Odoc_class.class_kind =
        Class_structure of inherited_class list * class_element list
        (** An explicit class structure, used in implementation and interface. *)
      | Class_apply of class_apply
        (** Application/alias of a class, used in implementation only. *)
      | Class_constr of class_constr
        (** A class used to give the type of the defined class,
           instead of a structure, used in interface only.
           For example, it will be used with the name [M1.M2....bar]
           when the class foo is defined like this :
           [class foo : int -> bar] *)
      | Class_constraint of class_kind * class_type_kind
        (** A class definition with a constraint. *)

    (** Representation of a class. *)
    and t_class = Odoc_class.t_class =
        {
          cl_name : Name.t ; (** Complete name of the class. *)
          mutable cl_info : info option ; (** Information found in the optional associated comment. *)
          cl_type : Types.class_type ; (** Type of the class. *)
          cl_type_parameters : Types.type_expr list ; (** Type parameters. *)
          cl_virtual : bool ; (** [true] when the class is virtual. *)
          mutable cl_kind : class_kind ; (** The way the class is defined. *)
          mutable cl_parameters : Parameter.parameter list ; (** The parameters of the class. *)
          mutable cl_loc : location ;
        }

    and class_type_alias = Odoc_class.class_type_alias =
        {
          cta_name : Name.t ; (** Complete name of the target class type. *)
          mutable cta_class : cct option ;  (** The target t_class or t_class_type, if we found it.*)
          cta_type_parameters : Types.type_expr list ; (** The type parameters. A VOIR : mettre des string ? *)
        }

    and class_type_kind = Odoc_class.class_type_kind =
        Class_signature of inherited_class list * class_element list
      | Class_type of class_type_alias (** A class type eventually applied to type args. *)

    (** Representation of a class type. *)
    and t_class_type = Odoc_class.t_class_type =
        {
          clt_name : Name.t ; (** Complete name of the type. *)
          mutable clt_info : info option ; (** Information found in the optional associated comment. *)
          clt_type : Types.class_type ;
          clt_type_parameters : Types.type_expr list ; (** Type parameters. *)
          clt_virtual : bool ; (** [true] if the class type is virtual *)
          mutable clt_kind : class_type_kind ; (** The way the class type is defined. *)
          mutable clt_loc : location ;
        }

    (** {3 Functions} *)

    (** Access to the elements of a class. *)
    val class_elements : ?trans:bool -> t_class -> class_element list

    (** Access to the list of class attributes. *)
    val class_attributes : ?trans:bool -> t_class -> Value.t_attribute list

    (** Access to the description associated to the given class parameter name. *)
    val class_parameter_text_by_name : t_class -> string -> text option

    (** Access to the methods of a class. *)
    val class_methods : ?trans:bool -> t_class -> Value.t_method list

    (** Access to the comments of a class. *)
    val class_comments : ?trans:bool -> t_class -> text list

    (** Access to the elements of a class type. *)
    val class_type_elements : ?trans:bool -> t_class_type -> class_element list

    (** Access to the list of class type attributes. *)
    val class_type_attributes : ?trans:bool -> t_class_type -> Value.t_attribute list

    (** Access to the description associated to the given class type parameter name. *)
    val class_type_parameter_text_by_name : t_class_type -> string -> text option

    (** Access to the methods of a class type. *)
    val class_type_methods : ?trans:bool -> t_class_type -> Value.t_method list

    (** Access to the comments of a class type. *)
    val class_type_comments : ?trans:bool -> t_class_type -> text list
  end

(** Representation and manipulation of modules and module types. *)
module Module :
  sig
    (** {3 Types} *)
    (** To keep the order of elements in a module. *)
    type module_element = Odoc_module.module_element =
        Element_module of t_module
      | Element_module_type of t_module_type
      | Element_included_module of included_module
      | Element_class of Class.t_class
      | Element_class_type of Class.t_class_type
      | Element_value of Value.t_value
      | Element_type_extension of Extension.t_type_extension
      | Element_exception of Exception.t_exception
      | Element_type of Type.t_type
      | Element_module_comment of text

    (** Used where we can reference t_module or t_module_type. *)
    and mmt = Odoc_module.mmt =
      | Mod of t_module
      | Modtype of t_module_type

    and included_module = Odoc_module.included_module =
        {
          im_name : Name.t ; (** Complete name of the included module. *)
          mutable im_module : mmt option ; (** The included module or module type, if we found it. *)
          mutable im_info : Odoc_types.info option ; (** comment associated to the includ directive *)
        }

    and module_alias = Odoc_module.module_alias =
        {
          ma_name : Name.t ; (** Complete name of the target module. *)
          mutable ma_module : mmt option ; (** The real module or module type if we could associate it. *)
        }

    and module_parameter = Odoc_module.module_parameter = {
        mp_name : string ; (** the name *)
        mp_type : Types.module_type option ; (** the type *)
        mp_type_code : string ; (** the original code *)
        mp_kind : module_type_kind ; (** the way the parameter was built *)
      }

    (** Different kinds of a module. *)
    and module_kind = Odoc_module.module_kind =
      | Module_struct of module_element list (** A complete module structure. *)
      | Module_alias of module_alias (** Complete name and corresponding module if we found it *)
      | Module_functor of module_parameter * module_kind
                     (** A functor, with its parameter and the rest of its definition *)
      | Module_apply of module_kind * module_kind
                     (** A module defined by application of a functor. *)
      | Module_with of module_type_kind * string
                     (** A module whose type is a with ... constraint.
                        Should appear in interface files only. *)
      | Module_constraint of module_kind * module_type_kind
                     (** A module constraint by a module type. *)
      | Module_typeof of string (** by now only the code of the module expression *)
      | Module_unpack of string * module_type_alias (** code of the expression and module type alias *)

    (** Representation of a module. *)
    and t_module = Odoc_module.t_module =
        {
          m_name : Name.t ; (** Complete name of the module. *)
          mutable m_type : Types.module_type ; (** The type of the module. *)
          mutable m_info : info option ; (** Information found in the optional associated comment. *)
          m_is_interface : bool ; (** [true] for modules read from interface files *)
          m_file : string ; (** The file the module is defined in. *)
          mutable m_kind : module_kind ; (** The way the module is defined. *)
          mutable m_loc : location ;
          mutable m_top_deps : Name.t list ; (** The toplevels module names this module depends on. *)
          mutable m_code : string option ; (** The whole code of the module *)
          mutable m_code_intf : string option ; (** The whole code of the interface of the module *)
          m_text_only : bool ; (** [true] if the module comes from a text file *)
        }

    and module_type_alias = Odoc_module.module_type_alias =
        {
          mta_name : Name.t ; (** Complete name of the target module type. *)
          mutable mta_module : t_module_type option ; (** The real module type if we could associate it. *)
        }

    (** Different kinds of module type. *)
    and module_type_kind = Odoc_module.module_type_kind =
      | Module_type_struct of module_element list (** A complete module signature. *)
      | Module_type_functor of module_parameter * module_type_kind
            (** A functor, with its parameter and the rest of its definition *)
      | Module_type_alias of module_type_alias
            (** Complete alias name and corresponding module type if we found it. *)
      | Module_type_with of module_type_kind * string
            (** The module type kind and the code of the with constraint. *)
      | Module_type_typeof of string
            (** by now only the code of the module expression *)

    (** Representation of a module type. *)
    and t_module_type = Odoc_module.t_module_type =
        {
          mt_name : Name.t ; (** Complete name of the module type. *)
          mutable mt_info : info option ; (** Information found in the optional associated comment. *)
          mutable mt_type : Types.module_type option ; (** [None] means that the module type is abstract. *)
          mt_is_interface : bool ; (** [true] for modules read from interface files. *)
          mt_file : string ; (** The file the module type is defined in. *)
          mutable mt_kind : module_type_kind option ;
              (** The way the module is defined. [None] means that module type is abstract.
                 It is always [None] when the module type was extracted from the implementation file.
                 That means module types are only analysed in interface files. *)
          mutable mt_loc : location ;
        }

    (** {3 Functions for modules} *)

    (** Access to the elements of a module. *)
    val module_elements : ?trans:bool -> t_module -> module_element list

    (** Access to the submodules of a module. *)
    val module_modules : ?trans:bool -> t_module -> t_module list

    (** Access to the module types of a module. *)
    val module_module_types : ?trans:bool -> t_module -> t_module_type list

    (** Access to the included modules of a module. *)
    val module_included_modules : ?trans:bool-> t_module -> included_module list

    (** Access to the type extensions of a module. *)
    val module_type_extensions : ?trans:bool-> t_module -> Extension.t_type_extension list

    (** Access to the exceptions of a module. *)
    val module_exceptions : ?trans:bool-> t_module -> Exception.t_exception list

    (** Access to the types of a module. *)
    val module_types : ?trans:bool-> t_module -> Type.t_type list

    (** Access to the values of a module. *)
    val module_values : ?trans:bool -> t_module -> Value.t_value list

    (** Access to functional values of a module. *)
    val module_functions : ?trans:bool-> t_module -> Value.t_value list

    (** Access to non-functional values of a module. *)
    val module_simple_values : ?trans:bool-> t_module -> Value.t_value list

    (** Access to the classes of a module. *)
    val module_classes : ?trans:bool-> t_module -> Class.t_class list

    (** Access to the class types of a module. *)
    val module_class_types : ?trans:bool-> t_module -> Class.t_class_type list

    (** The list of classes defined in this module and all its submodules and functors. *)
    val module_all_classes : ?trans:bool-> t_module -> Class.t_class list

    (** [true] if the module is functor. *)
    val module_is_functor : t_module -> bool

    (** The list of couples (module parameter, optional description). *)
    val module_parameters : ?trans:bool-> t_module -> (module_parameter * text option) list

    (** The list of module comments. *)
    val module_comments : ?trans:bool-> t_module -> text list

    (** {3 Functions for module types} *)

    (** Access to the elements of a module type. *)
    val module_type_elements : ?trans:bool-> t_module_type -> module_element list

    (** Access to the submodules of a module type. *)
    val module_type_modules : ?trans:bool-> t_module_type -> t_module list

    (** Access to the module types of a module type. *)
    val module_type_module_types : ?trans:bool-> t_module_type -> t_module_type list

    (** Access to the included modules of a module type. *)
    val module_type_included_modules : ?trans:bool-> t_module_type -> included_module list

    (** Access to the exceptions of a module type. *)
    val module_type_exceptions : ?trans:bool-> t_module_type -> Exception.t_exception list

    (** Access to the types of a module type. *)
    val module_type_types : ?trans:bool-> t_module_type -> Type.t_type list

    (** Access to the values of a module type. *)
    val module_type_values : ?trans:bool-> t_module_type -> Value.t_value list

    (** Access to functional values of a module type. *)
    val module_type_functions : ?trans:bool-> t_module_type -> Value.t_value list

    (** Access to non-functional values of a module type. *)
    val module_type_simple_values : ?trans:bool-> t_module_type -> Value.t_value list

    (** Access to the classes of a module type. *)
    val module_type_classes : ?trans:bool-> t_module_type -> Class.t_class list

    (** Access to the class types of a module type. *)
    val module_type_class_types : ?trans:bool-> t_module_type -> Class.t_class_type list

    (** The list of classes defined in this module type and all its submodules and functors. *)
    val module_type_all_classes : ?trans:bool-> t_module_type -> Class.t_class list

    (** [true] if the module type is functor. *)
    val module_type_is_functor : t_module_type -> bool

    (** The list of couples (module parameter, optional description). *)
    val module_type_parameters : ?trans:bool-> t_module_type -> (module_parameter * text option) list

    (** The list of module comments. *)
    val module_type_comments : ?trans:bool-> t_module_type -> text list
  end


(** {3 Getting strings from values} *)

(** This function is used to reset the names of type variables.
   It must be called when printing the whole type of a function,
   but not when printing the type of its parameters. Same for
   classes (call it) and methods and attributes (don't call it).*)
val reset_type_names : unit -> unit

(** [string_of_variance t (covariant, invariant)] returns ["+"] if
   the given information means "covariant", ["-"] if the it means
   "contravariant", orelse [""], and always [""] if the given
   type is not an abstract type with no manifest (i.e. no need
   for the variance to be printed.*)
val string_of_variance : Type.t_type -> (bool * bool) -> string

(** This function returns a string representing a Types.type_expr. *)
val string_of_type_expr : Types.type_expr -> string

(** @return a string to display the parameters of the given class,
   in the same form as the compiler. *)
val string_of_class_params : Class.t_class -> string

(** This function returns a string to represent the given list of types,
   with a given separator. *)
val string_of_type_list : ?par: bool -> string -> Types.type_expr list -> string

(** This function returns a string to represent the list of type parameters
   for the given type. *)
val string_of_type_param_list : Type.t_type -> string

(** This function returns a string to represent the list of type parameters
   for the given type extension. *)
val string_of_type_extension_param_list : Extension.t_type_extension -> string

(** This function returns a string to represent the given list of
   type parameters of a class or class type,
   with a given separator. *)
val string_of_class_type_param_list : Types.type_expr list -> string

(** This function returns a string representing a [Types.module_type].
   @param complete indicates if we must print complete signatures
   or just [sig end]. Default if [false].
   @param code if [complete = false] and the type contains something else
   than identificators and functors, then the given code is used.
*)
val string_of_module_type : ?code: string -> ?complete: bool -> Types.module_type -> string

(** This function returns a string representing a [Types.class_type].
   @param complete indicates if we must print complete signatures
   or just [object end]. Default if [false].
*)
val string_of_class_type : ?complete: bool -> Types.class_type -> string


(** Get a string from a text. *)
val string_of_text : text -> string

(** Get a string from an info structure. *)
val string_of_info : info -> string

(** @return a string to describe the given type. *)
val string_of_type : Type.t_type -> string

(** @return a string to describe the given type extension. *)
val string_of_type_extension : Extension.t_type_extension -> string

(** @return a string to describe the given exception. *)
val string_of_exception : Exception.t_exception -> string

(** @return a string to describe the given value. *)
val string_of_value : Value.t_value -> string

(** @return a string to describe the given attribute. *)
val string_of_attribute : Value.t_attribute -> string

(** @return a string to describe the given method. *)
val string_of_method : Value.t_method -> string

(** {3 Miscelaneous functions} *)

(** Return the first sentence (until the first dot followed by a blank
   or the first blank line) of a text.
   Don't stop in the middle of [Code], [CodePre], [Verbatim], [List], [Enum],
   [Latex], [Link], [Ref], [Subscript] or [Superscript]. *)
val first_sentence_of_text : text -> text

(** Return the first sentence (until the first dot followed by a blank
   or the first blank line) of a text, and the remaining text after.
   Don't stop in the middle of [Code], [CodePre], [Verbatim], [List], [Enum],
   [Latex], [Link], [Ref], [Subscript] or [Superscript].*)
val first_sentence_and_rest_of_text : text -> text * text

(** Return the given [text] without any title or list. *)
val text_no_title_no_list : text -> text

(** [concat sep l] concats the given list of text [l], each separated with
   the text [sep]. *)
val text_concat : Odoc_types.text -> Odoc_types.text list -> Odoc_types.text

(** Return the list of titles in a [text].
   A title is a title level, an optional label and a text.*)
val get_titles_in_text : text -> (int * string option * text) list

(** Take a sorted list of elements, a function to get the name
   of an element and return the list of list of elements,
   where each list group elements beginning by the same letter.
   Since the original list is sorted, elements whose name does not
   begin with a letter should be in the first returned list.*)
val create_index_lists : 'a list -> ('a -> string) -> 'a list list

(** Take a type and remove the option top constructor. This is
   useful when printing labels, we we then remove the top option contructor
   for optional labels.*)
val remove_option : Types.type_expr -> Types.type_expr

(** Return [true] if the given label is optional.*)
val is_optional : string -> bool

(** Return the label name for the given label,
   i.e. removes the beginning '?' if present.*)
val label_name : string -> string

(** Return the given name where the module name or
   part of it was removed, according to the list of modules
   which must be hidden (cf {!Odoc_args.hidden_modules})*)
val use_hidden_modules : Name.t -> Name.t

(** Print the given string if the verbose mode is activated. *)
val verbose : string -> unit

(** Print a warning message to stderr.
   If warnings must be treated as errors, then the
   error counter is incremented. *)
val warning : string -> unit

(** A flag to indicate whether ocamldoc warnings must be printed or not. *)
val print_warnings : bool ref

(** Increment this counter when an error is encountered.
   The ocamldoc tool will print the number of errors
   encountered exit with code 1 if this number is greater
   than 0. *)
val errors : int ref

(** Apply a function to an optional value. *)
val apply_opt : ('a -> 'b) -> 'a option -> 'b option

(** Apply a function to a first value if it is
   not different from a second value. If the two values
   are different, return the second one.*)
val apply_if_equal : ('a -> 'a) -> 'a -> 'a -> 'a

(** [text_of_string s] returns the text structure from the
   given string.
   @raise Text_syntax if a syntax error is encountered. *)
val text_of_string : string -> text

(** [text_string_of_text text] returns the string representing
   the given [text]. This string can then be parsed again
   by {!Odoc_info.text_of_string}.*)
val text_string_of_text : text -> string

(** [info_of_string s] parses the given string
   like a regular ocamldoc comment and return an
   {!Odoc_info.info} structure.
   @return an empty structure if there was a syntax error. TODO: change this
*)
val info_of_string : string -> info

(** [info_string_of_info info] returns the string representing
   the given [info]. This string can then be parsed again
   by {!Odoc_info.info_of_string}.*)
val info_string_of_info : info -> string

(** [info_of_comment_file file] parses the given file
   and return an {!Odoc_info.info} structure. The content of the
   file must have the same syntax as the content of a special comment.
   The given module list is used for cross reference.
   @raise Failure is the file could not be opened or there is a
   syntax error.
*)
val info_of_comment_file : Module.t_module list -> string -> info

(** [remove_ending_newline s] returns [s] without the optional ending newline. *)
val remove_ending_newline : string -> string

(** Research in elements *)
module Search :
    sig
      type result_element = Odoc_search.result_element =
          Res_module of Module.t_module
        | Res_module_type of Module.t_module_type
        | Res_class of Class.t_class
        | Res_class_type of Class.t_class_type
        | Res_value of Value.t_value
        | Res_type of Type.t_type
        | Res_extension of Extension.t_extension_constructor
        | Res_exception of Exception.t_exception
        | Res_attribute of Value.t_attribute
        | Res_method of Value.t_method
        | Res_section of string  * text
        | Res_recfield of Type.t_type * Type.record_field
        | Res_const of Type.t_type * Type.variant_constructor

      (** The type representing a research result.*)
      type search_result = result_element list

      (** Research of the elements whose name matches the given regular expression.*)
      val search_by_name : Module.t_module list -> Str.regexp -> search_result

      (** A function to search all the values in a list of modules. *)
      val values : Module.t_module list -> Value.t_value list

      (** A function to search all the extensions in a list of modules. *)
      val extensions : Module.t_module list -> Extension.t_extension_constructor list

      (** A function to search all the exceptions in a list of modules. *)
      val exceptions : Module.t_module list -> Exception.t_exception list

      (** A function to search all the types in a list of modules. *)
      val types : Module.t_module list -> Type.t_type list

      (** A function to search all the class attributes in a list of modules. *)
      val attributes : Module.t_module list -> Value.t_attribute list

      (** A function to search all the class methods in a list of modules. *)
      val methods : Module.t_module list -> Value.t_method list

      (** A function to search all the classes in a list of modules. *)
      val classes : Module.t_module list -> Class.t_class list

      (** A function to search all the class types in a list of modules. *)
      val class_types : Module.t_module list -> Class.t_class_type list

      (** A function to search all the modules in a list of modules. *)
      val modules : Module.t_module list -> Module.t_module list

      (** A function to search all the module types in a list of modules. *)
      val module_types : Module.t_module list -> Module.t_module_type list

    end

(** Scanning of collected information *)
module Scan :
  sig
    class scanner :
      object
      (** Scan of 'leaf elements'. *)

        method scan_value : Value.t_value -> unit

        method scan_type_pre : Type.t_type -> bool
        method scan_type_const : Type.t_type -> Type.variant_constructor -> unit
        method scan_type_recfield : Type.t_type -> Type.record_field -> unit
        method scan_type : Type.t_type -> unit
        method scan_extension_constructor : Extension.t_extension_constructor -> unit
        method scan_exception : Exception.t_exception -> unit
        method scan_attribute : Value.t_attribute -> unit
        method scan_method : Value.t_method -> unit
        method scan_included_module : Module.included_module -> unit

      (** Scan of a type extension *)

        (** Overide this method to perform controls on the extension's type,
            private and info. This method is called before scanning the
            extension's constructors.
            @return true if the extension's constructors must be scanned.*)
        method scan_type_extension_pre : Extension.t_type_extension -> bool

       (** This method scans the constructors of the given type extension. *)
        method scan_type_extension_constructors : Extension.t_type_extension -> unit

        (** Scan of a type extension. Should not be overridden. It calls [scan_type_extension_pre]
            and if [scan_type_extension_pre] returns [true], then it calls scan_type_extension_constructors.*)
        method scan_type_extension : Extension.t_type_extension -> unit

      (** Scan of a class. *)

        (** Scan of a comment inside a class. *)
        method scan_class_comment : text -> unit

       (** Override this method to perform controls on the class comment
          and params. This method is called before scanning the class elements.
          @return true if the class elements must be scanned.*)
        method scan_class_pre : Class.t_class -> bool

       (** This method scan the elements of the given class. *)
        method scan_class_elements : Class.t_class -> unit

       (** Scan of a class. Should not be overridden. It calls [scan_class_pre]
          and if [scan_class_pre] returns [true], then it calls scan_class_elements.*)
        method scan_class : Class.t_class -> unit

      (** Scan of a class type. *)

        (** Scan of a comment inside a class type. *)
        method scan_class_type_comment : text -> unit

        (** Override this method to perform controls on the class type comment
           and form. This method is called before scanning the class type elements.
           @return true if the class type elements must be scanned.*)
        method scan_class_type_pre : Class.t_class_type -> bool

        (** This method scan the elements of the given class type. *)
        method scan_class_type_elements : Class.t_class_type -> unit

        (** Scan of a class type. Should not be overridden. It calls [scan_class_type_pre]
           and if [scan_class_type_pre] returns [true], then it calls scan_class_type_elements.*)
        method scan_class_type : Class.t_class_type -> unit

      (** Scan of modules. *)

        (** Scan of a comment inside a module. *)
        method scan_module_comment : text -> unit

        (** Override this method to perform controls on the module comment
           and form. This method is called before scanning the module elements.
           @return true if the module elements must be scanned.*)
        method scan_module_pre : Module.t_module -> bool

        (** This method scan the elements of the given module. *)
        method scan_module_elements : Module.t_module -> unit

       (** Scan of a module. Should not be overridden. It calls [scan_module_pre]
          and if [scan_module_pre] returns [true], then it calls scan_module_elements.*)
        method scan_module : Module.t_module -> unit

      (** Scan of module types. *)

        (** Scan of a comment inside a module type. *)
        method scan_module_type_comment : text -> unit

        (** Override this method to perform controls on the module type comment
           and form. This method is called before scanning the module type elements.
           @return true if the module type elements must be scanned. *)
        method scan_module_type_pre : Module.t_module_type -> bool

        (** This method scan the elements of the given module type. *)
        method scan_module_type_elements : Module.t_module_type -> unit

        (** Scan of a module type. Should not be overridden. It calls [scan_module_type_pre]
           and if [scan_module_type_pre] returns [true], then it calls scan_module_type_elements.*)
        method scan_module_type : Module.t_module_type -> unit

      (** Main scanning method. *)

        (** Scan a list of modules. *)
        method scan_module_list : Module.t_module list -> unit
      end
  end

(** Computation of dependencies. *)
module Dep :
  sig
    (** Modify the modules depencies of the given list of modules,
       to get the minimum transitivity kernel. *)
    val kernel_deps_of_modules : Module.t_module list -> unit

    (** Return the list of dependencies between the given types,
       in the form of a list [(type name, names of types it depends on)].
       @param kernel indicates if we must keep only the transitivity kernel
       of the dependencies. Default is [false].
    *)
    val deps_of_types : ?kernel: bool -> Type.t_type list -> (Type.t_type * (Name.t list)) list
  end

(** {2 Some global variables} *)

module Global :
  sig
    val errors : int ref
    val warn_error : bool ref

    (** The file used by the generators outputting only one file. *)
    val out_file : string ref

    (** Verbose mode or not. *)
    val verbose : bool ref

    (** The directory where files have to be generated. *)
    val target_dir : string ref

    (** The optional title to use in the generated documentation. *)
    val title : string option ref

    (** The optional file whose content can be used as intro text. *)
    val intro_file : string option ref

    (** The flag which indicates if we must generate a table of contents. *)
    val with_toc : bool ref

    (** The flag which indicates if we must generate an index. *)
    val with_index : bool ref

    (** The flag which indicates if we must generate a header.*)
    val with_header : bool ref

    (** The flag which indicates if we must generate a trailer.*)
    val with_trailer : bool ref
end

(** Analysis of the given source files.
   @param init is the list of modules already known from a previous analysis.
   @return the list of analysed top modules. *)
val analyse_files :
    ?merge_options:Odoc_types.merge_option list ->
      ?include_dirs:string list ->
        ?labels:bool ->
          ?sort_modules:bool ->
            ?no_stop:bool ->
              ?init: Odoc_module.t_module list ->
                Odoc_global.source_file list ->
                  Module.t_module list

(** Dump of a list of modules into a file.
   @raise Failure if an error occurs.*)
val dump_modules : string -> Odoc_module.t_module list -> unit

(** Load of a list of modules from a file.
   @raise Failure if an error occurs.*)
val load_modules : string -> Odoc_module.t_module list
