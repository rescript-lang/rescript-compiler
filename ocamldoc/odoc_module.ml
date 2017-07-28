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

(** Representation and manipulation of modules and module types. *)

let print_DEBUG s = print_string s ; print_newline ()

module Name = Odoc_name

(** To keep the order of elements in a module. *)
type module_element =
    Element_module of t_module
  | Element_module_type of t_module_type
  | Element_included_module of included_module
  | Element_class of Odoc_class.t_class
  | Element_class_type of Odoc_class.t_class_type
  | Element_value of Odoc_value.t_value
  | Element_type_extension of Odoc_extension.t_type_extension
  | Element_exception of Odoc_exception.t_exception
  | Element_type of Odoc_type.t_type
  | Element_module_comment of Odoc_types.text

(** Used where we can reference t_module or t_module_type *)
and mmt =
  | Mod of t_module
  | Modtype of t_module_type

and included_module = {
    im_name : Name.t ; (** the name of the included module *)
    mutable im_module : mmt option ; (** the included module or module type *)
    mutable im_info : Odoc_types.info option ; (** comment associated to the includ directive *)
  }

and module_alias = {
    ma_name : Name.t ;
    mutable ma_module : mmt option ; (** the real module or module type if we could associate it *)
  }

and module_parameter = {
    mp_name : string ; (** the name *)
    mp_type : Types.module_type option ; (** the type *)
    mp_type_code : string ; (** the original code *)
    mp_kind : module_type_kind ; (** the way the parameter was built *)
  }

(** Different kinds of module. *)
and module_kind =
  | Module_struct of module_element list
  | Module_alias of module_alias (** complete name and corresponding module if we found it *)
  | Module_functor of module_parameter * module_kind
  | Module_apply of module_kind * module_kind
  | Module_with of module_type_kind * string
  | Module_constraint of module_kind * module_type_kind
  | Module_typeof of string (** by now only the code of the module expression *)
  | Module_unpack of string * module_type_alias (** code of the expression and module type alias *)

(** Representation of a module. *)
and t_module = {
    m_name : Name.t ;
    mutable m_type : Types.module_type ;
    mutable m_info : Odoc_types.info option ;
    m_is_interface : bool ; (** true for modules read from interface files *)
    m_file : string ; (** the file the module is defined in. *)
    mutable m_kind : module_kind ;
    mutable m_loc : Odoc_types.location ;
    mutable m_top_deps : Name.t list ; (** The toplevels module names this module depends on. *)
    mutable m_code : string option ; (** The whole code of the module *)
    mutable m_code_intf : string option ; (** The whole code of the interface of the module *)
    m_text_only : bool ; (** [true] if the module comes from a text file *)
  }

and module_type_alias = {
    mta_name : Name.t ;
    mutable mta_module : t_module_type option ; (** the real module type if we could associate it *)
  }

(** Different kinds of module type. *)
and module_type_kind =
  | Module_type_struct of module_element list
  | Module_type_functor of module_parameter * module_type_kind
  | Module_type_alias of module_type_alias (** complete name and corresponding module type if we found it *)
  | Module_type_with of module_type_kind * string (** the module type kind and the code of the with constraint *)
  | Module_type_typeof of string (** by now only the code of the module expression *)

(** Representation of a module type. *)
and t_module_type = {
    mt_name : Name.t ;
    mutable mt_info : Odoc_types.info option ;
    mutable mt_type : Types.module_type option ; (** [None] = abstract module type *)
    mt_is_interface : bool ; (** true for modules read from interface files *)
    mt_file : string ; (** the file the module type is defined in. *)
    mutable mt_kind : module_type_kind option ; (** [None] = abstract module type if mt_type = None ;
                                           Always [None] when the module type was extracted from the implementation file. *)
    mutable mt_loc : Odoc_types.location ;
  }


(** {2 Functions} *)

(** Returns the list of values from a list of module_element. *)
let values l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_value v -> acc @ [v]
      | _ -> acc
    )
    []
    l

(** Returns the list of types from a list of module_element. *)
let types l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_type t -> acc @ [t]
      | _ -> acc
    )
    []
    l

(** Returns the list of type extensions from a list of module_element. *)
let type_extensions l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_type_extension x -> acc @ [x]
      | _ -> acc
    )
    []
    l

(** Returns the list of exceptions from a list of module_element. *)
let exceptions l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_exception e -> acc @ [e]
      | _ -> acc
    )
    []
    l

(** Returns the list of classes from a list of module_element. *)
let classes l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_class c -> acc @ [c]
      | _ -> acc
    )
    []
    l

(** Returns the list of class types from a list of module_element. *)
let class_types l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_class_type ct -> acc @ [ct]
      | _ -> acc
    )
    []
    l

(** Returns the list of modules from a list of module_element. *)
let modules l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_module m -> acc @ [m]
      | _ -> acc
    )
    []
    l

(** Returns the list of module types from a list of module_element. *)
let mod_types l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_module_type mt -> acc @ [mt]
      | _ -> acc
    )
    []
    l

(** Returns the list of module comment from a list of module_element. *)
let comments l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_module_comment t -> acc @ [t]
      | _ -> acc
    )
    []
    l

(** Returns the list of included modules from a list of module_element. *)
let included_modules l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_included_module m -> acc @ [m]
      | _ -> acc
    )
    []
    l

(** Returns the list of elements of a module.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let rec module_elements ?(trans=true) m =
  let rec iter_kind = function
      Module_struct l ->
        print_DEBUG "Odoc_module.module_element: Module_struct";
        l
    | Module_alias ma ->
        print_DEBUG "Odoc_module.module_element: Module_alias";
        if trans then
          match ma.ma_module with
            None -> []
          | Some (Mod m) -> module_elements m
          | Some (Modtype mt) -> module_type_elements mt
        else
          []
    | Module_functor (_, k)
    | Module_apply (k, _) ->
        print_DEBUG "Odoc_module.module_element: Module_functor ou Module_apply";
        iter_kind k
    | Module_with (tk,_) ->
        print_DEBUG "Odoc_module.module_element: Module_with";
        module_type_elements ~trans: trans
          { mt_name = "" ; mt_info = None ; mt_type = None ;
            mt_is_interface = false ; mt_file = "" ; mt_kind = Some tk ;
            mt_loc = Odoc_types.dummy_loc ;
          }
    | Module_constraint (k, tk) ->
        print_DEBUG "Odoc_module.module_element: Module_constraint";
      (* A VOIR : utiliser k ou tk ? *)
        module_elements ~trans: trans
          { m_name = "" ;
            m_info = None ;
            m_type = Types.Mty_signature [] ;
            m_is_interface = false ; m_file = "" ; m_kind = k ;
            m_loc = Odoc_types.dummy_loc ;
            m_top_deps = [] ;
            m_code = None ;
            m_code_intf = None ;
            m_text_only = false ;
          }
    | Module_typeof _ -> []
    | Module_unpack _ -> []
(*
   module_type_elements ~trans: trans
   { mt_name = "" ; mt_info = None ; mt_type = None ;
   mt_is_interface = false ; mt_file = "" ; mt_kind = Some tk ;
   mt_loc = Odoc_types.dummy_loc }
*)
  in
  iter_kind m.m_kind

(** Returns the list of elements of a module type.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)
and module_type_elements ?(trans=true) mt =
  let rec iter_kind = function
    | None -> []
    | Some (Module_type_struct l) -> l
    | Some (Module_type_functor (_, k)) -> iter_kind (Some k)
    | Some (Module_type_with (k, _)) ->
        if trans then
          iter_kind (Some k)
        else
          []
    | Some (Module_type_alias mta) ->
        if trans then
          match mta.mta_module with
            None -> []
          | Some mt -> module_type_elements mt
        else
          []
  | Some (Module_type_typeof _) -> []
  in
  iter_kind mt.mt_kind

(** Returns the list of values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_values ?(trans=true) m = values (module_elements ~trans m)

(** Returns the list of functional values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_functions ?(trans=true) m =
  List.filter
    (fun v -> Odoc_value.is_function v)
    (values (module_elements ~trans m))

(** Returns the list of non-functional values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_simple_values ?(trans=true) m =
    List.filter
    (fun v -> not (Odoc_value.is_function v))
    (values (module_elements ~trans m))

(** Returns the list of types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_types ?(trans=true) m = types (module_elements ~trans m)

(** Returns the list of type extensions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_extensions ?(trans=true) m = type_extensions (module_elements ~trans m)

(** Returns the list of exceptions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_exceptions ?(trans=true) m = exceptions (module_elements ~trans m)

(** Returns the list of classes of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_classes ?(trans=true) m = classes (module_elements ~trans m)

(** Returns the list of class types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_class_types ?(trans=true) m = class_types (module_elements ~trans m)

(** Returns the list of modules of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_modules ?(trans=true) m = modules (module_elements ~trans m)

(** Returns the list of module types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_module_types ?(trans=true) m = mod_types (module_elements ~trans m)

(** Returns the list of included module of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_included_modules ?(trans=true) m = included_modules (module_elements ~trans m)

(** Returns the list of comments of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_comments ?(trans=true) m = comments (module_elements ~trans m)

(** Access to the parameters, for a functor type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let rec module_type_parameters ?(trans=true) mt =
  let rec iter k =
    match k with
      Some (Module_type_functor (p, k2)) ->
        let param =
           (* we create the couple (parameter, description opt), using
              the description of the parameter if we can find it in the comment.*)
          match mt.mt_info with
            None -> (p, None)
          | Some i ->
              try
                let d = List.assoc p.mp_name i.Odoc_types.i_params in
                (p, Some d)
              with
                Not_found ->
                  (p, None)
        in
        param :: (iter (Some k2))
    | Some (Module_type_alias mta) ->
        if trans then
          match mta.mta_module with
            None -> []
          | Some mt2 -> module_type_parameters ~trans mt2
        else
          []
    | Some (Module_type_with (k, _)) ->
        if trans then
          iter (Some k)
        else
          []
    | Some (Module_type_struct _) ->
        []
    | Some (Module_type_typeof _) -> []
      | None ->
        []
  in
  iter mt.mt_kind

(** Access to the parameters, for a functor.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)
and module_parameters ?(trans=true) m =
  let rec iter = function
      Module_functor (p, k) ->
        let param =
         (* we create the couple (parameter, description opt), using
            the description of the parameter if we can find it in the comment.*)
          match m.m_info with
            None ->(p, None)
          | Some i ->
              try
                let d = List.assoc p.mp_name i.Odoc_types.i_params in
                (p, Some d)
              with
                Not_found ->
                  (p, None)
        in
        param :: (iter k)

    | Module_alias ma ->
        if trans then
          match ma.ma_module with
            None -> []
          | Some (Mod m) -> module_parameters ~trans m
          | Some (Modtype mt) -> module_type_parameters ~trans mt
        else
          []
    | Module_constraint (k, tk) ->
        module_type_parameters ~trans: trans
          { mt_name = "" ; mt_info = None ; mt_type = None ;
            mt_is_interface = false ; mt_file = "" ; mt_kind = Some tk ;
            mt_loc = Odoc_types.dummy_loc }
    | Module_struct _
    | Module_apply _
    | Module_with _
    | Module_typeof _
    | Module_unpack _ -> []
  in
  iter m.m_kind

(** access to all submodules and sudmobules of submodules ... of the given module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let rec module_all_submodules ?(trans=true) m =
  let l = module_modules ~trans m in
  List.fold_left
    (fun acc -> fun m -> acc @ (module_all_submodules ~trans m))
    l
    l

(** The module type is a functor if is defined as a functor or if it is an alias for a functor. *)
let rec module_type_is_functor mt =
  let rec iter k =
    match k with
      Some (Module_type_functor _) -> true
    | Some (Module_type_alias mta) ->
        (
         match mta.mta_module with
           None -> false
         | Some mtyp -> module_type_is_functor mtyp
        )
    | Some (Module_type_with (k, _)) ->
        iter (Some k)
    | Some (Module_type_struct _)
    | Some (Module_type_typeof _)
    | None -> false
  in
  iter mt.mt_kind

(** The module is a functor if is defined as a functor or if it is an alias for a functor. *)
let module_is_functor m =
  let rec iter = function
      Module_functor _ -> true
    | Module_alias ma ->
        (
         match ma.ma_module with
           None -> false
         | Some (Mod mo) -> iter mo.m_kind
         | Some (Modtype mt) -> module_type_is_functor mt
        )
    | Module_constraint (k, _) ->
        iter k
    | _ -> false
  in
  iter m.m_kind

(** Returns the list of values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_values ?(trans=true) m = values (module_type_elements ~trans m)

(** Returns the list of types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_types ?(trans=true) m = types (module_type_elements ~trans m)

(** Returns the list of type extensions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_type_extensions ?(trans=true) m = type_extensions (module_type_elements ~trans m)

(** Returns the list of exceptions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_exceptions ?(trans=true) m = exceptions (module_type_elements ~trans m)

(** Returns the list of classes of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_classes ?(trans=true) m = classes (module_type_elements ~trans m)

(** Returns the list of class types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_class_types ?(trans=true) m = class_types (module_type_elements ~trans m)

(** Returns the list of modules of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_modules ?(trans=true)  m = modules (module_type_elements ~trans m)

(** Returns the list of module types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_module_types ?(trans=true) m = mod_types (module_type_elements ~trans m)

(** Returns the list of included module of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_included_modules ?(trans=true) m = included_modules (module_type_elements ~trans m)

(** Returns the list of comments of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_comments ?(trans=true) m = comments (module_type_elements ~trans m)

(** Returns the list of functional values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_functions ?(trans=true) mt =
  List.filter
    (fun v -> Odoc_value.is_function v)
    (values (module_type_elements ~trans mt))

(** Returns the list of non-functional values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_simple_values ?(trans=true) mt =
    List.filter
    (fun v -> not (Odoc_value.is_function v))
    (values (module_type_elements ~trans mt))

(** {2 Functions for modules and module types} *)

(** The list of classes defined in this module and all its modules, functors, ....
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let rec module_all_classes ?(trans=true) m =
  List.fold_left
    (fun acc -> fun m -> acc @ (module_all_classes ~trans m))
    (
       List.fold_left
       (fun acc -> fun mtyp -> acc @ (module_type_all_classes ~trans mtyp))
       (module_classes ~trans m)
       (module_module_types ~trans m)
    )
    (module_modules ~trans m)

(** The list of classes defined in this module type and all its modules, functors, ....
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
and module_type_all_classes ?(trans=true) mt =
  List.fold_left
    (fun acc -> fun m -> acc @ (module_all_classes ~trans m))
    (
     List.fold_left
       (fun acc -> fun mtyp -> acc @ (module_type_all_classes ~trans mtyp))
       (module_type_classes ~trans mt)
       (module_type_module_types ~trans mt)
    )
    (module_type_modules ~trans mt)
