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

(** Representation and manipulation of classes and class types.*)

module Name = Odoc_name

(** To keep the order of elements in a class *)
type class_element =
    Class_attribute of Odoc_value.t_attribute
  | Class_method of Odoc_value.t_method
  | Class_comment of Odoc_types.text

(** Used when we can reference t_class or t_class_type. *)
type cct =
    Cl of t_class
  | Cltype of t_class_type * Types.type_expr list (** class type and type parameters *)

and inherited_class = {
    ic_name : Name.t ; (** Complete name of the inherited class *)
    mutable ic_class : cct option ; (** The associated t_class or t_class_type *)
    ic_text : Odoc_types.text option ; (** The inheritance comment, if any *)
  }

and class_apply = {
    capp_name : Name.t ; (** The complete name of the applied class *)
    mutable capp_class : t_class option;  (** The associated t_class if we found it *)
    capp_params : Types.type_expr list; (** The type of expressions the class is applied to *)
    capp_params_code : string list ; (** The code of these expressions *)
  }

and class_constr = {
    cco_name : Name.t ; (** The complete name of the applied class *)
    mutable cco_class : cct option;  (** The associated class ot class type if we found it *)
    cco_type_parameters : Types.type_expr list; (** The type parameters of the class, if needed *)
  }


and class_kind =
    Class_structure of inherited_class list * class_element list
        (** an explicit class structure, used in implementation and interface *)
  | Class_apply of class_apply (** application/alias of a class, used in implementation only *)
  | Class_constr of class_constr (** a class used to give the type of the defined class,
                                    instead of a structure, used in interface only.
                                    For example, it will be used with the name "M1.M2....tutu"
                                    when the class to is defined like this :
                                    class toto : int -> tutu *)
  | Class_constraint of class_kind * class_type_kind
        (** A class definition with a constraint. *)

(** Representation of a class. *)
and t_class = {
    cl_name : Name.t ; (** Name of the class *)
    mutable cl_info : Odoc_types.info option ; (** The optional associated user information *)
    cl_type : Types.class_type ;
    cl_type_parameters : Types.type_expr list ; (** Type parameters *)
    cl_virtual : bool ; (** true = virtual *)
    mutable cl_kind : class_kind ;
    mutable cl_parameters : Odoc_parameter.parameter list ;
    mutable cl_loc : Odoc_types.location ;
  }

and class_type_alias = {
    cta_name : Name.t ;
    mutable cta_class : cct option ; (** we can have a t_class or a t_class_type *)
    cta_type_parameters : Types.type_expr list ; (** the type parameters *)
  }

and class_type_kind =
    Class_signature of inherited_class list * class_element list
  | Class_type of class_type_alias (** a class type eventually applied to type args *)

(** Representation of a class type. *)
and t_class_type = {
    clt_name : Name.t ;
    mutable clt_info : Odoc_types.info option ; (** The optional associated user information *)
    clt_type : Types.class_type ;
    clt_type_parameters : Types.type_expr list ; (** type parameters *)
    clt_virtual : bool ; (** true = virtual *)
    mutable clt_kind : class_type_kind ;
    mutable clt_loc : Odoc_types.location ;
  }


(** {2 Functions} *)

(** Returns the text associated to the given parameter label
   in the given class, or None. *)
let class_parameter_text_by_name cl label =
  match cl.cl_info with
    None -> None
  | Some i ->
      try
        let t = List.assoc label i.Odoc_types.i_params in
        Some t
      with
        Not_found ->
          None

(** Returns the list of elements of a t_class. *)
let rec class_elements ?(trans=true) cl =
  let rec iter_kind k =
    match k with
      Class_structure (_, elements) -> elements
    | Class_constraint (c_kind, ct_kind) ->
        iter_kind c_kind
      (* A VOIR : utiliser le c_kind ou le ct_kind ?
         Pour l'instant, comme le ct_kind n'est pas analyse,
         on cherche dans le c_kind
         class_type_elements ~trans: trans
         { clt_name = "" ; clt_info = None ;
          clt_type_parameters = [] ;
         clt_virtual = false ;
         clt_kind = ct_kind }
      *)
    | Class_apply capp ->
        (
         match capp.capp_class with
           Some c when trans -> class_elements ~trans: trans c
         | _ -> []
        )
    | Class_constr cco ->
        (
         match cco.cco_class with
           Some (Cl c) when trans -> class_elements ~trans: trans c
         | Some (Cltype (ct,_)) when trans -> class_type_elements ~trans: trans ct
         | _ -> []
        )
  in
  iter_kind cl.cl_kind

(** Returns the list of elements of a t_class_type. *)
and class_type_elements ?(trans=true) clt =
  match clt.clt_kind with
    Class_signature (_, elements) -> elements
  | Class_type { cta_class = Some (Cltype (ct, _)) } when trans ->
      class_type_elements ~trans ct
  | Class_type { cta_class = Some (Cl c) } when trans ->
      class_elements ~trans c
  | Class_type _ ->
      []

(** Returns the attributes of a t_class. *)
let class_attributes ?(trans=true) cl =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Class_attribute a ->
          acc @ [ a ]
      | _ ->
          acc
    )
    []
    (class_elements ~trans cl)

(** Returns the methods of a t_class. *)
let class_methods ?(trans=true) cl =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Class_method m ->
          acc @ [ m ]
      | _ ->
          acc
    )
    []
    (class_elements ~trans cl)

(** Returns the comments in a t_class. *)
let class_comments ?(trans=true) cl =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Class_comment t ->
          acc @ [ t ]
      | _ ->
          acc
    )
    []
    (class_elements ~trans cl)


(** Update the parameters text of a t_class, according to the cl_info field. *)
let class_update_parameters_text cl =
  let f p =
    Odoc_parameter.update_parameter_text (class_parameter_text_by_name cl) p
  in
  List.iter f cl.cl_parameters

(** Returns the attributes of a t_class_type. *)
let class_type_attributes ?(trans=true) clt =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Class_attribute a ->
          acc @ [ a ]
      | _ ->
          acc
    )
    []
    (class_type_elements ~trans clt)

(** Returns the methods of a t_class_type. *)
let class_type_methods ?(trans=true) clt =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Class_method m ->
          acc @ [ m ]
      | _ ->
          acc
    )
    []
    (class_type_elements ~trans clt)

(** Returns the comments in a t_class_type. *)
let class_type_comments ?(trans=true) clt =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Class_comment m ->
          acc @ [ m ]
      | _ ->
          acc
    )
    []
    (class_type_elements ~trans clt)

(** Returns the text associated to the given parameter label
   in the given class type, or None. *)
let class_type_parameter_text_by_name clt label =
  match clt.clt_info with
    None -> None
  | Some i ->
      try
        let t = List.assoc label i.Odoc_types.i_params in
        Some t
      with
        Not_found ->
          None
