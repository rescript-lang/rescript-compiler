module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result
module Ast_404
= struct
#1 "ast_404.ml"
# 1 "src/ast_404.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*            Jérémie Dimino and Leo White, Jane Street Europe            *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                         Alain Frisch, LexiFi                           *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Location = Location
module Longident = Longident

module Asttypes = struct
  (** Auxiliary AST types used by parsetree and typedtree. *)

  type constant (*IF_CURRENT = Asttypes.constant *) =
      Const_int of int
    | Const_char of char
    | Const_string of string * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint

  type rec_flag (*IF_CURRENT = Asttypes.rec_flag *) = Nonrecursive | Recursive

  type direction_flag (*IF_CURRENT = Asttypes.direction_flag *) = Upto | Downto

  (* Order matters, used in polymorphic comparison *)
  type private_flag (*IF_CURRENT = Asttypes.private_flag *) = Private | Public

  type mutable_flag (*IF_CURRENT = Asttypes.mutable_flag *) = Immutable | Mutable

  type virtual_flag (*IF_CURRENT = Asttypes.virtual_flag *) = Virtual | Concrete

  type override_flag (*IF_CURRENT = Asttypes.override_flag *) = Override | Fresh

  type closed_flag (*IF_CURRENT = Asttypes.closed_flag *) = Closed | Open

  type label = string

  type arg_label (*IF_CURRENT = Asttypes.arg_label *) =
      Nolabel
    | Labelled of string (*  label:T -> ... *)
    | Optional of string (* ?label:T -> ... *)

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }


  type variance (*IF_CURRENT = Asttypes.variance *) =
    | Covariant
    | Contravariant
    | Invariant
end

module Parsetree = struct
  (** Abstract syntax tree produced by parsing *)

  open Asttypes

  type constant (*IF_CURRENT = Parsetree.constant *) =
      Pconst_integer of string * char option
    (* 3 3l 3L 3n

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
    *)
    | Pconst_char of char
    (* 'c' *)
    | Pconst_string of string * string option
    (* "constant"
       {delim|other constant|delim}
    *)
    | Pconst_float of string * char option
    (* 3.4 2e5 1.4e-4

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes are rejected by the typechecker.
    *)

  (** {2 Extension points} *)

  type attribute = string loc * payload
         (* [@id ARG]
            [@@id ARG]

            Metadata containers passed around within the AST.
            The compiler ignores unknown attributes.
         *)

  and extension = string loc * payload
        (* [%id ARG]
           [%%id ARG]

           Sub-language placeholder -- rejected by the typechecker.
        *)

  and attributes = attribute list

  and payload (*IF_CURRENT = Parsetree.payload *) =
    | PStr of structure
    | PSig of signature (* : SIG *)
    | PTyp of core_type  (* : T *)
    | PPat of pattern * expression option  (* ? P  or  ? P when E *)

  (** {2 Core language} *)

  (* Type expressions *)

  and core_type (*IF_CURRENT = Parsetree.core_type *) =
      {
       ptyp_desc: core_type_desc;
       ptyp_loc: Location.t;
       ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and core_type_desc (*IF_CURRENT = Parsetree.core_type_desc *) =
    | Ptyp_any
          (*  _ *)
    | Ptyp_var of string
          (* 'a *)
    | Ptyp_arrow of arg_label * core_type * core_type
          (* T1 -> T2       Simple
             ~l:T1 -> T2    Labelled
             ?l:T1 -> T2    Otional
           *)
    | Ptyp_tuple of core_type list
          (* T1 * ... * Tn

             Invariant: n >= 2
          *)
    | Ptyp_constr of Longident.t loc * core_type list
          (* tconstr
             T tconstr
             (T1, ..., Tn) tconstr
           *)
    | Ptyp_object of (string * attributes * core_type) list * closed_flag
          (* < l1:T1; ...; ln:Tn >     (flag = Closed)
             < l1:T1; ...; ln:Tn; .. > (flag = Open)
           *)
    | Ptyp_class of Longident.t loc * core_type list
          (* #tconstr
             T #tconstr
             (T1, ..., Tn) #tconstr
           *)
    | Ptyp_alias of core_type * string
          (* T as 'a *)
    | Ptyp_variant of row_field list * closed_flag * label list option
          (* [ `A|`B ]         (flag = Closed; labels = None)
             [> `A|`B ]        (flag = Open;   labels = None)
             [< `A|`B ]        (flag = Closed; labels = Some [])
             [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
           *)
    | Ptyp_poly of string list * core_type
          (* 'a1 ... 'an. T

             Can only appear in the following context:

             - As the core_type of a Ppat_constraint node corresponding
               to a constraint on a let-binding: let x : 'a1 ... 'an. T
               = e ...

             - Under Cfk_virtual for methods (not values).

             - As the core_type of a Pctf_method node.

             - As the core_type of a Pexp_poly node.

             - As the pld_type field of a label_declaration.

             - As a core_type of a Ptyp_object node.
           *)

    | Ptyp_package of package_type
          (* (module S) *)
    | Ptyp_extension of extension
          (* [%id] *)

  and package_type = Longident.t loc * (Longident.t loc * core_type) list
        (*
          (module S)
          (module S with type t1 = T1 and ... and tn = Tn)
         *)

  and row_field (*IF_CURRENT = Parsetree.row_field *) =
    | Rtag of label * attributes * bool * core_type list
          (* [`A]                   ( true,  [] )
             [`A of T]              ( false, [T] )
             [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
             [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

            - The 2nd field is true if the tag contains a
              constant (empty) constructor.
            - '&' occurs when several types are used for the same constructor
              (see 4.2 in the manual)

            - TODO: switch to a record representation, and keep location
          *)
    | Rinherit of core_type
          (* [ T ] *)

  (* Patterns *)

  and pattern (*IF_CURRENT = Parsetree.pattern *) =
      {
       ppat_desc: pattern_desc;
       ppat_loc: Location.t;
       ppat_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and pattern_desc (*IF_CURRENT = Parsetree.pattern_desc *) =
    | Ppat_any
          (* _ *)
    | Ppat_var of string loc
          (* x *)
    | Ppat_alias of pattern * string loc
          (* P as 'a *)
    | Ppat_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Ppat_interval of constant * constant
          (* 'a'..'z'

             Other forms of interval are recognized by the parser
             but rejected by the type-checker. *)
    | Ppat_tuple of pattern list
          (* (P1, ..., Pn)

             Invariant: n >= 2
          *)
    | Ppat_construct of Longident.t loc * pattern option
          (* C                None
             C P              Some P
             C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
           *)
    | Ppat_variant of label * pattern option
          (* `A             (None)
             `A P           (Some P)
           *)
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
          (* { l1=P1; ...; ln=Pn }     (flag = Closed)
             { l1=P1; ...; ln=Pn; _}   (flag = Open)

             Invariant: n > 0
           *)
    | Ppat_array of pattern list
          (* [| P1; ...; Pn |] *)
    | Ppat_or of pattern * pattern
          (* P1 | P2 *)
    | Ppat_constraint of pattern * core_type
          (* (P : T) *)
    | Ppat_type of Longident.t loc
          (* #tconst *)
    | Ppat_lazy of pattern
          (* lazy P *)
    | Ppat_unpack of string loc
          (* (module P)
             Note: (module P : S) is represented as
             Ppat_constraint(Ppat_unpack, Ptyp_package)
           *)
    | Ppat_exception of pattern
          (* exception P *)
    | Ppat_extension of extension
          (* [%id] *)
    | Ppat_open of Longident.t loc * pattern

  (* Value expressions *)

  and expression (*IF_CURRENT = Parsetree.expression *) =
      {
       pexp_desc: expression_desc;
       pexp_loc: Location.t;
       pexp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and expression_desc (*IF_CURRENT = Parsetree.expression_desc *) =
    | Pexp_ident of Longident.t loc
          (* x
             M.x
           *)
    | Pexp_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Pexp_let of rec_flag * value_binding list * expression
          (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
           *)
    | Pexp_function of case list
          (* function P1 -> E1 | ... | Pn -> En *)
    | Pexp_fun of arg_label * expression option * pattern * expression
          (* fun P -> E1                          (Simple, None)
             fun ~l:P -> E1                       (Labelled l, None)
             fun ?l:P -> E1                       (Optional l, None)
             fun ?l:(P = E0) -> E1                (Optional l, Some E0)

             Notes:
             - If E0 is provided, only Optional is allowed.
             - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
             - "let f P = E" is represented using Pexp_fun.
           *)
    | Pexp_apply of expression * (arg_label * expression) list
          (* E0 ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pexp_match of expression * case list
          (* match E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_try of expression * case list
          (* try E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_tuple of expression list
          (* (E1, ..., En)

             Invariant: n >= 2
          *)
    | Pexp_construct of Longident.t loc * expression option
          (* C                None
             C E              Some E
             C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
          *)
    | Pexp_variant of label * expression option
          (* `A             (None)
             `A E           (Some E)
           *)
    | Pexp_record of (Longident.t loc * expression) list * expression option
          (* { l1=P1; ...; ln=Pn }     (None)
             { E0 with l1=P1; ...; ln=Pn }   (Some E0)

             Invariant: n > 0
           *)
    | Pexp_field of expression * Longident.t loc
          (* E.l *)
    | Pexp_setfield of expression * Longident.t loc * expression
          (* E1.l <- E2 *)
    | Pexp_array of expression list
          (* [| E1; ...; En |] *)
    | Pexp_ifthenelse of expression * expression * expression option
          (* if E1 then E2 else E3 *)
    | Pexp_sequence of expression * expression
          (* E1; E2 *)
    | Pexp_while of expression * expression
          (* while E1 do E2 done *)
    | Pexp_for of
        pattern *  expression * expression * direction_flag * expression
          (* for i = E1 to E2 do E3 done      (flag = Upto)
             for i = E1 downto E2 do E3 done  (flag = Downto)
           *)
    | Pexp_constraint of expression * core_type
          (* (E : T) *)
    | Pexp_coerce of expression * core_type option * core_type
          (* (E :> T)        (None, T)
             (E : T0 :> T)   (Some T0, T)
           *)
    | Pexp_send of expression * string
          (*  E # m *)
    | Pexp_new of Longident.t loc
          (* new M.c *)
    | Pexp_setinstvar of string loc * expression
          (* x <- 2 *)
    | Pexp_override of (string loc * expression) list
          (* {< x1 = E1; ...; Xn = En >} *)
    | Pexp_letmodule of string loc * module_expr * expression
          (* let module M = ME in E *)
    | Pexp_letexception of extension_constructor * expression
          (* let exception C in E *)
    | Pexp_assert of expression
          (* assert E
             Note: "assert false" is treated in a special way by the
             type-checker. *)
    | Pexp_lazy of expression
          (* lazy E *)
    | Pexp_poly of expression * core_type option
          (* Used for method bodies.

             Can only be used as the expression under Cfk_concrete
             for methods (not values). *)
    | Pexp_object of class_structure
          (* object ... end *)
    | Pexp_newtype of string * expression
          (* fun (type t) -> E *)
    | Pexp_pack of module_expr
          (* (module ME)

             (module ME : S) is represented as
             Pexp_constraint(Pexp_pack, Ptyp_package S) *)
    | Pexp_open of override_flag * Longident.t loc * expression
          (* let open M in E
             let! open M in E
          *)
    | Pexp_extension of extension
          (* [%id] *)
    | Pexp_unreachable
          (* . *)

  and case (*IF_CURRENT = Parsetree.case *) =   (* (P -> E) or (P when E0 -> E) *)
      {
       pc_lhs: pattern;
       pc_guard: expression option;
       pc_rhs: expression;
      }

  (* Value descriptions *)

  and value_description (*IF_CURRENT = Parsetree.value_description *) =
      {
       pval_name: string loc;
       pval_type: core_type;
       pval_prim: string list;
       pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
       pval_loc: Location.t;
      }

  (*
    val x: T                            (prim = [])
    external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
  *)

  (* Type declarations *)

  and type_declaration (*IF_CURRENT = Parsetree.type_declaration *) =
      {
       ptype_name: string loc;
       ptype_params: (core_type * variance) list;
             (* ('a1,...'an) t; None represents  _*)
       ptype_cstrs: (core_type * core_type * Location.t) list;
             (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
       ptype_kind: type_kind;
       ptype_private: private_flag;   (* = private ... *)
       ptype_manifest: core_type option;  (* = T *)
       ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
       ptype_loc: Location.t;
      }

  (*
    type t                     (abstract, no manifest)
    type t = T0                (abstract, manifest=T0)
    type t = C of T | ...      (variant,  no manifest)
    type t = T0 = C of T | ... (variant,  manifest=T0)
    type t = {l: T; ...}       (record,   no manifest)
    type t = T0 = {l : T; ...} (record,   manifest=T0)
    type t = ..                (open,     no manifest)
  *)

  and type_kind (*IF_CURRENT = Parsetree.type_kind *) =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
          (* Invariant: non-empty list *)
    | Ptype_record of label_declaration list
          (* Invariant: non-empty list *)
    | Ptype_open

  and label_declaration (*IF_CURRENT = Parsetree.label_declaration *) =
      {
       pld_name: string loc;
       pld_mutable: mutable_flag;
       pld_type: core_type;
       pld_loc: Location.t;
       pld_attributes: attributes; (* l [@id1] [@id2] : T *)
      }

  (*  { ...; l: T; ... }            (mutable=Immutable)
      { ...; mutable l: T; ... }    (mutable=Mutable)

      Note: T can be a Ptyp_poly.
  *)

  and constructor_declaration (*IF_CURRENT = Parsetree.constructor_declaration *) =
      {
       pcd_name: string loc;
       pcd_args: constructor_arguments;
       pcd_res: core_type option;
       pcd_loc: Location.t;
       pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }

  and constructor_arguments (*IF_CURRENT = Parsetree.constructor_arguments *) =
    | Pcstr_tuple of core_type list
    | Pcstr_record of label_declaration list

  (*
    | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
    | C: T0                  (res = Some T0, args = [])
    | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
    | C of {...}             (res = None,    args = Pcstr_record)
    | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
    | C of {...} as t        (res = None,    args = Pcstr_record)
  *)

  and type_extension (*IF_CURRENT = Parsetree.type_extension *) =
      {
       ptyext_path: Longident.t loc;
       ptyext_params: (core_type * variance) list;
       ptyext_constructors: extension_constructor list;
       ptyext_private: private_flag;
       ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
      }
  (*
    type t += ...
  *)

  and extension_constructor (*IF_CURRENT = Parsetree.extension_constructor *) =
      {
       pext_name: string loc;
       pext_kind : extension_constructor_kind;
       pext_loc : Location.t;
       pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }

  and extension_constructor_kind (*IF_CURRENT = Parsetree.extension_constructor_kind *) =
      Pext_decl of constructor_arguments * core_type option
        (*
           | C of T1 * ... * Tn     ([T1; ...; Tn], None)
           | C: T0                  ([], Some T0)
           | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
         *)
    | Pext_rebind of Longident.t loc
        (*
           | C = D
         *)

  (** {2 Class language} *)

  (* Type expressions for the class language *)

  and class_type (*IF_CURRENT = Parsetree.class_type *) =
      {
       pcty_desc: class_type_desc;
       pcty_loc: Location.t;
       pcty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_type_desc (*IF_CURRENT = Parsetree.class_type_desc *) =
    | Pcty_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcty_signature of class_signature
          (* object ... end *)
    | Pcty_arrow of arg_label * core_type * class_type
          (* T -> CT       Simple
             ~l:T -> CT    Labelled l
             ?l:T -> CT    Optional l
           *)
    | Pcty_extension of extension
          (* [%id] *)

  and class_signature (*IF_CURRENT = Parsetree.class_signature *) =
      {
       pcsig_self: core_type;
       pcsig_fields: class_type_field list;
      }
  (* object('selfpat) ... end
     object ... end             (self = Ptyp_any)
   *)

  and class_type_field (*IF_CURRENT = Parsetree.class_type_field *) =
      {
       pctf_desc: class_type_field_desc;
       pctf_loc: Location.t;
       pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_type_field_desc (*IF_CURRENT = Parsetree.class_type_field_desc *) =
    | Pctf_inherit of class_type
          (* inherit CT *)
    | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
          (* val x: T *)
    | Pctf_method  of (string * private_flag * virtual_flag * core_type)
          (* method x: T

             Note: T can be a Ptyp_poly.
           *)
    | Pctf_constraint  of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pctf_attribute of attribute
          (* [@@@id] *)
    | Pctf_extension of extension
          (* [%%id] *)

  and 'a class_infos (*IF_CURRENT = 'a Parsetree.class_infos *) =
      {
       pci_virt: virtual_flag;
       pci_params: (core_type * variance) list;
       pci_name: string loc;
       pci_expr: 'a;
       pci_loc: Location.t;
       pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
      }
  (* class c = ...
     class ['a1,...,'an] c = ...
     class virtual c = ...

     Also used for "class type" declaration.
  *)

  and class_description = class_type class_infos

  and class_type_declaration = class_type class_infos

  (* Value expressions for the class language *)

  and class_expr (*IF_CURRENT = Parsetree.class_expr *) =
      {
       pcl_desc: class_expr_desc;
       pcl_loc: Location.t;
       pcl_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_expr_desc (*IF_CURRENT = Parsetree.class_expr_desc *) =
    | Pcl_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcl_structure of class_structure
          (* object ... end *)
    | Pcl_fun of arg_label * expression option * pattern * class_expr
          (* fun P -> CE                          (Simple, None)
             fun ~l:P -> CE                       (Labelled l, None)
             fun ?l:P -> CE                       (Optional l, None)
             fun ?l:(P = E0) -> CE                (Optional l, Some E0)
           *)
    | Pcl_apply of class_expr * (arg_label * expression) list
          (* CE ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pcl_let of rec_flag * value_binding list * class_expr
          (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
           *)
    | Pcl_constraint of class_expr * class_type
          (* (CE : CT) *)
    | Pcl_extension of extension
          (* [%id] *)

  and class_structure (*IF_CURRENT = Parsetree.class_structure *) =
      {
       pcstr_self: pattern;
       pcstr_fields: class_field list;
      }
  (* object(selfpat) ... end
     object ... end           (self = Ppat_any)
   *)

  and class_field (*IF_CURRENT = Parsetree.class_field *) =
      {
       pcf_desc: class_field_desc;
       pcf_loc: Location.t;
       pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_field_desc (*IF_CURRENT = Parsetree.class_field_desc *) =
    | Pcf_inherit of override_flag * class_expr * string option
          (* inherit CE
             inherit CE as x
             inherit! CE
             inherit! CE as x
           *)
    | Pcf_val of (string loc * mutable_flag * class_field_kind)
          (* val x = E
             val virtual x: T
           *)
    | Pcf_method of (string loc * private_flag * class_field_kind)
          (* method x = E            (E can be a Pexp_poly)
             method virtual x: T     (T can be a Ptyp_poly)
           *)
    | Pcf_constraint of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pcf_initializer of expression
          (* initializer E *)
    | Pcf_attribute of attribute
          (* [@@@id] *)
    | Pcf_extension of extension
          (* [%%id] *)

  and class_field_kind (*IF_CURRENT = Parsetree.class_field_kind *) =
    | Cfk_virtual of core_type
    | Cfk_concrete of override_flag * expression

  and class_declaration = class_expr class_infos

  (** {2 Module language} *)

  (* Type expressions for the module language *)

  and module_type (*IF_CURRENT = Parsetree.module_type *) =
      {
       pmty_desc: module_type_desc;
       pmty_loc: Location.t;
       pmty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_type_desc (*IF_CURRENT = Parsetree.module_type_desc *) =
    | Pmty_ident of Longident.t loc
          (* S *)
    | Pmty_signature of signature
          (* sig ... end *)
    | Pmty_functor of string loc * module_type option * module_type
          (* functor(X : MT1) -> MT2 *)
    | Pmty_with of module_type * with_constraint list
          (* MT with ... *)
    | Pmty_typeof of module_expr
          (* module type of ME *)
    | Pmty_extension of extension
          (* [%id] *)
    | Pmty_alias of Longident.t loc
          (* (module M) *)

  and signature = signature_item list

  and signature_item (*IF_CURRENT = Parsetree.signature_item *) =
      {
       psig_desc: signature_item_desc;
       psig_loc: Location.t;
      }

  and signature_item_desc (*IF_CURRENT = Parsetree.signature_item_desc *) =
    | Psig_value of value_description
          (*
            val x: T
            external x: T = "s1" ... "sn"
           *)
    | Psig_type of rec_flag * type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Psig_typext of type_extension
          (* type t1 += ... *)
    | Psig_exception of extension_constructor
          (* exception C of T *)
    | Psig_module of module_declaration
          (* module X : MT *)
    | Psig_recmodule of module_declaration list
          (* module rec X1 : MT1 and ... and Xn : MTn *)
    | Psig_modtype of module_type_declaration
          (* module type S = MT
             module type S *)
    | Psig_open of open_description
          (* open X *)
    | Psig_include of include_description
          (* include MT *)
    | Psig_class of class_description list
          (* class c1 : ... and ... and cn : ... *)
    | Psig_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Psig_attribute of attribute
          (* [@@@id] *)
    | Psig_extension of extension * attributes
          (* [%%id] *)

  and module_declaration (*IF_CURRENT = Parsetree.module_declaration *) =
      {
       pmd_name: string loc;
       pmd_type: module_type;
       pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmd_loc: Location.t;
      }
  (* S : MT *)

  and module_type_declaration (*IF_CURRENT = Parsetree.module_type_declaration *) =
      {
       pmtd_name: string loc;
       pmtd_type: module_type option;
       pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmtd_loc: Location.t;
      }
  (* S = MT
     S       (abstract module type declaration, pmtd_type = None)
  *)

  and open_description (*IF_CURRENT = Parsetree.open_description *) =
      {
       popen_lid: Longident.t loc;
       popen_override: override_flag;
       popen_loc: Location.t;
       popen_attributes: attributes;
      }
  (* open! X - popen_override = Override (silences the 'used identifier
                                shadowing' warning)
     open  X - popen_override = Fresh
   *)

  and 'a include_infos (*IF_CURRENT = 'a Parsetree.include_infos *) =
      {
       pincl_mod: 'a;
       pincl_loc: Location.t;
       pincl_attributes: attributes;
      }

  and include_description = module_type include_infos
  (* include MT *)

  and include_declaration = module_expr include_infos
  (* include ME *)

  and with_constraint (*IF_CURRENT = Parsetree.with_constraint *) =
    | Pwith_type of Longident.t loc * type_declaration
          (* with type X.t = ...

             Note: the last component of the longident must match
             the name of the type_declaration. *)
    | Pwith_module of Longident.t loc * Longident.t loc
          (* with module X.Y = Z *)
    | Pwith_typesubst of type_declaration
          (* with type t := ... *)
    | Pwith_modsubst of string loc * Longident.t loc
          (* with module X := Z *)

  (* Value expressions for the module language *)

  and module_expr (*IF_CURRENT = Parsetree.module_expr *) =
      {
       pmod_desc: module_expr_desc;
       pmod_loc: Location.t;
       pmod_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_expr_desc (*IF_CURRENT = Parsetree.module_expr_desc *) =
    | Pmod_ident of Longident.t loc
          (* X *)
    | Pmod_structure of structure
          (* struct ... end *)
    | Pmod_functor of string loc * module_type option * module_expr
          (* functor(X : MT1) -> ME *)
    | Pmod_apply of module_expr * module_expr
          (* ME1(ME2) *)
    | Pmod_constraint of module_expr * module_type
          (* (ME : MT) *)
    | Pmod_unpack of expression
          (* (val E) *)
    | Pmod_extension of extension
          (* [%id] *)

  and structure = structure_item list

  and structure_item (*IF_CURRENT = Parsetree.structure_item *) =
      {
       pstr_desc: structure_item_desc;
       pstr_loc: Location.t;
      }

  and structure_item_desc (*IF_CURRENT = Parsetree.structure_item_desc *) =
    | Pstr_eval of expression * attributes
          (* E *)
    | Pstr_value of rec_flag * value_binding list
          (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
           *)
    | Pstr_primitive of value_description
          (*  val x: T
              external x: T = "s1" ... "sn" *)
    | Pstr_type of rec_flag * type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Pstr_typext of type_extension
          (* type t1 += ... *)
    | Pstr_exception of extension_constructor
          (* exception C of T
             exception C = M.X *)
    | Pstr_module of module_binding
          (* module X = ME *)
    | Pstr_recmodule of module_binding list
          (* module rec X1 = ME1 and ... and Xn = MEn *)
    | Pstr_modtype of module_type_declaration
          (* module type S = MT *)
    | Pstr_open of open_description
          (* open X *)
    | Pstr_class of class_declaration list
          (* class c1 = ... and ... and cn = ... *)
    | Pstr_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Pstr_include of include_declaration
          (* include ME *)
    | Pstr_attribute of attribute
          (* [@@@id] *)
    | Pstr_extension of extension * attributes
          (* [%%id] *)

  and value_binding (*IF_CURRENT = Parsetree.value_binding *) =
    {
      pvb_pat: pattern;
      pvb_expr: expression;
      pvb_attributes: attributes;
      pvb_loc: Location.t;
    }

  and module_binding (*IF_CURRENT = Parsetree.module_binding *) =
      {
       pmb_name: string loc;
       pmb_expr: module_expr;
       pmb_attributes: attributes;
       pmb_loc: Location.t;
      }
  (* X = ME *)

  (** {2 Toplevel} *)

  (* Toplevel phrases *)

  type toplevel_phrase (*IF_CURRENT = Parsetree.toplevel_phrase *) =
    | Ptop_def of structure
    | Ptop_dir of string * directive_argument
       (* #use, #load ... *)

  and directive_argument (*IF_CURRENT = Parsetree.directive_argument *) =
    | Pdir_none
    | Pdir_string of string
    | Pdir_int of string * char option
    | Pdir_ident of Longident.t
    | Pdir_bool of bool
end

module Docstrings : sig
  (** {3 Docstrings} *)

  (** Documentation comments *)
  type docstring

  (** Create a docstring *)
  val docstring : string -> Location.t -> docstring

  (** Get the text of a docstring *)
  val docstring_body : docstring -> string

  (** Get the location of a docstring *)
  val docstring_loc : docstring -> Location.t

  (** {3 Items}

      The {!docs} type represents documentation attached to an item. *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  val empty_docs : docs

  val docs_attr : docstring -> Parsetree.attribute

  (** Convert item documentation to attributes and add them to an
      attribute list *)
  val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Fields and constructors}

      The {!info} type represents documentation attached to a field or
      constructor. *)

  type info = docstring option

  val empty_info : info

  val info_attr : docstring -> Parsetree.attribute

  (** Convert field info to attributes and add them to an
      attribute list *)
  val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Unattached comments}

      The {!text} type represents documentation which is not attached to
      anything. *)

  type text = docstring list

  val empty_text : text

  val text_attr : docstring -> Parsetree.attribute

  (** Convert text to attributes and add them to an attribute list *)
  val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

end = struct
  open Location

  (* Docstrings *)

  type docstring =
    { ds_body: string;
      ds_loc: Location.t; }

  (* Docstring constructors and destructors *)

  let docstring body loc =
    let ds =
      { ds_body = body;
        ds_loc = loc; }
    in
    ds

  let docstring_body ds = ds.ds_body

  let docstring_loc ds = ds.ds_loc

  (* Docstrings attached to items *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  let empty_docs = { docs_pre = None; docs_post = None }

  let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

  let docs_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (doc_loc, PStr [item])

  let add_docs_attrs docs attrs =
    let attrs =
      match docs.docs_pre with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> docs_attr ds :: attrs
    in
    let attrs =
      match docs.docs_post with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> attrs @ [docs_attr ds]
    in
    attrs

  (* Docstrings attached to constructors or fields *)

  type info = docstring option

  let empty_info = None

  let info_attr = docs_attr

  let add_info_attrs info attrs =
    match info with
    | None | Some {ds_body=""; _} -> attrs
    | Some ds -> attrs @ [info_attr ds]

  (* Docstrings not attached to a specific item *)

  type text = docstring list

  let empty_text = []

  let text_loc = {txt = "ocaml.text"; loc = Location.none}

  let text_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (text_loc, PStr [item])

  let add_text_attrs dsl attrs =
    let fdsl = List.filter (function {ds_body=""; _} -> false| _ ->true) dsl in
    (List.map text_attr fdsl) @ attrs

end

module Ast_helper : sig
  (** Helpers to produce Parsetree fragments *)

  open Asttypes
  open Docstrings
  open Parsetree

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  (** {2 Default locations} *)

  val default_loc: loc ref
      (** Default value for all optional location arguments. *)

  val with_default_loc: loc -> (unit -> 'a) -> 'a
      (** Set the [default_loc] within the scope of the execution
          of the provided function. *)

  (** {2 Constants} *)

  module Const : sig
    val char : char -> constant
    val string : ?quotation_delimiter:string -> string -> constant
    val integer : ?suffix:char -> string -> constant
    val int : ?suffix:char -> int -> constant
    val int32 : ?suffix:char -> int32 -> constant
    val int64 : ?suffix:char -> int64 -> constant
    val nativeint : ?suffix:char -> nativeint -> constant
    val float : ?suffix:char -> string -> constant
  end

  (** {2 Core language} *)

  (** Type expressions *)
  module Typ :
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
      val attr: core_type -> attribute -> core_type

      val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
      val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
      val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type -> core_type
                 -> core_type
      val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val object_: ?loc:loc -> ?attrs:attrs ->
                    (string * attributes * core_type) list -> closed_flag ->
                    core_type
      val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
      val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
                   -> label list option -> core_type
      val poly: ?loc:loc -> ?attrs:attrs -> string list -> core_type -> core_type
      val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
                   -> core_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

      val force_poly: core_type -> core_type
    end

  (** Patterns *)
  module Pat:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
      val attr:pattern -> attribute -> pattern

      val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
      val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
      val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
      val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
      val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
      val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
                  -> pattern
      val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
      val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
      val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
      val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val unpack: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val open_: ?loc:loc -> ?attrs:attrs  -> lid -> pattern -> pattern
      val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
    end

  (** Expressions *)
  module Exp:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
      val attr: expression -> attribute -> expression

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
                -> expression -> expression
      val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option
                -> pattern -> expression -> expression
      val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
      val apply: ?loc:loc -> ?attrs:attrs -> expression
                 -> (arg_label * expression) list -> expression
      val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
                  -> expression
      val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
      val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                     -> expression
      val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
                   -> expression
      val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                  -> expression option -> expression
      val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
      val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                    -> expression
      val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                      -> expression option -> expression
      val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression
      val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
      val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
                -> direction_flag -> expression -> expression
      val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                  -> core_type -> expression
      val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                       -> expression
      val send: ?loc:loc -> ?attrs:attrs -> expression -> string -> expression
      val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
      val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                    -> expression
      val letmodule: ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression
                     -> expression
      val letexception:
        ?loc:loc -> ?attrs:attrs -> extension_constructor -> expression
        -> expression
      val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                -> expression
      val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
      val newtype: ?loc:loc -> ?attrs:attrs -> string -> expression -> expression
      val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
      val open_: ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> expression
                 -> expression
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
      val unreachable: ?loc:loc -> ?attrs:attrs -> unit -> expression

      val case: pattern -> ?guard:expression -> expression -> case
    end

  (** Value declarations *)
  module Val:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        ?prim:string list -> str -> core_type -> value_description
    end

  (** Type declarations *)
  module Type:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?params:(core_type * variance) list ->
        ?cstrs:(core_type * core_type * loc) list ->
        ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
        type_declaration

      val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?args:constructor_arguments -> ?res:core_type -> str ->
        constructor_declaration
      val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?mut:mutable_flag -> str -> core_type -> label_declaration
    end

  (** Type extensions *)
  module Te:
    sig
      val mk: ?attrs:attrs -> ?docs:docs ->
        ?params:(core_type * variance) list -> ?priv:private_flag ->
        lid -> extension_constructor list -> type_extension

      val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> extension_constructor_kind -> extension_constructor

      val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        ?args:constructor_arguments -> ?res:core_type -> str ->
        extension_constructor
      val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> lid -> extension_constructor
    end

  (** {2 Module language} *)

  (** Module type expressions *)
  module Mty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
      val attr: module_type -> attribute -> module_type

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_type -> module_type
      val with_: ?loc:loc -> ?attrs:attrs -> module_type ->
        with_constraint list -> module_type
      val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
    end

  (** Module expressions *)
  module Mod:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
      val attr: module_expr -> attribute -> module_expr

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
      val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_expr -> module_expr
      val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr ->
        module_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type ->
        module_expr
      val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
    end

  (** Signature items *)
  module Sig:
    sig
      val mk: ?loc:loc -> signature_item_desc -> signature_item

      val value: ?loc:loc -> value_description -> signature_item
      val type_: ?loc:loc -> rec_flag -> type_declaration list -> signature_item
      val type_extension: ?loc:loc -> type_extension -> signature_item
      val exception_: ?loc:loc -> extension_constructor -> signature_item
      val module_: ?loc:loc -> module_declaration -> signature_item
      val rec_module: ?loc:loc -> module_declaration list -> signature_item
      val modtype: ?loc:loc -> module_type_declaration -> signature_item
      val open_: ?loc:loc -> open_description -> signature_item
      val include_: ?loc:loc -> include_description -> signature_item
      val class_: ?loc:loc -> class_description list -> signature_item
      val class_type: ?loc:loc -> class_type_declaration list -> signature_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
      val attribute: ?loc:loc -> attribute -> signature_item
      val text: text -> signature_item list
    end

  (** Structure items *)
  module Str:
    sig
      val mk: ?loc:loc -> structure_item_desc -> structure_item

      val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
      val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
      val primitive: ?loc:loc -> value_description -> structure_item
      val type_: ?loc:loc -> rec_flag -> type_declaration list -> structure_item
      val type_extension: ?loc:loc -> type_extension -> structure_item
      val exception_: ?loc:loc -> extension_constructor -> structure_item
      val module_: ?loc:loc -> module_binding -> structure_item
      val rec_module: ?loc:loc -> module_binding list -> structure_item
      val modtype: ?loc:loc -> module_type_declaration -> structure_item
      val open_: ?loc:loc -> open_description -> structure_item
      val class_: ?loc:loc -> class_declaration list -> structure_item
      val class_type: ?loc:loc -> class_type_declaration list -> structure_item
      val include_: ?loc:loc -> include_declaration -> structure_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
      val attribute: ?loc:loc -> attribute -> structure_item
      val text: text -> structure_item list
    end

  (** Module declarations *)
  module Md:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_type -> module_declaration
    end

  (** Module type declarations *)
  module Mtd:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?typ:module_type -> str -> module_type_declaration
    end

  (** Module bindings *)
  module Mb:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_expr -> module_binding
    end

  (** Opens *)
  module Opn:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
        ?override:override_flag -> lid -> open_description
    end

  (** Includes *)
  module Incl:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
    end

  (** Value bindings *)
  module Vb:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        pattern -> expression -> value_binding
    end


  (** {2 Class language} *)

  (** Class type expressions *)
  module Cty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
      val attr: class_type -> attribute -> class_type

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
      val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
      val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type ->
        class_type -> class_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    end

  (** Class type fields *)
  module Ctf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        class_type_field_desc -> class_type_field
      val attr: class_type_field -> attribute -> class_type_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
      val val_: ?loc:loc -> ?attrs:attrs -> string -> mutable_flag ->
        virtual_flag -> core_type -> class_type_field
      val method_: ?loc:loc -> ?attrs:attrs -> string -> private_flag ->
        virtual_flag -> core_type -> class_type_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
        class_type_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
      val attribute: ?loc:loc -> attribute -> class_type_field
      val text: text -> class_type_field list
    end

  (** Class expressions *)
  module Cl:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
      val attr: class_expr -> attribute -> class_expr

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
      val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
      val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option ->
        pattern -> class_expr -> class_expr
      val apply: ?loc:loc -> ?attrs:attrs -> class_expr ->
        (arg_label * expression) list -> class_expr
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list ->
        class_expr -> class_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type ->
        class_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    end

  (** Class fields *)
  module Cf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc ->
        class_field
      val attr: class_field -> attribute -> class_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr ->
        string option -> class_field
      val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
        class_field_kind -> class_field
      val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
        class_field_kind -> class_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
        class_field
      val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
      val attribute: ?loc:loc -> attribute -> class_field
      val text: text -> class_field list

      val virtual_: core_type -> class_field_kind
      val concrete: override_flag -> expression -> class_field_kind

    end

  (** Classes *)
  module Ci:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?virt:virtual_flag -> ?params:(core_type * variance) list ->
        str -> 'a -> 'a class_infos
    end

  (** Class signatures *)
  module Csig:
    sig
      val mk: core_type -> class_type_field list -> class_signature
    end

  (** Class structures *)
  module Cstr:
    sig
      val mk: pattern -> class_field list -> class_structure
    end

end = struct
  (** Helpers to produce Parsetree fragments *)

  open Asttypes
  open Parsetree
  open Docstrings

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  let default_loc = ref Location.none

  let with_default_loc l f =
    let old = !default_loc in
    default_loc := l;
    try let r = f () in default_loc := old; r
    with exn -> default_loc := old; raise exn

  module Const = struct
    let integer ?suffix i = Pconst_integer (i, suffix)
    let int ?suffix i = integer ?suffix (string_of_int i)
    let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
    let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
    let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
    let float ?suffix f = Pconst_float (f, suffix)
    let char c = Pconst_char c
    let string ?quotation_delimiter s = Pconst_string (s, quotation_delimiter)
  end

  module Typ = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
    let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
    let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
    let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
    let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
    let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
    let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

    let force_poly t =
      match t.ptyp_desc with
      | Ptyp_poly _ -> t
      | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)
  end

  module Pat = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
    let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
    let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
    let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
    let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
    let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
    let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
    let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
    let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
  end

  module Exp = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
    let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
    let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
    let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
    let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
    let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
    let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
    let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
    let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
    let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
    let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
    let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
    let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
    let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
    let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
    let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
    let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
    let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
    let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
    let letexception ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letexception (a, b))
    let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
    let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
    let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
    let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
    let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
    let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable

    let case lhs ?guard rhs =
      {
       pc_lhs = lhs;
       pc_guard = guard;
       pc_rhs = rhs;
      }
  end

  module Mty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
    let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
    let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
    let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
    let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
    let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
  end

  module Mod = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
    let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

    let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
    let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
    let functor_ ?loc ?attrs arg arg_ty body =
      mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
    let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
    let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
    let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
  end

  module Sig = struct
    let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

    let value ?loc a = mk ?loc (Psig_value a)
    let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Psig_typext a)
    let exception_ ?loc a = mk ?loc (Psig_exception a)
    let module_ ?loc a = mk ?loc (Psig_module a)
    let rec_module ?loc a = mk ?loc (Psig_recmodule a)
    let modtype ?loc a = mk ?loc (Psig_modtype a)
    let open_ ?loc a = mk ?loc (Psig_open a)
    let include_ ?loc a = mk ?loc (Psig_include a)
    let class_ ?loc a = mk ?loc (Psig_class a)
    let class_type ?loc a = mk ?loc (Psig_class_type a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Psig_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Str = struct
    let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

    let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
    let value ?loc a b = mk ?loc (Pstr_value (a, b))
    let primitive ?loc a = mk ?loc (Pstr_primitive a)
    let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Pstr_typext a)
    let exception_ ?loc a = mk ?loc (Pstr_exception a)
    let module_ ?loc a = mk ?loc (Pstr_module a)
    let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
    let modtype ?loc a = mk ?loc (Pstr_modtype a)
    let open_ ?loc a = mk ?loc (Pstr_open a)
    let class_ ?loc a = mk ?loc (Pstr_class a)
    let class_type ?loc a = mk ?loc (Pstr_class_type a)
    let include_ ?loc a = mk ?loc (Pstr_include a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Pstr_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Cl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcl_desc = d;
       pcl_loc = loc;
       pcl_attributes = attrs;
      }
    let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
    let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
  end

  module Cty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcty_desc = d;
       pcty_loc = loc;
       pcty_attributes = attrs;
      }
    let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
  end

  module Ctf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
             ?(docs = empty_docs) d =
      {
       pctf_desc = d;
       pctf_loc = loc;
       pctf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
    let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
    let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
    let attribute ?loc a = mk ?loc (Pctf_attribute a)
    let text txt =
     let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
       List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

  end

  module Cf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) d =
      {
       pcf_desc = d;
       pcf_loc = loc;
       pcf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
    let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
    let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
    let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
    let attribute ?loc a = mk ?loc (Pcf_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let virtual_ ct = Cfk_virtual ct
    let concrete o e = Cfk_concrete (o, e)

    let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  end

  module Val = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(prim = []) name typ =
      {
       pval_name = name;
       pval_type = typ;
       pval_attributes = add_docs_attrs docs attrs;
       pval_loc = loc;
       pval_prim = prim;
      }
  end

  module Md = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name typ =
      {
       pmd_name = name;
       pmd_type = typ;
       pmd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmd_loc = loc;
      }
  end

  module Mtd = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) ?typ name =
      {
       pmtd_name = name;
       pmtd_type = typ;
       pmtd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmtd_loc = loc;
      }
  end

  module Mb = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name expr =
      {
       pmb_name = name;
       pmb_expr = expr;
       pmb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmb_loc = loc;
      }
  end

  module Opn = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(override = Fresh) lid =
      {
       popen_lid = lid;
       popen_override = override;
       popen_loc = loc;
       popen_attributes = add_docs_attrs docs attrs;
      }
  end

  module Incl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
      {
       pincl_mod = mexpr;
       pincl_loc = loc;
       pincl_attributes = add_docs_attrs docs attrs;
      }

  end

  module Vb = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(text = []) pat expr =
      {
       pvb_pat = pat;
       pvb_expr = expr;
       pvb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pvb_loc = loc;
      }
  end

  module Ci = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
          ?(virt = Concrete) ?(params = []) name expr =
      {
       pci_virt = virt;
       pci_params = params;
       pci_name = name;
       pci_expr = expr;
       pci_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pci_loc = loc;
      }
  end

  module Type = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
        ?(params = [])
        ?(cstrs = [])
        ?(kind = Ptype_abstract)
        ?(priv = Public)
        ?manifest
        name =
      {
       ptype_name = name;
       ptype_params = params;
       ptype_cstrs = cstrs;
       ptype_kind = kind;
       ptype_private = priv;
       ptype_manifest = manifest;
       ptype_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       ptype_loc = loc;
      }

    let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(args = Pcstr_tuple []) ?res name =
      {
       pcd_name = name;
       pcd_args = args;
       pcd_res = res;
       pcd_loc = loc;
       pcd_attributes = add_info_attrs info attrs;
      }

    let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(mut = Immutable) name typ =
      {
       pld_name = name;
       pld_mutable = mut;
       pld_type = typ;
       pld_loc = loc;
       pld_attributes = add_info_attrs info attrs;
      }

  end

  (** Type extensions *)
  module Te = struct
    let mk ?(attrs = []) ?(docs = empty_docs)
          ?(params = []) ?(priv = Public) path constructors =
      {
       ptyext_path = path;
       ptyext_params = params;
       ptyext_constructors = constructors;
       ptyext_private = priv;
       ptyext_attributes = add_docs_attrs docs attrs;
      }

    let constructor ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name kind =
      {
       pext_name = name;
       pext_kind = kind;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let decl ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
               ?(info = empty_info) ?(args = Pcstr_tuple []) ?res name =
      {
       pext_name = name;
       pext_kind = Pext_decl(args, res);
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let rebind ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name lid =
      {
       pext_name = name;
       pext_kind = Pext_rebind lid;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

  end

  module Csig = struct
    let mk self fields =
      {
       pcsig_self = self;
       pcsig_fields = fields;
      }
  end

  module Cstr = struct
    let mk self fields =
      {
       pcstr_self = self;
       pcstr_fields = fields;
      }
  end

end

module Ast_mapper : sig
  (** The interface of a -ppx rewriter

    A -ppx rewriter is a program that accepts a serialized abstract syntax
    tree and outputs another, possibly modified, abstract syntax tree.
    This module encapsulates the interface between the compiler and
    the -ppx rewriters, handling such details as the serialization format,
    forwarding of command-line flags, and storing state.

    {!mapper} allows to implement AST rewriting using open recursion.
    A typical mapper would be based on {!default_mapper}, a deep
    identity mapper, and will fall back on it for handling the syntax it
    does not modify. For example:

    {[
  open Asttypes
  open Parsetree
  open Ast_mapper

  let test_mapper argv =
    { default_mapper with
      expr = fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
          Ast_helper.Exp.constant (Const_int 42)
        | other -> default_mapper.expr mapper other; }

  let () =
    register "ppx_test" test_mapper]}

    This -ppx rewriter, which replaces [[%test]] in expressions with
    the constant [42], can be compiled using
    [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

    *)

  open Parsetree

  (** {2 A generic Parsetree mapper} *)

  type mapper (*IF_CURRENT = Ast_mapper.mapper*) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }
  (** A mapper record implements one "method" per syntactic category,
      using an open recursion style: each method takes as its first
      argument the mapper to be applied to children in the syntax
      tree. *)

  val default_mapper: mapper
  (** A default mapper, which implements a "deep identity" mapping. *)

  (** {2 Convenience functions to write mappers} *)

  val map_opt: ('a -> 'b) -> 'a option -> 'b option

  val extension_of_error: Location.error -> extension
  (** Encode an error into an 'ocaml.error' extension node which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the error. *)

  val attribute_of_warning: Location.t -> string -> attribute
  (** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the warning. *)

end = struct
  (* A generic Parsetree mapping class *)

  (*
  [@@@ocaml.warning "+9"]
    (* Ensure that record patterns don't miss any field. *)
  *)


  open Parsetree
  open Ast_helper
  open Location

  type mapper (*IF_CURRENT = Ast_mapper.mapper*) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }

  let map_fst f (x, y) = (f x, y)
  let map_snd f (x, y) = (x, f y)
  let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
  let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
  let map_opt f = function None -> None | Some x -> Some (f x)

  let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

  module T = struct
    (* Type expressions for the core language *)

    let row_field sub = function
      | Rtag (l, attrs, b, tl) ->
          Rtag (l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
      | Rinherit t -> Rinherit (sub.typ sub t)

    let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
      let open Typ in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ptyp_any -> any ~loc ~attrs ()
      | Ptyp_var s -> var ~loc ~attrs s
      | Ptyp_arrow (lab, t1, t2) ->
          arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
      | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
      | Ptyp_constr (lid, tl) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_object (l, o) ->
          let f (s, a, t) = (s, sub.attributes sub a, sub.typ sub t) in
          object_ ~loc ~attrs (List.map f l) o
      | Ptyp_class (lid, tl) ->
          class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
      | Ptyp_variant (rl, b, ll) ->
          variant ~loc ~attrs (List.map (row_field sub) rl) b ll
      | Ptyp_poly (sl, t) -> poly ~loc ~attrs sl (sub.typ sub t)
      | Ptyp_package (lid, l) ->
          package ~loc ~attrs (map_loc sub lid)
            (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
      | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_type_declaration sub
        {ptype_name; ptype_params; ptype_cstrs;
         ptype_kind;
         ptype_private;
         ptype_manifest;
         ptype_attributes;
         ptype_loc} =
      Type.mk (map_loc sub ptype_name)
        ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
        ~priv:ptype_private
        ~cstrs:(List.map
                  (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                  ptype_cstrs)
        ~kind:(sub.type_kind sub ptype_kind)
        ?manifest:(map_opt (sub.typ sub) ptype_manifest)
        ~loc:(sub.location sub ptype_loc)
        ~attrs:(sub.attributes sub ptype_attributes)

    let map_type_kind sub = function
      | Ptype_abstract -> Ptype_abstract
      | Ptype_variant l ->
          Ptype_variant (List.map (sub.constructor_declaration sub) l)
      | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
      | Ptype_open -> Ptype_open

    let map_constructor_arguments sub = function
      | Pcstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
      | Pcstr_record l ->
          Pcstr_record (List.map (sub.label_declaration sub) l)

    let map_type_extension sub
        {ptyext_path; ptyext_params;
         ptyext_constructors;
         ptyext_private;
         ptyext_attributes} =
      Te.mk
        (map_loc sub ptyext_path)
        (List.map (sub.extension_constructor sub) ptyext_constructors)
        ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
        ~priv:ptyext_private
        ~attrs:(sub.attributes sub ptyext_attributes)

    let map_extension_constructor_kind sub = function
        Pext_decl(ctl, cto) ->
          Pext_decl(map_constructor_arguments sub ctl, map_opt (sub.typ sub) cto)
      | Pext_rebind li ->
          Pext_rebind (map_loc sub li)

    let map_extension_constructor sub
        {pext_name;
         pext_kind;
         pext_loc;
         pext_attributes} =
      Te.constructor
        (map_loc sub pext_name)
        (map_extension_constructor_kind sub pext_kind)
        ~loc:(sub.location sub pext_loc)
        ~attrs:(sub.attributes sub pext_attributes)

  end

  module CT = struct
    (* Type expressions for the class language *)

    let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
      let open Cty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcty_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
      | Pcty_arrow (lab, t, ct) ->
          arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
      | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
      =
      let open Ctf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
      | Pctf_val (s, m, v, t) -> val_ ~loc ~attrs s m v (sub.typ sub t)
      | Pctf_method (s, p, v, t) -> method_ ~loc ~attrs s p v (sub.typ sub t)
      | Pctf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_signature sub {pcsig_self; pcsig_fields} =
      Csig.mk
        (sub.typ sub pcsig_self)
        (List.map (sub.class_type_field sub) pcsig_fields)
  end

  module MT = struct
    (* Type expressions for the module language *)

    let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
      let open Mty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
      | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
      | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
      | Pmty_functor (s, mt1, mt2) ->
          functor_ ~loc ~attrs (map_loc sub s)
            (Misc.may_map (sub.module_type sub) mt1)
            (sub.module_type sub mt2)
      | Pmty_with (mt, l) ->
          with_ ~loc ~attrs (sub.module_type sub mt)
            (List.map (sub.with_constraint sub) l)
      | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
      | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_with_constraint sub = function
      | Pwith_type (lid, d) ->
          Pwith_type (map_loc sub lid, sub.type_declaration sub d)
      | Pwith_module (lid, lid2) ->
          Pwith_module (map_loc sub lid, map_loc sub lid2)
      | Pwith_typesubst d -> Pwith_typesubst (sub.type_declaration sub d)
      | Pwith_modsubst (s, lid) ->
          Pwith_modsubst (map_loc sub s, map_loc sub lid)

    let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
      let open Sig in
      let loc = sub.location sub loc in
      match desc with
      | Psig_value vd -> value ~loc (sub.value_description sub vd)
      | Psig_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
      | Psig_recmodule l ->
          rec_module ~loc (List.map (sub.module_declaration sub) l)
      | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Psig_open x -> open_ ~loc (sub.open_description sub x)
      | Psig_include x -> include_ ~loc (sub.include_description sub x)
      | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
      | Psig_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Psig_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
  end


  module M = struct
    (* Value expressions for the module language *)

    let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
      let open Mod in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
      | Pmod_functor (arg, arg_ty, body) ->
          functor_ ~loc ~attrs (map_loc sub arg)
            (Misc.may_map (sub.module_type sub) arg_ty)
            (sub.module_expr sub body)
      | Pmod_apply (m1, m2) ->
          apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
      | Pmod_constraint (m, mty) ->
          constraint_ ~loc ~attrs (sub.module_expr sub m)
                      (sub.module_type sub mty)
      | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
      | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
      let open Str in
      let loc = sub.location sub loc in
      match desc with
      | Pstr_eval (x, attrs) ->
          eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
      | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
      | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
      | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
      | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
      | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Pstr_open x -> open_ ~loc (sub.open_description sub x)
      | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
      | Pstr_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
      | Pstr_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
  end

  module E = struct
    (* Value expressions for the core language *)

    let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
      let open Exp in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pexp_constant x -> constant ~loc ~attrs x
      | Pexp_let (r, vbs, e) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.expr sub e)
      | Pexp_fun (lab, def, p, e) ->
          fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
            (sub.expr sub e)
      | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
      | Pexp_apply (e, l) ->
          apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
      | Pexp_match (e, pel) ->
          match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_construct (lid, arg) ->
          construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
      | Pexp_variant (lab, eo) ->
          variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
      | Pexp_record (l, eo) ->
          record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
            (map_opt (sub.expr sub) eo)
      | Pexp_field (e, lid) ->
          field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
      | Pexp_setfield (e1, lid, e2) ->
          setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
            (sub.expr sub e2)
      | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_ifthenelse (e1, e2, e3) ->
          ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
            (map_opt (sub.expr sub) e3)
      | Pexp_sequence (e1, e2) ->
          sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_while (e1, e2) ->
          while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_for (p, e1, e2, d, e3) ->
          for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
            (sub.expr sub e3)
      | Pexp_coerce (e, t1, t2) ->
          coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
            (sub.typ sub t2)
      | Pexp_constraint (e, t) ->
          constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
      | Pexp_send (e, s) -> send ~loc ~attrs (sub.expr sub e) s
      | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
      | Pexp_setinstvar (s, e) ->
          setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
      | Pexp_override sel ->
          override ~loc ~attrs
            (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
      | Pexp_letmodule (s, me, e) ->
          letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
            (sub.expr sub e)
      | Pexp_letexception (cd, e) ->
          letexception ~loc ~attrs
            (sub.extension_constructor sub cd)
            (sub.expr sub e)
      | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
      | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
      | Pexp_poly (e, t) ->
          poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
      | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
      | Pexp_newtype (s, e) -> newtype ~loc ~attrs s (sub.expr sub e)
      | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
      | Pexp_open (ovf, lid, e) ->
          open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
      | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
      | Pexp_unreachable -> unreachable ~loc ~attrs ()
  end

  module P = struct
    (* Patterns *)

    let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
      let open Pat in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ppat_any -> any ~loc ~attrs ()
      | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
      | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
      | Ppat_constant c -> constant ~loc ~attrs c
      | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
      | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_construct (l, p) ->
          construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
      | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
      | Ppat_record (lpl, cf) ->
          record ~loc ~attrs
                 (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
      | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
      | Ppat_constraint (p, t) ->
          constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
      | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
      | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
      | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
      | Ppat_open (lid,p) -> open_ ~loc ~attrs (map_loc sub lid) (sub.pat sub p)
      | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
      | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
  end

  module CE = struct
    (* Value expressions for the class language *)

    let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
      let open Cl in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcl_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcl_structure s ->
          structure ~loc ~attrs (sub.class_structure sub s)
      | Pcl_fun (lab, e, p, ce) ->
          fun_ ~loc ~attrs lab
            (map_opt (sub.expr sub) e)
            (sub.pat sub p)
            (sub.class_expr sub ce)
      | Pcl_apply (ce, l) ->
          apply ~loc ~attrs (sub.class_expr sub ce)
            (List.map (map_snd (sub.expr sub)) l)
      | Pcl_let (r, vbs, ce) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.class_expr sub ce)
      | Pcl_constraint (ce, ct) ->
          constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
      | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_kind sub = function
      | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
      | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

    let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
      let open Cf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcf_inherit (o, ce, s) -> inherit_ ~loc ~attrs o (sub.class_expr sub ce) s
      | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
      | Pcf_method (s, p, k) ->
          method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
      | Pcf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
      | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure sub {pcstr_self; pcstr_fields} =
      {
        pcstr_self = sub.pat sub pcstr_self;
        pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
      }

    let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                           pci_loc; pci_attributes} =
      Ci.mk
       ~virt:pci_virt
       ~params:(List.map (map_fst (sub.typ sub)) pl)
        (map_loc sub pci_name)
        (f pci_expr)
        ~loc:(sub.location sub pci_loc)
        ~attrs:(sub.attributes sub pci_attributes)
  end

  (* Now, a generic AST mapper, to be extended to cover all kinds and
     cases of the OCaml grammar.  The default behavior of the mapper is
     the identity. *)

  let default_mapper =
    {
      structure = (fun this l -> List.map (this.structure_item this) l);
      structure_item = M.map_structure_item;
      module_expr = M.map;
      signature = (fun this l -> List.map (this.signature_item this) l);
      signature_item = MT.map_signature_item;
      module_type = MT.map;
      with_constraint = MT.map_with_constraint;
      class_declaration =
        (fun this -> CE.class_infos this (this.class_expr this));
      class_expr = CE.map;
      class_field = CE.map_field;
      class_structure = CE.map_structure;
      class_type = CT.map;
      class_type_field = CT.map_field;
      class_signature = CT.map_signature;
      class_type_declaration =
        (fun this -> CE.class_infos this (this.class_type this));
      class_description =
        (fun this -> CE.class_infos this (this.class_type this));
      type_declaration = T.map_type_declaration;
      type_kind = T.map_type_kind;
      typ = T.map;
      type_extension = T.map_type_extension;
      extension_constructor = T.map_extension_constructor;
      value_description =
        (fun this {pval_name; pval_type; pval_prim; pval_loc;
                   pval_attributes} ->
          Val.mk
            (map_loc this pval_name)
            (this.typ this pval_type)
            ~attrs:(this.attributes this pval_attributes)
            ~loc:(this.location this pval_loc)
            ~prim:pval_prim
        );

      pat = P.map;
      expr = E.map;

      module_declaration =
        (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
           Md.mk
             (map_loc this pmd_name)
             (this.module_type this pmd_type)
             ~attrs:(this.attributes this pmd_attributes)
             ~loc:(this.location this pmd_loc)
        );

      module_type_declaration =
        (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
           Mtd.mk
             (map_loc this pmtd_name)
             ?typ:(map_opt (this.module_type this) pmtd_type)
             ~attrs:(this.attributes this pmtd_attributes)
             ~loc:(this.location this pmtd_loc)
        );

      module_binding =
        (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
           Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
             ~attrs:(this.attributes this pmb_attributes)
             ~loc:(this.location this pmb_loc)
        );


      open_description =
        (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
           Opn.mk (map_loc this popen_lid)
             ~override:popen_override
             ~loc:(this.location this popen_loc)
             ~attrs:(this.attributes this popen_attributes)
        );


      include_description =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_type this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );

      include_declaration =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_expr this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );


      value_binding =
        (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
           Vb.mk
             (this.pat this pvb_pat)
             (this.expr this pvb_expr)
             ~loc:(this.location this pvb_loc)
             ~attrs:(this.attributes this pvb_attributes)
        );


      constructor_declaration =
        (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
          Type.constructor
            (map_loc this pcd_name)
            ~args:(T.map_constructor_arguments this pcd_args)
            ?res:(map_opt (this.typ this) pcd_res)
            ~loc:(this.location this pcd_loc)
            ~attrs:(this.attributes this pcd_attributes)
        );

      label_declaration =
        (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
           Type.field
             (map_loc this pld_name)
             (this.typ this pld_type)
             ~mut:pld_mutable
             ~loc:(this.location this pld_loc)
             ~attrs:(this.attributes this pld_attributes)
        );

      cases = (fun this l -> List.map (this.case this) l);
      case =
        (fun this {pc_lhs; pc_guard; pc_rhs} ->
           {
             pc_lhs = this.pat this pc_lhs;
             pc_guard = map_opt (this.expr this) pc_guard;
             pc_rhs = this.expr this pc_rhs;
           }
        );



      location = (fun _this l -> l);

      extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attributes = (fun this l -> List.map (this.attribute this) l);
      payload =
        (fun this -> function
           | PStr x -> PStr (this.structure this x)
           | PSig x -> PSig (this.signature this x)
           | PTyp x -> PTyp (this.typ this x)
           | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
        );
    }

  let rec extension_of_error {loc; msg; if_highlight; sub} =
    { loc; txt = "ocaml.error" },
    PStr ([Str.eval (Exp.constant (Pconst_string (msg, None)));
           Str.eval (Exp.constant (Pconst_string (if_highlight, None)))] @
          (List.map (fun ext -> Str.extension (extension_of_error ext)) sub))

  let attribute_of_warning loc s =
    { loc; txt = "ocaml.ppwarning" },
    PStr ([Str.eval ~loc (Exp.constant (Pconst_string (s, None)))])

end

module Outcometree = struct
  (* Module [Outcometree]: results displayed by the toplevel *)

  (* These types represent messages that the toplevel displays as normal
     results or errors. The real displaying is customisable using the hooks:
        [Toploop.print_out_value]
        [Toploop.print_out_type]
        [Toploop.print_out_sig_item]
        [Toploop.print_out_phrase] *)

  type out_ident (*IF_CURRENT = Outcometree.out_ident *) =
    | Oide_apply of out_ident * out_ident
    | Oide_dot of out_ident * string
    | Oide_ident of string

  type out_attribute (*IF_CURRENT = Outcometree.out_attribute *) =
    { oattr_name: string }

  type out_value (*IF_CURRENT = Outcometree.out_value *) =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option

  type out_type (*IF_CURRENT = Outcometree.out_type *) =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type * string
    | Otyp_arrow of string * out_type * out_type
    | Otyp_class of bool * out_ident * out_type list
    | Otyp_constr of out_ident * out_type list
    | Otyp_manifest of out_type * out_type
    | Otyp_object of (string * out_type) list * bool option
    | Otyp_record of (string * bool * out_type) list
    | Otyp_stuff of string
    | Otyp_sum of (string * out_type list * out_type option) list
    | Otyp_tuple of out_type list
    | Otyp_var of bool * string
    | Otyp_variant of
        bool * out_variant * bool * (string list) option
    | Otyp_poly of string list * out_type
    | Otyp_module of string * string list * out_type list
    | Otyp_attribute of out_type * out_attribute

  and out_variant (*IF_CURRENT = Outcometree.out_variant *) =
    | Ovar_fields of (string * bool * out_type list) list
    | Ovar_name of out_ident * out_type list

  type out_class_type (*IF_CURRENT = Outcometree.out_class_type *) =
    | Octy_constr of out_ident * out_type list
    | Octy_arrow of string * out_type * out_class_type
    | Octy_signature of out_type option * out_class_sig_item list
  and out_class_sig_item (*IF_CURRENT = Outcometree.out_class_sig_item *) =
    | Ocsg_constraint of out_type * out_type
    | Ocsg_method of string * bool * bool * out_type
    | Ocsg_value of string * bool * bool * out_type

  type out_module_type (*IF_CURRENT = Outcometree.out_module_type *) =
    | Omty_abstract
    | Omty_functor of string * out_module_type option * out_module_type
    | Omty_ident of out_ident
    | Omty_signature of out_sig_item list
    | Omty_alias of out_ident
  and out_sig_item (*IF_CURRENT = Outcometree.out_sig_item *) =
    | Osig_class of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_class_type of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_typext of out_extension_constructor * out_ext_status
    | Osig_modtype of string * out_module_type
    | Osig_module of string * out_module_type * out_rec_status
    | Osig_type of out_type_decl * out_rec_status
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl (*IF_CURRENT = Outcometree.out_type_decl *) =
    { otype_name: string;
      otype_params: (string * (bool * bool)) list;
      otype_type: out_type;
      otype_private: Asttypes.private_flag;
      otype_immediate: bool;
      otype_unboxed: bool;
      otype_cstrs: (out_type * out_type) list }
  and out_extension_constructor (*IF_CURRENT = Outcometree.out_extension_constructor *) =
    { oext_name: string;
      oext_type_name: string;
      oext_type_params: string list;
      oext_args: out_type list;
      oext_ret_type: out_type option;
      oext_private: Asttypes.private_flag }
  and out_type_extension (*IF_CURRENT = Outcometree.out_type_extension *) =
    { otyext_name: string;
      otyext_params: string list;
      otyext_constructors: (string * out_type list * out_type option) list;
      otyext_private: Asttypes.private_flag }
  and out_val_decl (*IF_CURRENT = Outcometree.out_val_decl *) =
    { oval_name: string;
      oval_type: out_type;
      oval_prims: string list;
      oval_attributes: out_attribute list }
  and out_rec_status (*IF_CURRENT = Outcometree.out_rec_status *) =
    | Orec_not
    | Orec_first
    | Orec_next
  and out_ext_status (*IF_CURRENT = Outcometree.out_ext_status *) =
    | Oext_first
    | Oext_next
    | Oext_exception

  type out_phrase (*IF_CURRENT = Outcometree.out_phrase *) =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)

end

module Config = struct
  let ast_impl_magic_number = "Caml1999M020"
  let ast_intf_magic_number = "Caml1999N018"
end

let map_signature mapper = mapper.Ast_mapper.signature mapper
let map_structure mapper = mapper.Ast_mapper.structure mapper

let failing_mapper =
  let fail _ _ =
    invalid_arg "failing_mapper: this mapper function should never get called"
  in
  {
    Ast_mapper.
    structure               = fail;
    structure_item          = fail;
    module_expr             = fail;
    signature               = fail;
    signature_item          = fail;
    module_type             = fail;
    with_constraint         = fail;
    class_declaration       = fail;
    class_expr              = fail;
    class_field             = fail;
    class_structure         = fail;
    class_type              = fail;
    class_type_field        = fail;
    class_signature         = fail;
    class_type_declaration  = fail;
    class_description       = fail;
    type_declaration        = fail;
    type_kind               = fail;
    typ                     = fail;
    type_extension          = fail;
    extension_constructor   = fail;
    value_description       = fail;
    pat                     = fail;
    expr                    = fail;
    module_declaration      = fail;
    module_type_declaration = fail;
    module_binding          = fail;
    open_description        = fail;
    include_description     = fail;
    include_declaration     = fail;
    value_binding           = fail;
    constructor_declaration = fail;
    label_declaration       = fail;
    cases                   = fail;
    case                    = fail;
    location                = fail;
    extension               = fail;
    attribute               = fail;
    attributes              = fail;
    payload                 = fail;
  }

let make_top_mapper ~signature ~structure =
  {failing_mapper with Ast_mapper.
                    signature = (fun _ x -> signature x);
                    structure = (fun _ x -> structure x) }

end
module Ast_402
= struct
#1 "ast_402.ml"
# 1 "src/ast_402.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*            Jérémie Dimino and Leo White, Jane Street Europe            *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                         Alain Frisch, LexiFi                           *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Location = Location
module Longident = Longident

module Asttypes = struct
  (* Auxiliary a.s.t. types used by parsetree and typedtree. *)

  type constant              = Asttypes.constant    =
      Const_int of int
    | Const_char of char
    | Const_string of string * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint

  type rec_flag              = Asttypes.rec_flag    = Nonrecursive | Recursive

  type direction_flag              = Asttypes.direction_flag    = Upto | Downto

  type private_flag              = Asttypes.private_flag    = Private | Public

  type mutable_flag              = Asttypes.mutable_flag    = Immutable | Mutable

  type virtual_flag              = Asttypes.virtual_flag    = Virtual | Concrete

  type override_flag              = Asttypes.override_flag    = Override | Fresh

  type closed_flag              = Asttypes.closed_flag    = Closed | Open

  type label = string

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }


  type variance              = Asttypes.variance    =
    | Covariant
    | Contravariant
    | Invariant
end

module Parsetree = struct
  (** Abstract syntax tree produced by parsing *)

  open Asttypes

  (** {2 Extension points} *)

  type attribute = string loc * payload
         (* [@id ARG]
            [@@id ARG]

            Metadata containers passed around within the AST.
            The compiler ignores unknown attributes.
         *)

  and extension = string loc * payload
        (* [%id ARG]
           [%%id ARG]

           Sub-language placeholder -- rejected by the typechecker.
        *)

  and attributes = attribute list

  and payload              = Parsetree.payload    =
    | PStr of structure
    | PTyp of core_type  (* : T *)
    | PPat of pattern * expression option  (* ? P  or  ? P when E *)

  (** {2 Core language} *)

  (* Type expressions *)

  and core_type              = Parsetree.core_type    =
      {
       ptyp_desc: core_type_desc;
       ptyp_loc: Location.t;
       ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and core_type_desc              = Parsetree.core_type_desc    =
    | Ptyp_any
          (*  _ *)
    | Ptyp_var of string
          (* 'a *)
    | Ptyp_arrow of label * core_type * core_type
          (* T1 -> T2       (label = "")
             ~l:T1 -> T2    (label = "l")
             ?l:T1 -> T2    (label = "?l")
           *)
    | Ptyp_tuple of core_type list
          (* T1 * ... * Tn

             Invariant: n >= 2
          *)
    | Ptyp_constr of Longident.t loc * core_type list
          (* tconstr
             T tconstr
             (T1, ..., Tn) tconstr
           *)
    | Ptyp_object of (string * attributes * core_type) list * closed_flag
          (* < l1:T1; ...; ln:Tn >     (flag = Closed)
             < l1:T1; ...; ln:Tn; .. > (flag = Open)
           *)
    | Ptyp_class of Longident.t loc * core_type list
          (* #tconstr
             T #tconstr
             (T1, ..., Tn) #tconstr
           *)
    | Ptyp_alias of core_type * string
          (* T as 'a *)
    | Ptyp_variant of row_field list * closed_flag * label list option
          (* [ `A|`B ]         (flag = Closed; labels = None)
             [> `A|`B ]        (flag = Open;   labels = None)
             [< `A|`B ]        (flag = Closed; labels = Some [])
             [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
           *)
    | Ptyp_poly of string list * core_type
          (* 'a1 ... 'an. T

             Can only appear in the following context:

             - As the core_type of a Ppat_constraint node corresponding
               to a constraint on a let-binding: let x : 'a1 ... 'an. T
               = e ...

             - Under Cfk_virtual for methods (not values).

             - As the core_type of a Pctf_method node.

             - As the core_type of a Pexp_poly node.

             - As the pld_type field of a label_declaration.

             - As a core_type of a Ptyp_object node.
           *)

    | Ptyp_package of package_type
          (* (module S) *)
    | Ptyp_extension of extension
          (* [%id] *)

  and package_type = Longident.t loc * (Longident.t loc * core_type) list
        (*
          (module S)
          (module S with type t1 = T1 and ... and tn = Tn)
         *)

  and row_field              = Parsetree.row_field    =
    | Rtag of label * attributes * bool * core_type list
          (* [`A]                   ( true,  [] )
             [`A of T]              ( false, [T] )
             [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
             [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

            - The 2nd field is true if the tag contains a
              constant (empty) constructor.
            - '&' occurs when several types are used for the same constructor
              (see 4.2 in the manual)

            - TODO: switch to a record representation, and keep location
          *)
    | Rinherit of core_type
          (* [ T ] *)

  (* Patterns *)

  and pattern              = Parsetree.pattern    =
      {
       ppat_desc: pattern_desc;
       ppat_loc: Location.t;
       ppat_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and pattern_desc              = Parsetree.pattern_desc    =
    | Ppat_any
          (* _ *)
    | Ppat_var of string loc
          (* x *)
    | Ppat_alias of pattern * string loc
          (* P as 'a *)
    | Ppat_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Ppat_interval of constant * constant
          (* 'a'..'z'

             Other forms of interval are recognized by the parser
             but rejected by the type-checker. *)
    | Ppat_tuple of pattern list
          (* (P1, ..., Pn)

             Invariant: n >= 2
          *)
    | Ppat_construct of Longident.t loc * pattern option
          (* C                None
             C P              Some P
             C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
           *)
    | Ppat_variant of label * pattern option
          (* `A             (None)
             `A P           (Some P)
           *)
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
          (* { l1=P1; ...; ln=Pn }     (flag = Closed)
             { l1=P1; ...; ln=Pn; _}   (flag = Open)

             Invariant: n > 0
           *)
    | Ppat_array of pattern list
          (* [| P1; ...; Pn |] *)
    | Ppat_or of pattern * pattern
          (* P1 | P2 *)
    | Ppat_constraint of pattern * core_type
          (* (P : T) *)
    | Ppat_type of Longident.t loc
          (* #tconst *)
    | Ppat_lazy of pattern
          (* lazy P *)
    | Ppat_unpack of string loc
          (* (module P)
             Note: (module P : S) is represented as
             Ppat_constraint(Ppat_unpack, Ptyp_package)
           *)
    | Ppat_exception of pattern
          (* exception P *)
    | Ppat_extension of extension
          (* [%id] *)

  (* Value expressions *)

  and expression              = Parsetree.expression    =
      {
       pexp_desc: expression_desc;
       pexp_loc: Location.t;
       pexp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and expression_desc              = Parsetree.expression_desc    =
    | Pexp_ident of Longident.t loc
          (* x
             M.x
           *)
    | Pexp_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Pexp_let of rec_flag * value_binding list * expression
          (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
           *)
    | Pexp_function of case list
          (* function P1 -> E1 | ... | Pn -> En *)
    | Pexp_fun of label * expression option * pattern * expression
          (* fun P -> E1                          (lab = "", None)
             fun ~l:P -> E1                       (lab = "l", None)
             fun ?l:P -> E1                       (lab = "?l", None)
             fun ?l:(P = E0) -> E1                (lab = "?l", Some E0)

             Notes:
             - If E0 is provided, lab must start with '?'.
             - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
             - "let f P = E" is represented using Pexp_fun.
           *)
    | Pexp_apply of expression * (label * expression) list
          (* E0 ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pexp_match of expression * case list
          (* match E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_try of expression * case list
          (* try E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_tuple of expression list
          (* (E1, ..., En)

             Invariant: n >= 2
          *)
    | Pexp_construct of Longident.t loc * expression option
          (* C                None
             C E              Some E
             C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
          *)
    | Pexp_variant of label * expression option
          (* `A             (None)
             `A E           (Some E)
           *)
    | Pexp_record of (Longident.t loc * expression) list * expression option
          (* { l1=P1; ...; ln=Pn }     (None)
             { E0 with l1=P1; ...; ln=Pn }   (Some E0)

             Invariant: n > 0
           *)
    | Pexp_field of expression * Longident.t loc
          (* E.l *)
    | Pexp_setfield of expression * Longident.t loc * expression
          (* E1.l <- E2 *)
    | Pexp_array of expression list
          (* [| E1; ...; En |] *)
    | Pexp_ifthenelse of expression * expression * expression option
          (* if E1 then E2 else E3 *)
    | Pexp_sequence of expression * expression
          (* E1; E2 *)
    | Pexp_while of expression * expression
          (* while E1 do E2 done *)
    | Pexp_for of
        pattern *  expression * expression * direction_flag * expression
          (* for i = E1 to E2 do E3 done      (flag = Upto)
             for i = E1 downto E2 do E3 done  (flag = Downto)
           *)
    | Pexp_constraint of expression * core_type
          (* (E : T) *)
    | Pexp_coerce of expression * core_type option * core_type
          (* (E :> T)        (None, T)
             (E : T0 :> T)   (Some T0, T)
           *)
    | Pexp_send of expression * string
          (*  E # m *)
    | Pexp_new of Longident.t loc
          (* new M.c *)
    | Pexp_setinstvar of string loc * expression
          (* x <- 2 *)
    | Pexp_override of (string loc * expression) list
          (* {< x1 = E1; ...; Xn = En >} *)
    | Pexp_letmodule of string loc * module_expr * expression
          (* let module M = ME in E *)
    | Pexp_assert of expression
          (* assert E
             Note: "assert false" is treated in a special way by the
             type-checker. *)
    | Pexp_lazy of expression
          (* lazy E *)
    | Pexp_poly of expression * core_type option
          (* Used for method bodies.

             Can only be used as the expression under Cfk_concrete
             for methods (not values). *)
    | Pexp_object of class_structure
          (* object ... end *)
    | Pexp_newtype of string * expression
          (* fun (type t) -> E *)
    | Pexp_pack of module_expr
          (* (module ME)

             (module ME : S) is represented as
             Pexp_constraint(Pexp_pack, Ptyp_package S) *)
    | Pexp_open of override_flag * Longident.t loc * expression
          (* let open M in E
             let! open M in E
          *)
    | Pexp_extension of extension
          (* [%id] *)

  and case              = Parsetree.case    =   (* (P -> E) or (P when E0 -> E) *)
      {
       pc_lhs: pattern;
       pc_guard: expression option;
       pc_rhs: expression;
      }

  (* Value descriptions *)

  and value_description              = Parsetree.value_description    =
      {
       pval_name: string loc;
       pval_type: core_type;
       pval_prim: string list;
       pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
       pval_loc: Location.t;
      }

  (*
    val x: T                            (prim = [])
    external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])

    Note: when used under Pstr_primitive, prim cannot be empty
  *)

  (* Type declarations *)

  and type_declaration              = Parsetree.type_declaration    =
      {
       ptype_name: string loc;
       ptype_params: (core_type * variance) list;
             (* ('a1,...'an) t; None represents  _*)
       ptype_cstrs: (core_type * core_type * Location.t) list;
             (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
       ptype_kind: type_kind;
       ptype_private: private_flag;   (* = private ... *)
       ptype_manifest: core_type option;  (* = T *)
       ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
       ptype_loc: Location.t;
      }

  (*
    type t                     (abstract, no manifest)
    type t = T0                (abstract, manifest=T0)
    type t = C of T | ...      (variant,  no manifest)
    type t = T0 = C of T | ... (variant,  manifest=T0)
    type t = {l: T; ...}       (record,   no manifest)
    type t = T0 = {l : T; ...} (record,   manifest=T0)
    type t = ..                (open,     no manifest)
  *)

  and type_kind              = Parsetree.type_kind    =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
          (* Invariant: non-empty list *)
    | Ptype_record of label_declaration list
          (* Invariant: non-empty list *)
    | Ptype_open

  and label_declaration              = Parsetree.label_declaration    =
      {
       pld_name: string loc;
       pld_mutable: mutable_flag;
       pld_type: core_type;
       pld_loc: Location.t;
       pld_attributes: attributes; (* l [@id1] [@id2] : T *)
      }

  (*  { ...; l: T; ... }            (mutable=Immutable)
      { ...; mutable l: T; ... }    (mutable=Mutable)

      Note: T can be a Ptyp_poly.
  *)

  and constructor_declaration              = Parsetree.constructor_declaration    =
      {
       pcd_name: string loc;
       pcd_args: core_type list;
       pcd_res: core_type option;
       pcd_loc: Location.t;
       pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }
  (*
    | C of T1 * ... * Tn     (res = None)
    | C: T0                  (args = [], res = Some T0)
    | C: T1 * ... * Tn -> T0 (res = Some T0)
  *)

  and type_extension              = Parsetree.type_extension    =
      {
       ptyext_path: Longident.t loc;
       ptyext_params: (core_type * variance) list;
       ptyext_constructors: extension_constructor list;
       ptyext_private: private_flag;
       ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
      }
  (*
    type t += ...
  *)

  and extension_constructor              = Parsetree.extension_constructor    =
      {
       pext_name: string loc;
       pext_kind : extension_constructor_kind;
       pext_loc : Location.t;
       pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }

  and extension_constructor_kind              = Parsetree.extension_constructor_kind    =
      Pext_decl of core_type list * core_type option
        (*
           | C of T1 * ... * Tn     ([T1; ...; Tn], None)
           | C: T0                  ([], Some T0)
           | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
         *)
    | Pext_rebind of Longident.t loc
        (*
           | C = D
         *)

  (** {2 Class language} *)

  (* Type expressions for the class language *)

  and class_type              = Parsetree.class_type    =
      {
       pcty_desc: class_type_desc;
       pcty_loc: Location.t;
       pcty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_type_desc              = Parsetree.class_type_desc    =
    | Pcty_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcty_signature of class_signature
          (* object ... end *)
    | Pcty_arrow of label * core_type * class_type
          (* T -> CT       (label = "")
             ~l:T -> CT    (label = "l")
             ?l:T -> CT    (label = "?l")
           *)
    | Pcty_extension of extension
          (* [%id] *)

  and class_signature              = Parsetree.class_signature    =
      {
       pcsig_self: core_type;
       pcsig_fields: class_type_field list;
      }
  (* object('selfpat) ... end
     object ... end             (self = Ptyp_any)
   *)

  and class_type_field              = Parsetree.class_type_field    =
      {
       pctf_desc: class_type_field_desc;
       pctf_loc: Location.t;
       pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_type_field_desc              = Parsetree.class_type_field_desc    =
    | Pctf_inherit of class_type
          (* inherit CT *)
    | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
          (* val x: T *)
    | Pctf_method  of (string * private_flag * virtual_flag * core_type)
          (* method x: T

             Note: T can be a Ptyp_poly.
           *)
    | Pctf_constraint  of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pctf_attribute of attribute
          (* [@@@id] *)
    | Pctf_extension of extension
          (* [%%id] *)

  and 'a class_infos              = 'a Parsetree.class_infos    =
      {
       pci_virt: virtual_flag;
       pci_params: (core_type * variance) list;
       pci_name: string loc;
       pci_expr: 'a;
       pci_loc: Location.t;
       pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
      }
  (* class c = ...
     class ['a1,...,'an] c = ...
     class virtual c = ...

     Also used for "class type" declaration.
  *)

  and class_description = class_type class_infos

  and class_type_declaration = class_type class_infos

  (* Value expressions for the class language *)

  and class_expr              = Parsetree.class_expr    =
      {
       pcl_desc: class_expr_desc;
       pcl_loc: Location.t;
       pcl_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_expr_desc              = Parsetree.class_expr_desc    =
    | Pcl_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcl_structure of class_structure
          (* object ... end *)
    | Pcl_fun of label * expression option * pattern * class_expr
          (* fun P -> CE                          (lab = "", None)
             fun ~l:P -> CE                       (lab = "l", None)
             fun ?l:P -> CE                       (lab = "?l", None)
             fun ?l:(P = E0) -> CE                (lab = "?l", Some E0)
           *)
    | Pcl_apply of class_expr * (label * expression) list
          (* CE ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pcl_let of rec_flag * value_binding list * class_expr
          (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
           *)
    | Pcl_constraint of class_expr * class_type
          (* (CE : CT) *)
    | Pcl_extension of extension
          (* [%id] *)

  and class_structure              = Parsetree.class_structure    =
      {
       pcstr_self: pattern;
       pcstr_fields: class_field list;
      }
  (* object(selfpat) ... end
     object ... end           (self = Ppat_any)
   *)

  and class_field              = Parsetree.class_field    =
      {
       pcf_desc: class_field_desc;
       pcf_loc: Location.t;
       pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_field_desc              = Parsetree.class_field_desc    =
    | Pcf_inherit of override_flag * class_expr * string option
          (* inherit CE
             inherit CE as x
             inherit! CE
             inherit! CE as x
           *)
    | Pcf_val of (string loc * mutable_flag * class_field_kind)
          (* val x = E
             val virtual x: T
           *)
    | Pcf_method of (string loc * private_flag * class_field_kind)
          (* method x = E            (E can be a Pexp_poly)
             method virtual x: T     (T can be a Ptyp_poly)
           *)
    | Pcf_constraint of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pcf_initializer of expression
          (* initializer E *)
    | Pcf_attribute of attribute
          (* [@@@id] *)
    | Pcf_extension of extension
          (* [%%id] *)

  and class_field_kind              = Parsetree.class_field_kind    =
    | Cfk_virtual of core_type
    | Cfk_concrete of override_flag * expression

  and class_declaration = class_expr class_infos

  (** {2 Module language} *)

  (* Type expressions for the module language *)

  and module_type              = Parsetree.module_type    =
      {
       pmty_desc: module_type_desc;
       pmty_loc: Location.t;
       pmty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_type_desc              = Parsetree.module_type_desc    =
    | Pmty_ident of Longident.t loc
          (* S *)
    | Pmty_signature of signature
          (* sig ... end *)
    | Pmty_functor of string loc * module_type option * module_type
          (* functor(X : MT1) -> MT2 *)
    | Pmty_with of module_type * with_constraint list
          (* MT with ... *)
    | Pmty_typeof of module_expr
          (* module type of ME *)
    | Pmty_extension of extension
          (* [%id] *)
    | Pmty_alias of Longident.t loc
          (* (module M) *)

  and signature = signature_item list

  and signature_item              = Parsetree.signature_item    =
      {
       psig_desc: signature_item_desc;
       psig_loc: Location.t;
      }

  and signature_item_desc              = Parsetree.signature_item_desc    =
    | Psig_value of value_description
          (*
            val x: T
            external x: T = "s1" ... "sn"
           *)
    | Psig_type of type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Psig_typext of type_extension
          (* type t1 += ... *)
    | Psig_exception of extension_constructor
          (* exception C of T *)
    | Psig_module of module_declaration
          (* module X : MT *)
    | Psig_recmodule of module_declaration list
          (* module rec X1 : MT1 and ... and Xn : MTn *)
    | Psig_modtype of module_type_declaration
          (* module type S = MT
             module type S *)
    | Psig_open of open_description
          (* open X *)
    | Psig_include of include_description
          (* include MT *)
    | Psig_class of class_description list
          (* class c1 : ... and ... and cn : ... *)
    | Psig_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Psig_attribute of attribute
          (* [@@@id] *)
    | Psig_extension of extension * attributes
          (* [%%id] *)

  and module_declaration              = Parsetree.module_declaration    =
      {
       pmd_name: string loc;
       pmd_type: module_type;
       pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmd_loc: Location.t;
      }
  (* S : MT *)

  and module_type_declaration              = Parsetree.module_type_declaration    =
      {
       pmtd_name: string loc;
       pmtd_type: module_type option;
       pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmtd_loc: Location.t;
      }
  (* S = MT
     S       (abstract module type declaration, pmtd_type = None)
  *)

  and open_description              = Parsetree.open_description    =
      {
       popen_lid: Longident.t loc;
       popen_override: override_flag;
       popen_loc: Location.t;
       popen_attributes: attributes;
      }
  (* open! X - popen_override = Override (silences the 'used identifier
                                shadowing' warning)
     open  X - popen_override = Fresh
   *)

  and 'a include_infos              = 'a Parsetree.include_infos    =
      {
       pincl_mod: 'a;
       pincl_loc: Location.t;
       pincl_attributes: attributes;
      }

  and include_description = module_type include_infos
  (* include MT *)

  and include_declaration = module_expr include_infos
  (* include ME *)

  and with_constraint              = Parsetree.with_constraint    =
    | Pwith_type of Longident.t loc * type_declaration
          (* with type X.t = ...

             Note: the last component of the longident must match
             the name of the type_declaration. *)
    | Pwith_module of Longident.t loc * Longident.t loc
          (* with module X.Y = Z *)
    | Pwith_typesubst of type_declaration
          (* with type t := ... *)
    | Pwith_modsubst of string loc * Longident.t loc
          (* with module X := Z *)

  (* Value expressions for the module language *)

  and module_expr              = Parsetree.module_expr    =
      {
       pmod_desc: module_expr_desc;
       pmod_loc: Location.t;
       pmod_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_expr_desc              = Parsetree.module_expr_desc    =
    | Pmod_ident of Longident.t loc
          (* X *)
    | Pmod_structure of structure
          (* struct ... end *)
    | Pmod_functor of string loc * module_type option * module_expr
          (* functor(X : MT1) -> ME *)
    | Pmod_apply of module_expr * module_expr
          (* ME1(ME2) *)
    | Pmod_constraint of module_expr * module_type
          (* (ME : MT) *)
    | Pmod_unpack of expression
          (* (val E) *)
    | Pmod_extension of extension
          (* [%id] *)

  and structure = structure_item list

  and structure_item              = Parsetree.structure_item    =
      {
       pstr_desc: structure_item_desc;
       pstr_loc: Location.t;
      }

  and structure_item_desc              = Parsetree.structure_item_desc    =
    | Pstr_eval of expression * attributes
          (* E *)
    | Pstr_value of rec_flag * value_binding list
          (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
           *)
    | Pstr_primitive of value_description
          (* external x: T = "s1" ... "sn" *)
    | Pstr_type of type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Pstr_typext of type_extension
          (* type t1 += ... *)
    | Pstr_exception of extension_constructor
          (* exception C of T
             exception C = M.X *)
    | Pstr_module of module_binding
          (* module X = ME *)
    | Pstr_recmodule of module_binding list
          (* module rec X1 = ME1 and ... and Xn = MEn *)
    | Pstr_modtype of module_type_declaration
          (* module type S = MT *)
    | Pstr_open of open_description
          (* open X *)
    | Pstr_class of class_declaration list
          (* class c1 = ... and ... and cn = ... *)
    | Pstr_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Pstr_include of include_declaration
          (* include ME *)
    | Pstr_attribute of attribute
          (* [@@@id] *)
    | Pstr_extension of extension * attributes
          (* [%%id] *)

  and value_binding              = Parsetree.value_binding    =
    {
      pvb_pat: pattern;
      pvb_expr: expression;
      pvb_attributes: attributes;
      pvb_loc: Location.t;
    }

  and module_binding              = Parsetree.module_binding    =
      {
       pmb_name: string loc;
       pmb_expr: module_expr;
       pmb_attributes: attributes;
       pmb_loc: Location.t;
      }
  (* X = ME *)

  (** {2 Toplevel} *)

  (* Toplevel phrases *)

  type toplevel_phrase              = Parsetree.toplevel_phrase    =
    | Ptop_def of structure
    | Ptop_dir of string * directive_argument
       (* #use, #load ... *)

  and directive_argument              = Parsetree.directive_argument    =
    | Pdir_none
    | Pdir_string of string
    | Pdir_int of int
    | Pdir_ident of Longident.t
    | Pdir_bool of bool
end

module Docstrings : sig
  (** {3 Docstrings} *)

  (** Documentation comments *)
  type docstring

  (** Create a docstring *)
  val docstring : string -> Location.t -> docstring

  (** Get the text of a docstring *)
  val docstring_body : docstring -> string

  (** Get the location of a docstring *)
  val docstring_loc : docstring -> Location.t

  (** {3 Items}

      The {!docs} type represents documentation attached to an item. *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  val empty_docs : docs

  val docs_attr : docstring -> Parsetree.attribute

  (** Convert item documentation to attributes and add them to an
      attribute list *)
  val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Fields and constructors}

      The {!info} type represents documentation attached to a field or
      constructor. *)

  type info = docstring option

  val empty_info : info

  val info_attr : docstring -> Parsetree.attribute

  (** Convert field info to attributes and add them to an
      attribute list *)
  val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Unattached comments}

      The {!text} type represents documentation which is not attached to
      anything. *)

  type text = docstring list

  val empty_text : text

  val text_attr : docstring -> Parsetree.attribute

  (** Convert text to attributes and add them to an attribute list *)
  val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

end = struct
  open Location

  (* Docstrings *)

  type docstring =
    { ds_body: string;
      ds_loc: Location.t; }

  (* Docstring constructors and destructors *)

  let docstring body loc =
    let ds =
      { ds_body = body;
        ds_loc = loc; }
    in
    ds

  let docstring_body ds = ds.ds_body

  let docstring_loc ds = ds.ds_loc

  (* Docstrings attached to items *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  let empty_docs = { docs_pre = None; docs_post = None }

  let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

  let docs_attr ds =
    let open Asttypes in
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Const_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (doc_loc, PStr [item])

  let add_docs_attrs docs attrs =
    let attrs =
      match docs.docs_pre with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> docs_attr ds :: attrs
    in
    let attrs =
      match docs.docs_post with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> attrs @ [docs_attr ds]
    in
    attrs

  (* Docstrings attached to constructors or fields *)

  type info = docstring option

  let empty_info = None

  let info_attr = docs_attr

  let add_info_attrs info attrs =
    match info with
    | None | Some {ds_body=""; _} -> attrs
    | Some ds -> attrs @ [info_attr ds]

  (* Docstrings not attached to a specific item *)

  type text = docstring list

  let empty_text = []

  let text_loc = {txt = "ocaml.text"; loc = Location.none}

  let text_attr ds =
    let open Asttypes in
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Const_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (text_loc, PStr [item])

  let add_text_attrs dsl attrs =
    let fdsl = List.filter (function {ds_body=""; _} -> false| _ ->true) dsl in
    (List.map text_attr fdsl) @ attrs

end

module Ast_helper : sig
  (** Helpers to produce Parsetree fragments *)

  open Parsetree
  open Asttypes
  open Docstrings

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  (** {2 Default locations} *)

  val default_loc: loc ref
      (** Default value for all optional location arguments. *)

  val with_default_loc: loc -> (unit -> 'a) -> 'a
      (** Set the [default_loc] within the scope of the execution
          of the provided function. *)

  (** {2 Core language} *)

  (** Type expressions *)
  module Typ :
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
      val attr: core_type -> attribute -> core_type

      val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
      val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
      val arrow: ?loc:loc -> ?attrs:attrs -> label -> core_type -> core_type
                 -> core_type
      val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val object_: ?loc:loc -> ?attrs:attrs ->
                    (string * attributes * core_type) list -> closed_flag ->
                    core_type
      val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
      val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
                   -> label list option -> core_type
      val poly: ?loc:loc -> ?attrs:attrs -> string list -> core_type -> core_type
      val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
                   -> core_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

      val force_poly: core_type -> core_type
    end

  (** Patterns *)
  module Pat:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
      val attr:pattern -> attribute -> pattern

      val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
      val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
      val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
      val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
      val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
      val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
                  -> pattern
      val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
      val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
      val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
      val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val unpack: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
    end

  (** Expressions *)
  module Exp:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
      val attr: expression -> attribute -> expression

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
                -> expression -> expression
      val fun_: ?loc:loc -> ?attrs:attrs -> label -> expression option -> pattern
                -> expression -> expression
      val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
      val apply: ?loc:loc -> ?attrs:attrs -> expression
                 -> (label * expression) list -> expression
      val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
                  -> expression
      val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
      val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                     -> expression
      val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
                   -> expression
      val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                  -> expression option -> expression
      val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
      val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                    -> expression
      val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                      -> expression option -> expression
      val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression
      val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
      val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
                -> direction_flag -> expression -> expression
      val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                  -> core_type -> expression
      val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                       -> expression
      val send: ?loc:loc -> ?attrs:attrs -> expression -> string -> expression
      val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
      val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                    -> expression
      val letmodule: ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression
                     -> expression
      val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option -> expression
      val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
      val newtype: ?loc:loc -> ?attrs:attrs -> string -> expression -> expression
      val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
      val open_: ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> expression -> expression
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression

      val case: pattern -> ?guard:expression -> expression -> case
    end

  (** Value declarations *)
  module Val:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        ?prim:string list -> str -> core_type -> value_description
    end

  (** Type declarations *)
  module Type:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?params:(core_type * variance) list -> ?cstrs:(core_type * core_type * loc) list ->
        ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
        type_declaration

      val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?args:core_type list -> ?res:core_type -> str -> constructor_declaration
      val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?mut:mutable_flag -> str -> core_type -> label_declaration
    end

  (** Type extensions *)
  module Te:
    sig
      val mk: ?attrs:attrs -> ?docs:docs ->
        ?params:(core_type * variance) list -> ?priv:private_flag ->
        lid -> extension_constructor list -> type_extension

      val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> extension_constructor_kind -> extension_constructor

      val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        ?args:core_type list -> ?res:core_type -> str -> extension_constructor
      val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> lid -> extension_constructor
    end

  (** {2 Module language} *)

  (** Module type expressions *)
  module Mty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
      val attr: module_type -> attribute -> module_type

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_type -> module_type
      val with_: ?loc:loc -> ?attrs:attrs -> module_type -> with_constraint list -> module_type
      val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
    end

  (** Module expressions *)
  module Mod:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
      val attr: module_expr -> attribute -> module_expr

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
      val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_expr -> module_expr
      val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr -> module_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type -> module_expr
      val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
    end

  (** Signature items *)
  module Sig:
    sig
      val mk: ?loc:loc -> signature_item_desc -> signature_item

      val value: ?loc:loc -> value_description -> signature_item
      val type_: ?loc:loc -> type_declaration list -> signature_item
      val type_extension: ?loc:loc -> type_extension -> signature_item
      val exception_: ?loc:loc -> extension_constructor -> signature_item
      val module_: ?loc:loc -> module_declaration -> signature_item
      val rec_module: ?loc:loc -> module_declaration list -> signature_item
      val modtype: ?loc:loc -> module_type_declaration -> signature_item
      val open_: ?loc:loc -> open_description -> signature_item
      val include_: ?loc:loc -> include_description -> signature_item
      val class_: ?loc:loc -> class_description list -> signature_item
      val class_type: ?loc:loc -> class_type_declaration list -> signature_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
      val attribute: ?loc:loc -> attribute -> signature_item
      val text: text -> signature_item list
    end

  (** Structure items *)
  module Str:
    sig
      val mk: ?loc:loc -> structure_item_desc -> structure_item

      val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
      val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
      val primitive: ?loc:loc -> value_description -> structure_item
      val type_: ?loc:loc -> type_declaration list -> structure_item
      val type_extension: ?loc:loc -> type_extension -> structure_item
      val exception_: ?loc:loc -> extension_constructor -> structure_item
      val module_: ?loc:loc -> module_binding -> structure_item
      val rec_module: ?loc:loc -> module_binding list -> structure_item
      val modtype: ?loc:loc -> module_type_declaration -> structure_item
      val open_: ?loc:loc -> open_description -> structure_item
      val class_: ?loc:loc -> class_declaration list -> structure_item
      val class_type: ?loc:loc -> class_type_declaration list -> structure_item
      val include_: ?loc:loc -> include_declaration -> structure_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
      val attribute: ?loc:loc -> attribute -> structure_item
      val text: text -> structure_item list
    end

  (** Module declarations *)
  module Md:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_type -> module_declaration
    end

  (** Module type declarations *)
  module Mtd:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?typ:module_type -> str -> module_type_declaration
    end

  (** Module bindings *)
  module Mb:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_expr -> module_binding
    end

  (* Opens *)
  module Opn:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
        ?override:override_flag -> lid -> open_description
    end

  (* Includes *)
  module Incl:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
    end

  (** Value bindings *)

  module Vb:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        pattern -> expression -> value_binding
    end


  (** {2 Class language} *)

  (** Class type expressions *)
  module Cty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
      val attr: class_type -> attribute -> class_type

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
      val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
      val arrow: ?loc:loc -> ?attrs:attrs -> label -> core_type -> class_type -> class_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    end

  (** Class type fields *)
  module Ctf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        class_type_field_desc -> class_type_field
      val attr: class_type_field -> attribute -> class_type_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
      val val_: ?loc:loc -> ?attrs:attrs -> string -> mutable_flag -> virtual_flag -> core_type -> class_type_field
      val method_: ?loc:loc -> ?attrs:attrs -> string -> private_flag -> virtual_flag -> core_type -> class_type_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_type_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
      val attribute: ?loc:loc -> attribute -> class_type_field
      val text: text -> class_type_field list
    end

  (** Class expressions *)
  module Cl:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
      val attr: class_expr -> attribute -> class_expr

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
      val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
      val fun_: ?loc:loc -> ?attrs:attrs -> label -> expression option -> pattern -> class_expr -> class_expr
      val apply: ?loc:loc -> ?attrs:attrs -> class_expr -> (label * expression) list -> class_expr
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list -> class_expr -> class_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type -> class_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    end

  (** Class fields *)
  module Cf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc -> class_field
      val attr: class_field -> attribute -> class_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr -> string option -> class_field
      val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag -> class_field_kind -> class_field
      val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag -> class_field_kind -> class_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_field
      val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
      val attribute: ?loc:loc -> attribute -> class_field
      val text: text -> class_field list

      val virtual_: core_type -> class_field_kind
      val concrete: override_flag -> expression -> class_field_kind

    end

  (** Classes *)
  module Ci:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?virt:virtual_flag -> ?params:(core_type * variance) list ->
        str -> 'a -> 'a class_infos
    end

  (** Class signatures *)
  module Csig:
    sig
      val mk: core_type -> class_type_field list -> class_signature
    end

  (** Class structures *)
  module Cstr:
    sig
      val mk: pattern -> class_field list -> class_structure
    end

end = struct
  (** Helpers to produce Parsetree fragments *)

  open Asttypes
  open Parsetree
  open Docstrings

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  let default_loc = ref Location.none

  let with_default_loc l f =
    let old = !default_loc in
    default_loc := l;
    try let r = f () in default_loc := old; r
    with exn -> default_loc := old; raise exn

  module Typ = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
    let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
    let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
    let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
    let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
    let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
    let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

    let force_poly t =
      match t.ptyp_desc with
      | Ptyp_poly _ -> t
      | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)
  end

  module Pat = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
    let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
    let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
    let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
    let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
    let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
    let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
    let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
    let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
  end

  module Exp = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
    let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
    let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
    let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
    let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
    let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
    let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
    let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
    let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
    let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
    let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
    let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
    let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
    let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
    let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
    let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
    let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
    let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
    let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
    let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
    let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
    let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
    let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
    let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)

    let case lhs ?guard rhs =
      {
       pc_lhs = lhs;
       pc_guard = guard;
       pc_rhs = rhs;
      }
  end

  module Mty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
    let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
    let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
    let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
    let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
    let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
  end

  module Mod = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
    let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

    let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
    let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
    let functor_ ?loc ?attrs arg arg_ty body =
      mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
    let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
    let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
    let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
  end

  module Sig = struct
    let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

    let value ?loc a = mk ?loc (Psig_value a)
    let type_ ?loc a = mk ?loc (Psig_type a)
    let type_extension ?loc a = mk ?loc (Psig_typext a)
    let exception_ ?loc a = mk ?loc (Psig_exception a)
    let module_ ?loc a = mk ?loc (Psig_module a)
    let rec_module ?loc a = mk ?loc (Psig_recmodule a)
    let modtype ?loc a = mk ?loc (Psig_modtype a)
    let open_ ?loc a = mk ?loc (Psig_open a)
    let include_ ?loc a = mk ?loc (Psig_include a)
    let class_ ?loc a = mk ?loc (Psig_class a)
    let class_type ?loc a = mk ?loc (Psig_class_type a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Psig_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt
  end

  module Str = struct
    let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

    let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
    let value ?loc a b = mk ?loc (Pstr_value (a, b))
    let primitive ?loc a = mk ?loc (Pstr_primitive a)
    let type_ ?loc a = mk ?loc (Pstr_type a)
    let type_extension ?loc a = mk ?loc (Pstr_typext a)
    let exception_ ?loc a = mk ?loc (Pstr_exception a)
    let module_ ?loc a = mk ?loc (Pstr_module a)
    let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
    let modtype ?loc a = mk ?loc (Pstr_modtype a)
    let open_ ?loc a = mk ?loc (Pstr_open a)
    let class_ ?loc a = mk ?loc (Pstr_class a)
    let class_type ?loc a = mk ?loc (Pstr_class_type a)
    let include_ ?loc a = mk ?loc (Pstr_include a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Pstr_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt
  end

  module Cl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcl_desc = d;
       pcl_loc = loc;
       pcl_attributes = attrs;
      }
    let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
    let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
  end

  module Cty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcty_desc = d;
       pcty_loc = loc;
       pcty_attributes = attrs;
      }
    let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
  end

  module Ctf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
             ?(docs = empty_docs) d =
      {
       pctf_desc = d;
       pctf_loc = loc;
       pctf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
    let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
    let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
    let attribute ?loc a = mk ?loc (Pctf_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt

    let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

  end

  module Cf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) d =
      {
       pcf_desc = d;
       pcf_loc = loc;
       pcf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
    let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
    let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
    let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
    let attribute ?loc a = mk ?loc (Pcf_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt

    let virtual_ ct = Cfk_virtual ct
    let concrete o e = Cfk_concrete (o, e)

    let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  end

  module Val = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(prim = []) name typ =
      {
       pval_name = name;
       pval_type = typ;
       pval_attributes = add_docs_attrs docs attrs;
       pval_loc = loc;
       pval_prim = prim;
      }
  end

  module Md = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name typ =
      {
       pmd_name = name;
       pmd_type = typ;
       pmd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmd_loc = loc;
      }
  end

  module Mtd = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) ?typ name =
      {
       pmtd_name = name;
       pmtd_type = typ;
       pmtd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmtd_loc = loc;
      }
  end

  module Mb = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name expr =
      {
       pmb_name = name;
       pmb_expr = expr;
       pmb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmb_loc = loc;
      }
  end

  module Opn = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(override = Fresh) lid =
      {
       popen_lid = lid;
       popen_override = override;
       popen_loc = loc;
       popen_attributes = add_docs_attrs docs attrs;
      }
  end

  module Incl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
      {
       pincl_mod = mexpr;
       pincl_loc = loc;
       pincl_attributes = add_docs_attrs docs attrs;
      }

  end

  module Vb = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(text = []) pat expr =
      {
       pvb_pat = pat;
       pvb_expr = expr;
       pvb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pvb_loc = loc;
      }
  end

  module Ci = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
          ?(virt = Concrete) ?(params = []) name expr =
      {
       pci_virt = virt;
       pci_params = params;
       pci_name = name;
       pci_expr = expr;
       pci_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pci_loc = loc;
      }
  end

  module Type = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
        ?(params = [])
        ?(cstrs = [])
        ?(kind = Ptype_abstract)
        ?(priv = Public)
        ?manifest
        name =
      {
       ptype_name = name;
       ptype_params = params;
       ptype_cstrs = cstrs;
       ptype_kind = kind;
       ptype_private = priv;
       ptype_manifest = manifest;
       ptype_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       ptype_loc = loc;
      }

    let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(args = []) ?res name =
      {
       pcd_name = name;
       pcd_args = args;
       pcd_res = res;
       pcd_loc = loc;
       pcd_attributes = add_info_attrs info attrs;
      }

    let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(mut = Immutable) name typ =
      {
       pld_name = name;
       pld_mutable = mut;
       pld_type = typ;
       pld_loc = loc;
       pld_attributes = add_info_attrs info attrs;
      }

  end

  (** Type extensions *)
  module Te = struct
    let mk ?(attrs = []) ?(docs = empty_docs)
          ?(params = []) ?(priv = Public) path constructors =
      {
       ptyext_path = path;
       ptyext_params = params;
       ptyext_constructors = constructors;
       ptyext_private = priv;
       ptyext_attributes = add_docs_attrs docs attrs;
      }

    let constructor ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name kind =
      {
       pext_name = name;
       pext_kind = kind;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let decl ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) ?(args = []) ?res name =
      {
       pext_name = name;
       pext_kind = Pext_decl(args, res);
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let rebind ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name lid =
      {
       pext_name = name;
       pext_kind = Pext_rebind lid;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

  end

  module Csig = struct
    let mk self fields =
      {
       pcsig_self = self;
       pcsig_fields = fields;
      }
  end

  module Cstr = struct
    let mk self fields =
      {
       pcstr_self = self;
       pcstr_fields = fields;
      }
  end

end

module Ast_mapper : sig
  (** The interface of a -ppx rewriter

    A -ppx rewriter is a program that accepts a serialized abstract syntax
    tree and outputs another, possibly modified, abstract syntax tree.
    This module encapsulates the interface between the compiler and
    the -ppx rewriters, handling such details as the serialization format,
    forwarding of command-line flags, and storing state.

    {!mapper} allows to implement AST rewriting using open recursion.
    A typical mapper would be based on {!default_mapper}, a deep
    identity mapper, and will fall back on it for handling the syntax it
    does not modify. For example:

    {[
  open Asttypes
  open Parsetree
  open Ast_mapper

  let test_mapper argv =
    { default_mapper with
      expr = fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
          Ast_helper.Exp.constant (Const_int 42)
        | other -> default_mapper.expr mapper other; }

  let () =
    register "ppx_test" test_mapper]}

    This -ppx rewriter, which replaces [[%test]] in expressions with
    the constant [42], can be compiled using
    [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

    *)

  open Parsetree

  (** {2 A generic Parsetree mapper} *)

  type mapper              = Ast_mapper.mapper   = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }
  (** A mapper record implements one "method" per syntactic category,
      using an open recursion style: each method takes as its first
      argument the mapper to be applied to children in the syntax
      tree. *)

  val default_mapper: mapper
  (** A default mapper, which implements a "deep identity" mapping. *)

  (** {2 Convenience functions to write mappers} *)

  val map_opt: ('a -> 'b) -> 'a option -> 'b option

  val extension_of_error: Location.error -> extension
  (** Encode an error into an 'ocaml.error' extension node which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the error. *)

  val attribute_of_warning: Location.t -> string -> attribute
  (** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the warning. *)

end = struct
  (* A generic Parsetree mapping class *)

  (*
  [@@@ocaml.warning "+9"]
    (* Ensure that record patterns don't miss any field. *)
  *)


  open Parsetree
  open Ast_helper
  open Location

  type mapper              = Ast_mapper.mapper   = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }

  let map_fst f (x, y) = (f x, y)
  let map_snd f (x, y) = (x, f y)
  let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
  let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
  let map_opt f = function None -> None | Some x -> Some (f x)

  let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

  module T = struct
    (* Type expressions for the core language *)

    let row_field sub = function
      | Rtag (l, attrs, b, tl) ->
          Rtag (l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
      | Rinherit t -> Rinherit (sub.typ sub t)

    let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
      let open Typ in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ptyp_any -> any ~loc ~attrs ()
      | Ptyp_var s -> var ~loc ~attrs s
      | Ptyp_arrow (lab, t1, t2) ->
          arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
      | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
      | Ptyp_constr (lid, tl) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_object (l, o) ->
          let f (s, a, t) = (s, sub.attributes sub a, sub.typ sub t) in
          object_ ~loc ~attrs (List.map f l) o
      | Ptyp_class (lid, tl) ->
          class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
      | Ptyp_variant (rl, b, ll) ->
          variant ~loc ~attrs (List.map (row_field sub) rl) b ll
      | Ptyp_poly (sl, t) -> poly ~loc ~attrs sl (sub.typ sub t)
      | Ptyp_package (lid, l) ->
          package ~loc ~attrs (map_loc sub lid)
            (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
      | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_type_declaration sub
        {ptype_name; ptype_params; ptype_cstrs;
         ptype_kind;
         ptype_private;
         ptype_manifest;
         ptype_attributes;
         ptype_loc} =
      Type.mk (map_loc sub ptype_name)
        ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
        ~priv:ptype_private
        ~cstrs:(List.map
                  (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                  ptype_cstrs)
        ~kind:(sub.type_kind sub ptype_kind)
        ?manifest:(map_opt (sub.typ sub) ptype_manifest)
        ~loc:(sub.location sub ptype_loc)
        ~attrs:(sub.attributes sub ptype_attributes)

    let map_type_kind sub = function
      | Ptype_abstract -> Ptype_abstract
      | Ptype_variant l ->
          Ptype_variant (List.map (sub.constructor_declaration sub) l)
      | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
      | Ptype_open -> Ptype_open

    let map_type_extension sub
        {ptyext_path; ptyext_params;
         ptyext_constructors;
         ptyext_private;
         ptyext_attributes} =
      Te.mk
        (map_loc sub ptyext_path)
        (List.map (sub.extension_constructor sub) ptyext_constructors)
        ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
        ~priv:ptyext_private
        ~attrs:(sub.attributes sub ptyext_attributes)

    let map_extension_constructor_kind sub = function
        Pext_decl(ctl, cto) ->
          Pext_decl(List.map (sub.typ sub) ctl, map_opt (sub.typ sub) cto)
      | Pext_rebind li ->
          Pext_rebind (map_loc sub li)

    let map_extension_constructor sub
        {pext_name;
         pext_kind;
         pext_loc;
         pext_attributes} =
      Te.constructor
        (map_loc sub pext_name)
        (map_extension_constructor_kind sub pext_kind)
        ~loc:(sub.location sub pext_loc)
        ~attrs:(sub.attributes sub pext_attributes)

  end

  module CT = struct
    (* Type expressions for the class language *)

    let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
      let open Cty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcty_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
      | Pcty_arrow (lab, t, ct) ->
          arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
      | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
      =
      let open Ctf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
      | Pctf_val (s, m, v, t) -> val_ ~loc ~attrs s m v (sub.typ sub t)
      | Pctf_method (s, p, v, t) -> method_ ~loc ~attrs s p v (sub.typ sub t)
      | Pctf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_signature sub {pcsig_self; pcsig_fields} =
      Csig.mk
        (sub.typ sub pcsig_self)
        (List.map (sub.class_type_field sub) pcsig_fields)
  end

  module MT = struct
    (* Type expressions for the module language *)

    let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
      let open Mty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
      | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
      | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
      | Pmty_functor (s, mt1, mt2) ->
          functor_ ~loc ~attrs (map_loc sub s)
            (Misc.may_map (sub.module_type sub) mt1)
            (sub.module_type sub mt2)
      | Pmty_with (mt, l) ->
          with_ ~loc ~attrs (sub.module_type sub mt)
            (List.map (sub.with_constraint sub) l)
      | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
      | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_with_constraint sub = function
      | Pwith_type (lid, d) ->
          Pwith_type (map_loc sub lid, sub.type_declaration sub d)
      | Pwith_module (lid, lid2) ->
          Pwith_module (map_loc sub lid, map_loc sub lid2)
      | Pwith_typesubst d -> Pwith_typesubst (sub.type_declaration sub d)
      | Pwith_modsubst (s, lid) ->
          Pwith_modsubst (map_loc sub s, map_loc sub lid)

    let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
      let open Sig in
      let loc = sub.location sub loc in
      match desc with
      | Psig_value vd -> value ~loc (sub.value_description sub vd)
      | Psig_type l -> type_ ~loc (List.map (sub.type_declaration sub) l)
      | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
      | Psig_recmodule l ->
          rec_module ~loc (List.map (sub.module_declaration sub) l)
      | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Psig_open x -> open_ ~loc (sub.open_description sub x)
      | Psig_include x -> include_ ~loc (sub.include_description sub x)
      | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
      | Psig_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Psig_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
  end


  module M = struct
    (* Value expressions for the module language *)

    let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
      let open Mod in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
      | Pmod_functor (arg, arg_ty, body) ->
          functor_ ~loc ~attrs (map_loc sub arg)
            (Misc.may_map (sub.module_type sub) arg_ty)
            (sub.module_expr sub body)
      | Pmod_apply (m1, m2) ->
          apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
      | Pmod_constraint (m, mty) ->
          constraint_ ~loc ~attrs (sub.module_expr sub m)
                      (sub.module_type sub mty)
      | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
      | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
      let open Str in
      let loc = sub.location sub loc in
      match desc with
      | Pstr_eval (x, attrs) ->
          eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
      | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
      | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
      | Pstr_type l -> type_ ~loc (List.map (sub.type_declaration sub) l)
      | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
      | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
      | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Pstr_open x -> open_ ~loc (sub.open_description sub x)
      | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
      | Pstr_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
      | Pstr_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
  end

  module E = struct
    (* Value expressions for the core language *)

    let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
      let open Exp in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pexp_constant x -> constant ~loc ~attrs x
      | Pexp_let (r, vbs, e) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.expr sub e)
      | Pexp_fun (lab, def, p, e) ->
          fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
            (sub.expr sub e)
      | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
      | Pexp_apply (e, l) ->
          apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
      | Pexp_match (e, pel) ->
          match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_construct (lid, arg) ->
          construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
      | Pexp_variant (lab, eo) ->
          variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
      | Pexp_record (l, eo) ->
          record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
            (map_opt (sub.expr sub) eo)
      | Pexp_field (e, lid) ->
          field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
      | Pexp_setfield (e1, lid, e2) ->
          setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
            (sub.expr sub e2)
      | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_ifthenelse (e1, e2, e3) ->
          ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
            (map_opt (sub.expr sub) e3)
      | Pexp_sequence (e1, e2) ->
          sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_while (e1, e2) ->
          while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_for (p, e1, e2, d, e3) ->
          for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
            (sub.expr sub e3)
      | Pexp_coerce (e, t1, t2) ->
          coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
            (sub.typ sub t2)
      | Pexp_constraint (e, t) ->
          constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
      | Pexp_send (e, s) -> send ~loc ~attrs (sub.expr sub e) s
      | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
      | Pexp_setinstvar (s, e) ->
          setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
      | Pexp_override sel ->
          override ~loc ~attrs
            (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
      | Pexp_letmodule (s, me, e) ->
          letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
            (sub.expr sub e)
      | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
      | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
      | Pexp_poly (e, t) ->
          poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
      | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
      | Pexp_newtype (s, e) -> newtype ~loc ~attrs s (sub.expr sub e)
      | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
      | Pexp_open (ovf, lid, e) ->
          open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
      | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
  end

  module P = struct
    (* Patterns *)

    let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
      let open Pat in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ppat_any -> any ~loc ~attrs ()
      | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
      | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
      | Ppat_constant c -> constant ~loc ~attrs c
      | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
      | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_construct (l, p) ->
          construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
      | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
      | Ppat_record (lpl, cf) ->
          record ~loc ~attrs
                 (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
      | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
      | Ppat_constraint (p, t) ->
          constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
      | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
      | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
      | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
      | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
      | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
  end

  module CE = struct
    (* Value expressions for the class language *)

    let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
      let open Cl in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcl_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcl_structure s ->
          structure ~loc ~attrs (sub.class_structure sub s)
      | Pcl_fun (lab, e, p, ce) ->
          fun_ ~loc ~attrs lab
            (map_opt (sub.expr sub) e)
            (sub.pat sub p)
            (sub.class_expr sub ce)
      | Pcl_apply (ce, l) ->
          apply ~loc ~attrs (sub.class_expr sub ce)
            (List.map (map_snd (sub.expr sub)) l)
      | Pcl_let (r, vbs, ce) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.class_expr sub ce)
      | Pcl_constraint (ce, ct) ->
          constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
      | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_kind sub = function
      | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
      | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

    let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
      let open Cf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcf_inherit (o, ce, s) -> inherit_ ~loc ~attrs o (sub.class_expr sub ce) s
      | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
      | Pcf_method (s, p, k) ->
          method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
      | Pcf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
      | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure sub {pcstr_self; pcstr_fields} =
      {
        pcstr_self = sub.pat sub pcstr_self;
        pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
      }

    let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                           pci_loc; pci_attributes} =
      Ci.mk
       ~virt:pci_virt
       ~params:(List.map (map_fst (sub.typ sub)) pl)
        (map_loc sub pci_name)
        (f pci_expr)
        ~loc:(sub.location sub pci_loc)
        ~attrs:(sub.attributes sub pci_attributes)
  end

  (* Now, a generic AST mapper, to be extended to cover all kinds and
     cases of the OCaml grammar.  The default behavior of the mapper is
     the identity. *)

  let default_mapper =
    {
      structure = (fun this l -> List.map (this.structure_item this) l);
      structure_item = M.map_structure_item;
      module_expr = M.map;
      signature = (fun this l -> List.map (this.signature_item this) l);
      signature_item = MT.map_signature_item;
      module_type = MT.map;
      with_constraint = MT.map_with_constraint;
      class_declaration =
        (fun this -> CE.class_infos this (this.class_expr this));
      class_expr = CE.map;
      class_field = CE.map_field;
      class_structure = CE.map_structure;
      class_type = CT.map;
      class_type_field = CT.map_field;
      class_signature = CT.map_signature;
      class_type_declaration =
        (fun this -> CE.class_infos this (this.class_type this));
      class_description =
        (fun this -> CE.class_infos this (this.class_type this));
      type_declaration = T.map_type_declaration;
      type_kind = T.map_type_kind;
      typ = T.map;
      type_extension = T.map_type_extension;
      extension_constructor = T.map_extension_constructor;
      value_description =
        (fun this {pval_name; pval_type; pval_prim; pval_loc;
                   pval_attributes} ->
          Val.mk
            (map_loc this pval_name)
            (this.typ this pval_type)
            ~attrs:(this.attributes this pval_attributes)
            ~loc:(this.location this pval_loc)
            ~prim:pval_prim
        );

      pat = P.map;
      expr = E.map;

      module_declaration =
        (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
           Md.mk
             (map_loc this pmd_name)
             (this.module_type this pmd_type)
             ~attrs:(this.attributes this pmd_attributes)
             ~loc:(this.location this pmd_loc)
        );

      module_type_declaration =
        (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
           Mtd.mk
             (map_loc this pmtd_name)
             ?typ:(map_opt (this.module_type this) pmtd_type)
             ~attrs:(this.attributes this pmtd_attributes)
             ~loc:(this.location this pmtd_loc)
        );

      module_binding =
        (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
           Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
             ~attrs:(this.attributes this pmb_attributes)
             ~loc:(this.location this pmb_loc)
        );


      open_description =
        (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
           Opn.mk (map_loc this popen_lid)
             ~override:popen_override
             ~loc:(this.location this popen_loc)
             ~attrs:(this.attributes this popen_attributes)
        );


      include_description =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_type this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );

      include_declaration =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_expr this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );


      value_binding =
        (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
           Vb.mk
             (this.pat this pvb_pat)
             (this.expr this pvb_expr)
             ~loc:(this.location this pvb_loc)
             ~attrs:(this.attributes this pvb_attributes)
        );


      constructor_declaration =
        (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
          Type.constructor
            (map_loc this pcd_name)
            ~args:(List.map (this.typ this) pcd_args)
            ?res:(map_opt (this.typ this) pcd_res)
            ~loc:(this.location this pcd_loc)
            ~attrs:(this.attributes this pcd_attributes)
        );

      label_declaration =
        (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
           Type.field
             (map_loc this pld_name)
             (this.typ this pld_type)
             ~mut:pld_mutable
             ~loc:(this.location this pld_loc)
             ~attrs:(this.attributes this pld_attributes)
        );

      cases = (fun this l -> List.map (this.case this) l);
      case =
        (fun this {pc_lhs; pc_guard; pc_rhs} ->
           {
             pc_lhs = this.pat this pc_lhs;
             pc_guard = map_opt (this.expr this) pc_guard;
             pc_rhs = this.expr this pc_rhs;
           }
        );



      location = (fun _this l -> l);

      extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attributes = (fun this l -> List.map (this.attribute this) l);
      payload =
        (fun this -> function
           | PStr x -> PStr (this.structure this x)
           | PTyp x -> PTyp (this.typ this x)
           | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
        );
    }

  let rec extension_of_error {loc; msg; if_highlight; sub} =
    { loc; txt = "ocaml.error" },
    PStr ([Str.eval (Exp.constant (Asttypes.Const_string (msg, None)));
           Str.eval (Exp.constant (Asttypes.Const_string (if_highlight, None)))] @
          (List.map (fun ext -> Str.extension (extension_of_error ext)) sub))

  let attribute_of_warning loc s =
    { loc; txt = "ocaml.ppwarning" },
    PStr ([Str.eval ~loc (Exp.constant (Asttypes.Const_string (s, None)))])
end

module Outcometree = struct
  (* Module [Outcometree]: results displayed by the toplevel *)

  (* These types represent messages that the toplevel displays as normal
     results or errors. The real displaying is customisable using the hooks:
        [Toploop.print_out_value]
        [Toploop.print_out_type]
        [Toploop.print_out_sig_item]
        [Toploop.print_out_phrase] *)

  type out_ident              = Outcometree.out_ident    =
    | Oide_apply of out_ident * out_ident
    | Oide_dot of out_ident * string
    | Oide_ident of string

  type out_value              = Outcometree.out_value    =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option

  type out_type              = Outcometree.out_type    =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type * string
    | Otyp_arrow of string * out_type * out_type
    | Otyp_class of bool * out_ident * out_type list
    | Otyp_constr of out_ident * out_type list
    | Otyp_manifest of out_type * out_type
    | Otyp_object of (string * out_type) list * bool option
    | Otyp_record of (string * bool * out_type) list
    | Otyp_stuff of string
    | Otyp_sum of (string * out_type list * out_type option) list
    | Otyp_tuple of out_type list
    | Otyp_var of bool * string
    | Otyp_variant of
        bool * out_variant * bool * (string list) option
    | Otyp_poly of string list * out_type
    | Otyp_module of string * string list * out_type list

  and out_variant              = Outcometree.out_variant    =
    | Ovar_fields of (string * bool * out_type list) list
    | Ovar_name of out_ident * out_type list

  type out_class_type              = Outcometree.out_class_type    =
    | Octy_constr of out_ident * out_type list
    | Octy_arrow of string * out_type * out_class_type
    | Octy_signature of out_type option * out_class_sig_item list
  and out_class_sig_item               = Outcometree.out_class_sig_item    =
    | Ocsg_constraint of out_type * out_type
    | Ocsg_method of string * bool * bool * out_type
    | Ocsg_value of string * bool * bool * out_type

  type out_module_type              = Outcometree.out_module_type    =
    | Omty_abstract
    | Omty_functor of string * out_module_type option * out_module_type
    | Omty_ident of out_ident
    | Omty_signature of out_sig_item list
    | Omty_alias of out_ident
  and out_sig_item               = Outcometree.out_sig_item    =
    | Osig_class of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_class_type of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_typext of out_extension_constructor * out_ext_status
    | Osig_modtype of string * out_module_type
    | Osig_module of string * out_module_type * out_rec_status
    | Osig_type of out_type_decl * out_rec_status
    | Osig_value of string * out_type * string list
  and out_type_decl               = Outcometree.out_type_decl    =
    { otype_name: string;
      otype_params: (string * (bool * bool)) list;
      otype_type: out_type;
      otype_private: Asttypes.private_flag;
      otype_cstrs: (out_type * out_type) list }
  and out_extension_constructor              = Outcometree.out_extension_constructor    =
    { oext_name: string;
      oext_type_name: string;
      oext_type_params: string list;
      oext_args: out_type list;
      oext_ret_type: out_type option;
      oext_private: Asttypes.private_flag }
  and out_type_extension              = Outcometree.out_type_extension    =
    { otyext_name: string;
      otyext_params: string list;
      otyext_constructors: (string * out_type list * out_type option) list;
      otyext_private: Asttypes.private_flag }
  and out_rec_status              = Outcometree.out_rec_status    =
    | Orec_not
    | Orec_first
    | Orec_next
  and out_ext_status              = Outcometree.out_ext_status   =
    | Oext_first
    | Oext_next
    | Oext_exception

  type out_phrase              = Outcometree.out_phrase    =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)

end

module Config = struct
  let ast_impl_magic_number = "Caml1999M016"
  let ast_intf_magic_number = "Caml1999N015"
end

let map_signature mapper = mapper.Ast_mapper.signature mapper
let map_structure mapper = mapper.Ast_mapper.structure mapper

let failing_mapper =
  let fail _ _ =
    invalid_arg "failing_mapper: this mapper function should never get called"
  in
  {
    Ast_mapper.
    structure               = fail;
    structure_item          = fail;
    module_expr             = fail;
    signature               = fail;
    signature_item          = fail;
    module_type             = fail;
    with_constraint         = fail;
    class_declaration       = fail;
    class_expr              = fail;
    class_field             = fail;
    class_structure         = fail;
    class_type              = fail;
    class_type_field        = fail;
    class_signature         = fail;
    class_type_declaration  = fail;
    class_description       = fail;
    type_declaration        = fail;
    type_kind               = fail;
    typ                     = fail;
    type_extension          = fail;
    extension_constructor   = fail;
    value_description       = fail;
    pat                     = fail;
    expr                    = fail;
    module_declaration      = fail;
    module_type_declaration = fail;
    module_binding          = fail;
    open_description        = fail;
    include_description     = fail;
    include_declaration     = fail;
    value_binding           = fail;
    constructor_declaration = fail;
    label_declaration       = fail;
    cases                   = fail;
    case                    = fail;
    location                = fail;
    extension               = fail;
    attribute               = fail;
    attributes              = fail;
    payload                 = fail;
  }

let make_top_mapper ~signature ~structure =
  {failing_mapper with Ast_mapper.
                    signature = (fun _ x -> signature x);
                    structure = (fun _ x -> structure x) }

end
module Ast_403
= struct
#1 "ast_403.ml"
# 1 "src/ast_403.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*            Jérémie Dimino and Leo White, Jane Street Europe            *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                         Alain Frisch, LexiFi                           *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Location = Location
module Longident = Longident

module Asttypes = struct
  (* Auxiliary a.s.t. types used by parsetree and typedtree. *)

  type constant (*IF_CURRENT = Asttypes.constant *) =
      Const_int of int
    | Const_char of char
    | Const_string of string * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint

  type rec_flag (*IF_CURRENT = Asttypes.rec_flag *) = Nonrecursive | Recursive

  type direction_flag (*IF_CURRENT = Asttypes.direction_flag *) = Upto | Downto

  (* Order matters, used in polymorphic comparison *)
  type private_flag (*IF_CURRENT = Asttypes.private_flag *) = Private | Public

  type mutable_flag (*IF_CURRENT = Asttypes.mutable_flag *) = Immutable | Mutable

  type virtual_flag (*IF_CURRENT = Asttypes.virtual_flag *) = Virtual | Concrete

  type override_flag (*IF_CURRENT = Asttypes.override_flag *) = Override | Fresh

  type closed_flag (*IF_CURRENT = Asttypes.closed_flag *) = Closed | Open

  type label = string

  type arg_label (*IF_CURRENT = Asttypes.arg_label *) =
      Nolabel
    | Labelled of string (*  label:T -> ... *)
    | Optional of string (* ?label:T -> ... *)

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }


  type variance (*IF_CURRENT = Asttypes.variance *) =
    | Covariant
    | Contravariant
    | Invariant
end

module Parsetree = struct
  (** Abstract syntax tree produced by parsing *)

  open Asttypes

  type constant (*IF_CURRENT = Parsetree.constant *) =
      Pconst_integer of string * char option
    (* 3 3l 3L 3n

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
    *)
    | Pconst_char of char
    (* 'c' *)
    | Pconst_string of string * string option
    (* "constant"
       {delim|other constant|delim}
    *)
    | Pconst_float of string * char option
    (* 3.4 2e5 1.4e-4

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes are rejected by the typechecker.
    *)

  (** {2 Extension points} *)

  type attribute = string loc * payload
         (* [@id ARG]
            [@@id ARG]

            Metadata containers passed around within the AST.
            The compiler ignores unknown attributes.
         *)

  and extension = string loc * payload
        (* [%id ARG]
           [%%id ARG]

           Sub-language placeholder -- rejected by the typechecker.
        *)

  and attributes = attribute list

  and payload (*IF_CURRENT = Parsetree.payload *) =
    | PStr of structure
    | PSig of signature (* : SIG *)
    | PTyp of core_type  (* : T *)
    | PPat of pattern * expression option  (* ? P  or  ? P when E *)

  (** {2 Core language} *)

  (* Type expressions *)

  and core_type (*IF_CURRENT = Parsetree.core_type *) =
      {
       ptyp_desc: core_type_desc;
       ptyp_loc: Location.t;
       ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and core_type_desc (*IF_CURRENT = Parsetree.core_type_desc *) =
    | Ptyp_any
          (*  _ *)
    | Ptyp_var of string
          (* 'a *)
    | Ptyp_arrow of arg_label * core_type * core_type
          (* T1 -> T2       Simple
             ~l:T1 -> T2    Labelled
             ?l:T1 -> T2    Otional
           *)
    | Ptyp_tuple of core_type list
          (* T1 * ... * Tn

             Invariant: n >= 2
          *)
    | Ptyp_constr of Longident.t loc * core_type list
          (* tconstr
             T tconstr
             (T1, ..., Tn) tconstr
           *)
    | Ptyp_object of (string * attributes * core_type) list * closed_flag
          (* < l1:T1; ...; ln:Tn >     (flag = Closed)
             < l1:T1; ...; ln:Tn; .. > (flag = Open)
           *)
    | Ptyp_class of Longident.t loc * core_type list
          (* #tconstr
             T #tconstr
             (T1, ..., Tn) #tconstr
           *)
    | Ptyp_alias of core_type * string
          (* T as 'a *)
    | Ptyp_variant of row_field list * closed_flag * label list option
          (* [ `A|`B ]         (flag = Closed; labels = None)
             [> `A|`B ]        (flag = Open;   labels = None)
             [< `A|`B ]        (flag = Closed; labels = Some [])
             [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
           *)
    | Ptyp_poly of string list * core_type
          (* 'a1 ... 'an. T

             Can only appear in the following context:

             - As the core_type of a Ppat_constraint node corresponding
               to a constraint on a let-binding: let x : 'a1 ... 'an. T
               = e ...

             - Under Cfk_virtual for methods (not values).

             - As the core_type of a Pctf_method node.

             - As the core_type of a Pexp_poly node.

             - As the pld_type field of a label_declaration.

             - As a core_type of a Ptyp_object node.
           *)

    | Ptyp_package of package_type
          (* (module S) *)
    | Ptyp_extension of extension
          (* [%id] *)

  and package_type = Longident.t loc * (Longident.t loc * core_type) list
        (*
          (module S)
          (module S with type t1 = T1 and ... and tn = Tn)
         *)

  and row_field (*IF_CURRENT = Parsetree.row_field *) =
    | Rtag of label * attributes * bool * core_type list
          (* [`A]                   ( true,  [] )
             [`A of T]              ( false, [T] )
             [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
             [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

            - The 2nd field is true if the tag contains a
              constant (empty) constructor.
            - '&' occurs when several types are used for the same constructor
              (see 4.2 in the manual)

            - TODO: switch to a record representation, and keep location
          *)
    | Rinherit of core_type
          (* [ T ] *)

  (* Patterns *)

  and pattern (*IF_CURRENT = Parsetree.pattern *) =
      {
       ppat_desc: pattern_desc;
       ppat_loc: Location.t;
       ppat_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and pattern_desc (*IF_CURRENT = Parsetree.pattern_desc *) =
    | Ppat_any
          (* _ *)
    | Ppat_var of string loc
          (* x *)
    | Ppat_alias of pattern * string loc
          (* P as 'a *)
    | Ppat_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Ppat_interval of constant * constant
          (* 'a'..'z'

             Other forms of interval are recognized by the parser
             but rejected by the type-checker. *)
    | Ppat_tuple of pattern list
          (* (P1, ..., Pn)

             Invariant: n >= 2
          *)
    | Ppat_construct of Longident.t loc * pattern option
          (* C                None
             C P              Some P
             C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
           *)
    | Ppat_variant of label * pattern option
          (* `A             (None)
             `A P           (Some P)
           *)
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
          (* { l1=P1; ...; ln=Pn }     (flag = Closed)
             { l1=P1; ...; ln=Pn; _}   (flag = Open)

             Invariant: n > 0
           *)
    | Ppat_array of pattern list
          (* [| P1; ...; Pn |] *)
    | Ppat_or of pattern * pattern
          (* P1 | P2 *)
    | Ppat_constraint of pattern * core_type
          (* (P : T) *)
    | Ppat_type of Longident.t loc
          (* #tconst *)
    | Ppat_lazy of pattern
          (* lazy P *)
    | Ppat_unpack of string loc
          (* (module P)
             Note: (module P : S) is represented as
             Ppat_constraint(Ppat_unpack, Ptyp_package)
           *)
    | Ppat_exception of pattern
          (* exception P *)
    | Ppat_extension of extension
          (* [%id] *)

  (* Value expressions *)

  and expression (*IF_CURRENT = Parsetree.expression *) =
      {
       pexp_desc: expression_desc;
       pexp_loc: Location.t;
       pexp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and expression_desc (*IF_CURRENT = Parsetree.expression_desc *) =
    | Pexp_ident of Longident.t loc
          (* x
             M.x
           *)
    | Pexp_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Pexp_let of rec_flag * value_binding list * expression
          (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
           *)
    | Pexp_function of case list
          (* function P1 -> E1 | ... | Pn -> En *)
    | Pexp_fun of arg_label * expression option * pattern * expression
          (* fun P -> E1                          (Simple, None)
             fun ~l:P -> E1                       (Labelled l, None)
             fun ?l:P -> E1                       (Optional l, None)
             fun ?l:(P = E0) -> E1                (Optional l, Some E0)

             Notes:
             - If E0 is provided, only Optional is allowed.
             - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
             - "let f P = E" is represented using Pexp_fun.
           *)
    | Pexp_apply of expression * (arg_label * expression) list
          (* E0 ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pexp_match of expression * case list
          (* match E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_try of expression * case list
          (* try E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_tuple of expression list
          (* (E1, ..., En)

             Invariant: n >= 2
          *)
    | Pexp_construct of Longident.t loc * expression option
          (* C                None
             C E              Some E
             C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
          *)
    | Pexp_variant of label * expression option
          (* `A             (None)
             `A E           (Some E)
           *)
    | Pexp_record of (Longident.t loc * expression) list * expression option
          (* { l1=P1; ...; ln=Pn }     (None)
             { E0 with l1=P1; ...; ln=Pn }   (Some E0)

             Invariant: n > 0
           *)
    | Pexp_field of expression * Longident.t loc
          (* E.l *)
    | Pexp_setfield of expression * Longident.t loc * expression
          (* E1.l <- E2 *)
    | Pexp_array of expression list
          (* [| E1; ...; En |] *)
    | Pexp_ifthenelse of expression * expression * expression option
          (* if E1 then E2 else E3 *)
    | Pexp_sequence of expression * expression
          (* E1; E2 *)
    | Pexp_while of expression * expression
          (* while E1 do E2 done *)
    | Pexp_for of
        pattern *  expression * expression * direction_flag * expression
          (* for i = E1 to E2 do E3 done      (flag = Upto)
             for i = E1 downto E2 do E3 done  (flag = Downto)
           *)
    | Pexp_constraint of expression * core_type
          (* (E : T) *)
    | Pexp_coerce of expression * core_type option * core_type
          (* (E :> T)        (None, T)
             (E : T0 :> T)   (Some T0, T)
           *)
    | Pexp_send of expression * string
          (*  E # m *)
    | Pexp_new of Longident.t loc
          (* new M.c *)
    | Pexp_setinstvar of string loc * expression
          (* x <- 2 *)
    | Pexp_override of (string loc * expression) list
          (* {< x1 = E1; ...; Xn = En >} *)
    | Pexp_letmodule of string loc * module_expr * expression
          (* let module M = ME in E *)
    | Pexp_assert of expression
          (* assert E
             Note: "assert false" is treated in a special way by the
             type-checker. *)
    | Pexp_lazy of expression
          (* lazy E *)
    | Pexp_poly of expression * core_type option
          (* Used for method bodies.

             Can only be used as the expression under Cfk_concrete
             for methods (not values). *)
    | Pexp_object of class_structure
          (* object ... end *)
    | Pexp_newtype of string * expression
          (* fun (type t) -> E *)
    | Pexp_pack of module_expr
          (* (module ME)

             (module ME : S) is represented as
             Pexp_constraint(Pexp_pack, Ptyp_package S) *)
    | Pexp_open of override_flag * Longident.t loc * expression
          (* let open M in E
             let! open M in E
          *)
    | Pexp_extension of extension
          (* [%id] *)
    | Pexp_unreachable
          (* . *)

  and case (*IF_CURRENT = Parsetree.case *) =   (* (P -> E) or (P when E0 -> E) *)
      {
       pc_lhs: pattern;
       pc_guard: expression option;
       pc_rhs: expression;
      }

  (* Value descriptions *)

  and value_description (*IF_CURRENT = Parsetree.value_description *) =
      {
       pval_name: string loc;
       pval_type: core_type;
       pval_prim: string list;
       pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
       pval_loc: Location.t;
      }

  (*
    val x: T                            (prim = [])
    external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
  *)

  (* Type declarations *)

  and type_declaration (*IF_CURRENT = Parsetree.type_declaration *) =
      {
       ptype_name: string loc;
       ptype_params: (core_type * variance) list;
             (* ('a1,...'an) t; None represents  _*)
       ptype_cstrs: (core_type * core_type * Location.t) list;
             (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
       ptype_kind: type_kind;
       ptype_private: private_flag;   (* = private ... *)
       ptype_manifest: core_type option;  (* = T *)
       ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
       ptype_loc: Location.t;
      }

  (*
    type t                     (abstract, no manifest)
    type t = T0                (abstract, manifest=T0)
    type t = C of T | ...      (variant,  no manifest)
    type t = T0 = C of T | ... (variant,  manifest=T0)
    type t = {l: T; ...}       (record,   no manifest)
    type t = T0 = {l : T; ...} (record,   manifest=T0)
    type t = ..                (open,     no manifest)
  *)

  and type_kind (*IF_CURRENT = Parsetree.type_kind *) =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
          (* Invariant: non-empty list *)
    | Ptype_record of label_declaration list
          (* Invariant: non-empty list *)
    | Ptype_open

  and label_declaration (*IF_CURRENT = Parsetree.label_declaration *) =
      {
       pld_name: string loc;
       pld_mutable: mutable_flag;
       pld_type: core_type;
       pld_loc: Location.t;
       pld_attributes: attributes; (* l [@id1] [@id2] : T *)
      }

  (*  { ...; l: T; ... }            (mutable=Immutable)
      { ...; mutable l: T; ... }    (mutable=Mutable)

      Note: T can be a Ptyp_poly.
  *)

  and constructor_declaration (*IF_CURRENT = Parsetree.constructor_declaration *) =
      {
       pcd_name: string loc;
       pcd_args: constructor_arguments;
       pcd_res: core_type option;
       pcd_loc: Location.t;
       pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }

  and constructor_arguments (*IF_CURRENT = Parsetree.constructor_arguments *) =
    | Pcstr_tuple of core_type list
    | Pcstr_record of label_declaration list

  (*
    | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
    | C: T0                  (res = Some T0, args = [])
    | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
    | C of {...}             (res = None,    args = Pcstr_record)
    | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
    | C of {...} as t        (res = None,    args = Pcstr_record)
  *)

  and type_extension (*IF_CURRENT = Parsetree.type_extension *) =
      {
       ptyext_path: Longident.t loc;
       ptyext_params: (core_type * variance) list;
       ptyext_constructors: extension_constructor list;
       ptyext_private: private_flag;
       ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
      }
  (*
    type t += ...
  *)

  and extension_constructor (*IF_CURRENT = Parsetree.extension_constructor *) =
      {
       pext_name: string loc;
       pext_kind : extension_constructor_kind;
       pext_loc : Location.t;
       pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }

  and extension_constructor_kind (*IF_CURRENT = Parsetree.extension_constructor_kind *) =
      Pext_decl of constructor_arguments * core_type option
        (*
           | C of T1 * ... * Tn     ([T1; ...; Tn], None)
           | C: T0                  ([], Some T0)
           | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
         *)
    | Pext_rebind of Longident.t loc
        (*
           | C = D
         *)

  (** {2 Class language} *)

  (* Type expressions for the class language *)

  and class_type (*IF_CURRENT = Parsetree.class_type *) =
      {
       pcty_desc: class_type_desc;
       pcty_loc: Location.t;
       pcty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_type_desc (*IF_CURRENT = Parsetree.class_type_desc *) =
    | Pcty_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcty_signature of class_signature
          (* object ... end *)
    | Pcty_arrow of arg_label * core_type * class_type
          (* T -> CT       Simple
             ~l:T -> CT    Labelled l
             ?l:T -> CT    Optional l
           *)
    | Pcty_extension of extension
          (* [%id] *)

  and class_signature (*IF_CURRENT = Parsetree.class_signature *) =
      {
       pcsig_self: core_type;
       pcsig_fields: class_type_field list;
      }
  (* object('selfpat) ... end
     object ... end             (self = Ptyp_any)
   *)

  and class_type_field (*IF_CURRENT = Parsetree.class_type_field *) =
      {
       pctf_desc: class_type_field_desc;
       pctf_loc: Location.t;
       pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_type_field_desc (*IF_CURRENT = Parsetree.class_type_field_desc *) =
    | Pctf_inherit of class_type
          (* inherit CT *)
    | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
          (* val x: T *)
    | Pctf_method  of (string * private_flag * virtual_flag * core_type)
          (* method x: T

             Note: T can be a Ptyp_poly.
           *)
    | Pctf_constraint  of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pctf_attribute of attribute
          (* [@@@id] *)
    | Pctf_extension of extension
          (* [%%id] *)

  and 'a class_infos (*IF_CURRENT = 'a Parsetree.class_infos *) =
      {
       pci_virt: virtual_flag;
       pci_params: (core_type * variance) list;
       pci_name: string loc;
       pci_expr: 'a;
       pci_loc: Location.t;
       pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
      }
  (* class c = ...
     class ['a1,...,'an] c = ...
     class virtual c = ...

     Also used for "class type" declaration.
  *)

  and class_description = class_type class_infos

  and class_type_declaration = class_type class_infos

  (* Value expressions for the class language *)

  and class_expr (*IF_CURRENT = Parsetree.class_expr *) =
      {
       pcl_desc: class_expr_desc;
       pcl_loc: Location.t;
       pcl_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_expr_desc (*IF_CURRENT = Parsetree.class_expr_desc *) =
    | Pcl_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcl_structure of class_structure
          (* object ... end *)
    | Pcl_fun of arg_label * expression option * pattern * class_expr
          (* fun P -> CE                          (Simple, None)
             fun ~l:P -> CE                       (Labelled l, None)
             fun ?l:P -> CE                       (Optional l, None)
             fun ?l:(P = E0) -> CE                (Optional l, Some E0)
           *)
    | Pcl_apply of class_expr * (arg_label * expression) list
          (* CE ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pcl_let of rec_flag * value_binding list * class_expr
          (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
           *)
    | Pcl_constraint of class_expr * class_type
          (* (CE : CT) *)
    | Pcl_extension of extension
          (* [%id] *)

  and class_structure (*IF_CURRENT = Parsetree.class_structure *) =
      {
       pcstr_self: pattern;
       pcstr_fields: class_field list;
      }
  (* object(selfpat) ... end
     object ... end           (self = Ppat_any)
   *)

  and class_field (*IF_CURRENT = Parsetree.class_field *) =
      {
       pcf_desc: class_field_desc;
       pcf_loc: Location.t;
       pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_field_desc (*IF_CURRENT = Parsetree.class_field_desc *) =
    | Pcf_inherit of override_flag * class_expr * string option
          (* inherit CE
             inherit CE as x
             inherit! CE
             inherit! CE as x
           *)
    | Pcf_val of (string loc * mutable_flag * class_field_kind)
          (* val x = E
             val virtual x: T
           *)
    | Pcf_method of (string loc * private_flag * class_field_kind)
          (* method x = E            (E can be a Pexp_poly)
             method virtual x: T     (T can be a Ptyp_poly)
           *)
    | Pcf_constraint of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pcf_initializer of expression
          (* initializer E *)
    | Pcf_attribute of attribute
          (* [@@@id] *)
    | Pcf_extension of extension
          (* [%%id] *)

  and class_field_kind (*IF_CURRENT = Parsetree.class_field_kind *) =
    | Cfk_virtual of core_type
    | Cfk_concrete of override_flag * expression

  and class_declaration = class_expr class_infos

  (** {2 Module language} *)

  (* Type expressions for the module language *)

  and module_type (*IF_CURRENT = Parsetree.module_type *) =
      {
       pmty_desc: module_type_desc;
       pmty_loc: Location.t;
       pmty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_type_desc (*IF_CURRENT = Parsetree.module_type_desc *) =
    | Pmty_ident of Longident.t loc
          (* S *)
    | Pmty_signature of signature
          (* sig ... end *)
    | Pmty_functor of string loc * module_type option * module_type
          (* functor(X : MT1) -> MT2 *)
    | Pmty_with of module_type * with_constraint list
          (* MT with ... *)
    | Pmty_typeof of module_expr
          (* module type of ME *)
    | Pmty_extension of extension
          (* [%id] *)
    | Pmty_alias of Longident.t loc
          (* (module M) *)

  and signature = signature_item list

  and signature_item (*IF_CURRENT = Parsetree.signature_item *) =
      {
       psig_desc: signature_item_desc;
       psig_loc: Location.t;
      }

  and signature_item_desc (*IF_CURRENT = Parsetree.signature_item_desc *) =
    | Psig_value of value_description
          (*
            val x: T
            external x: T = "s1" ... "sn"
           *)
    | Psig_type of rec_flag * type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Psig_typext of type_extension
          (* type t1 += ... *)
    | Psig_exception of extension_constructor
          (* exception C of T *)
    | Psig_module of module_declaration
          (* module X : MT *)
    | Psig_recmodule of module_declaration list
          (* module rec X1 : MT1 and ... and Xn : MTn *)
    | Psig_modtype of module_type_declaration
          (* module type S = MT
             module type S *)
    | Psig_open of open_description
          (* open X *)
    | Psig_include of include_description
          (* include MT *)
    | Psig_class of class_description list
          (* class c1 : ... and ... and cn : ... *)
    | Psig_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Psig_attribute of attribute
          (* [@@@id] *)
    | Psig_extension of extension * attributes
          (* [%%id] *)

  and module_declaration (*IF_CURRENT = Parsetree.module_declaration *) =
      {
       pmd_name: string loc;
       pmd_type: module_type;
       pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmd_loc: Location.t;
      }
  (* S : MT *)

  and module_type_declaration (*IF_CURRENT = Parsetree.module_type_declaration *) =
      {
       pmtd_name: string loc;
       pmtd_type: module_type option;
       pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmtd_loc: Location.t;
      }
  (* S = MT
     S       (abstract module type declaration, pmtd_type = None)
  *)

  and open_description (*IF_CURRENT = Parsetree.open_description *) =
      {
       popen_lid: Longident.t loc;
       popen_override: override_flag;
       popen_loc: Location.t;
       popen_attributes: attributes;
      }
  (* open! X - popen_override = Override (silences the 'used identifier
                                shadowing' warning)
     open  X - popen_override = Fresh
   *)

  and 'a include_infos (*IF_CURRENT = 'a Parsetree.include_infos *) =
      {
       pincl_mod: 'a;
       pincl_loc: Location.t;
       pincl_attributes: attributes;
      }

  and include_description = module_type include_infos
  (* include MT *)

  and include_declaration = module_expr include_infos
  (* include ME *)

  and with_constraint (*IF_CURRENT = Parsetree.with_constraint *) =
    | Pwith_type of Longident.t loc * type_declaration
          (* with type X.t = ...

             Note: the last component of the longident must match
             the name of the type_declaration. *)
    | Pwith_module of Longident.t loc * Longident.t loc
          (* with module X.Y = Z *)
    | Pwith_typesubst of type_declaration
          (* with type t := ... *)
    | Pwith_modsubst of string loc * Longident.t loc
          (* with module X := Z *)

  (* Value expressions for the module language *)

  and module_expr (*IF_CURRENT = Parsetree.module_expr *) =
      {
       pmod_desc: module_expr_desc;
       pmod_loc: Location.t;
       pmod_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_expr_desc (*IF_CURRENT = Parsetree.module_expr_desc *) =
    | Pmod_ident of Longident.t loc
          (* X *)
    | Pmod_structure of structure
          (* struct ... end *)
    | Pmod_functor of string loc * module_type option * module_expr
          (* functor(X : MT1) -> ME *)
    | Pmod_apply of module_expr * module_expr
          (* ME1(ME2) *)
    | Pmod_constraint of module_expr * module_type
          (* (ME : MT) *)
    | Pmod_unpack of expression
          (* (val E) *)
    | Pmod_extension of extension
          (* [%id] *)

  and structure = structure_item list

  and structure_item (*IF_CURRENT = Parsetree.structure_item *) =
      {
       pstr_desc: structure_item_desc;
       pstr_loc: Location.t;
      }

  and structure_item_desc (*IF_CURRENT = Parsetree.structure_item_desc *) =
    | Pstr_eval of expression * attributes
          (* E *)
    | Pstr_value of rec_flag * value_binding list
          (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
           *)
    | Pstr_primitive of value_description
          (*  val x: T
              external x: T = "s1" ... "sn" *)
    | Pstr_type of rec_flag * type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Pstr_typext of type_extension
          (* type t1 += ... *)
    | Pstr_exception of extension_constructor
          (* exception C of T
             exception C = M.X *)
    | Pstr_module of module_binding
          (* module X = ME *)
    | Pstr_recmodule of module_binding list
          (* module rec X1 = ME1 and ... and Xn = MEn *)
    | Pstr_modtype of module_type_declaration
          (* module type S = MT *)
    | Pstr_open of open_description
          (* open X *)
    | Pstr_class of class_declaration list
          (* class c1 = ... and ... and cn = ... *)
    | Pstr_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Pstr_include of include_declaration
          (* include ME *)
    | Pstr_attribute of attribute
          (* [@@@id] *)
    | Pstr_extension of extension * attributes
          (* [%%id] *)

  and value_binding (*IF_CURRENT = Parsetree.value_binding *) =
    {
      pvb_pat: pattern;
      pvb_expr: expression;
      pvb_attributes: attributes;
      pvb_loc: Location.t;
    }

  and module_binding (*IF_CURRENT = Parsetree.module_binding *) =
      {
       pmb_name: string loc;
       pmb_expr: module_expr;
       pmb_attributes: attributes;
       pmb_loc: Location.t;
      }
  (* X = ME *)

  (** {2 Toplevel} *)

  (* Toplevel phrases *)

  type toplevel_phrase (*IF_CURRENT = Parsetree.toplevel_phrase *) =
    | Ptop_def of structure
    | Ptop_dir of string * directive_argument
       (* #use, #load ... *)

  and directive_argument (*IF_CURRENT = Parsetree.directive_argument *) =
    | Pdir_none
    | Pdir_string of string
    | Pdir_int of string * char option
    | Pdir_ident of Longident.t
    | Pdir_bool of bool
end

module Docstrings : sig
  (** {3 Docstrings} *)

  (** Documentation comments *)
  type docstring

  (** Create a docstring *)
  val docstring : string -> Location.t -> docstring

  (** Get the text of a docstring *)
  val docstring_body : docstring -> string

  (** Get the location of a docstring *)
  val docstring_loc : docstring -> Location.t

  (** {3 Items}

      The {!docs} type represents documentation attached to an item. *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  val empty_docs : docs

  val docs_attr : docstring -> Parsetree.attribute

  (** Convert item documentation to attributes and add them to an
      attribute list *)
  val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Fields and constructors}

      The {!info} type represents documentation attached to a field or
      constructor. *)

  type info = docstring option

  val empty_info : info

  val info_attr : docstring -> Parsetree.attribute

  (** Convert field info to attributes and add them to an
      attribute list *)
  val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Unattached comments}

      The {!text} type represents documentation which is not attached to
      anything. *)

  type text = docstring list

  val empty_text : text

  val text_attr : docstring -> Parsetree.attribute

  (** Convert text to attributes and add them to an attribute list *)
  val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

end = struct
  open Location

  (* Docstrings *)

  type docstring =
    { ds_body: string;
      ds_loc: Location.t; }

  (* Docstring constructors and destructors *)

  let docstring body loc =
    let ds =
      { ds_body = body;
        ds_loc = loc; }
    in
    ds

  let docstring_body ds = ds.ds_body

  let docstring_loc ds = ds.ds_loc

  (* Docstrings attached to items *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  let empty_docs = { docs_pre = None; docs_post = None }

  let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

  let docs_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (doc_loc, PStr [item])

  let add_docs_attrs docs attrs =
    let attrs =
      match docs.docs_pre with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> docs_attr ds :: attrs
    in
    let attrs =
      match docs.docs_post with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> attrs @ [docs_attr ds]
    in
    attrs

  (* Docstrings attached to constructors or fields *)

  type info = docstring option

  let empty_info = None

  let info_attr = docs_attr

  let add_info_attrs info attrs =
    match info with
    | None | Some {ds_body=""; _} -> attrs
    | Some ds -> attrs @ [info_attr ds]

  (* Docstrings not attached to a specific item *)

  type text = docstring list

  let empty_text = []

  let text_loc = {txt = "ocaml.text"; loc = Location.none}

  let text_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (text_loc, PStr [item])

  let add_text_attrs dsl attrs =
    let fdsl = List.filter (function {ds_body=""; _} -> false| _ ->true) dsl in
    (List.map text_attr fdsl) @ attrs

end

module Ast_helper : sig
  (** Helpers to produce Parsetree fragments *)

  open Asttypes
  open Docstrings
  open Parsetree

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  (** {2 Default locations} *)

  val default_loc: loc ref
      (** Default value for all optional location arguments. *)

  val with_default_loc: loc -> (unit -> 'a) -> 'a
      (** Set the [default_loc] within the scope of the execution
          of the provided function. *)

  (** {2 Constants} *)

  module Const : sig
    val char : char -> constant
    val string : ?quotation_delimiter:string -> string -> constant
    val integer : ?suffix:char -> string -> constant
    val int : ?suffix:char -> int -> constant
    val int32 : ?suffix:char -> int32 -> constant
    val int64 : ?suffix:char -> int64 -> constant
    val nativeint : ?suffix:char -> nativeint -> constant
    val float : ?suffix:char -> string -> constant
  end

  (** {2 Core language} *)

  (** Type expressions *)
  module Typ :
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
      val attr: core_type -> attribute -> core_type

      val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
      val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
      val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type -> core_type
                 -> core_type
      val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val object_: ?loc:loc -> ?attrs:attrs ->
                    (string * attributes * core_type) list -> closed_flag ->
                    core_type
      val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
      val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
                   -> label list option -> core_type
      val poly: ?loc:loc -> ?attrs:attrs -> string list -> core_type -> core_type
      val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
                   -> core_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

      val force_poly: core_type -> core_type
    end

  (** Patterns *)
  module Pat:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
      val attr:pattern -> attribute -> pattern

      val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
      val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
      val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
      val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
      val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
      val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
                  -> pattern
      val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
      val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
      val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
      val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val unpack: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
    end

  (** Expressions *)
  module Exp:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
      val attr: expression -> attribute -> expression

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
                -> expression -> expression
      val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option
                -> pattern -> expression -> expression
      val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
      val apply: ?loc:loc -> ?attrs:attrs -> expression
                 -> (arg_label * expression) list -> expression
      val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
                  -> expression
      val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
      val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                     -> expression
      val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
                   -> expression
      val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                  -> expression option -> expression
      val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
      val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                    -> expression
      val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                      -> expression option -> expression
      val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression
      val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
      val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
                -> direction_flag -> expression -> expression
      val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                  -> core_type -> expression
      val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                       -> expression
      val send: ?loc:loc -> ?attrs:attrs -> expression -> string -> expression
      val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
      val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                    -> expression
      val letmodule: ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression
                     -> expression
      val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                -> expression
      val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
      val newtype: ?loc:loc -> ?attrs:attrs -> string -> expression -> expression
      val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
      val open_: ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> expression
                 -> expression
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
      val unreachable: ?loc:loc -> ?attrs:attrs -> unit -> expression

      val case: pattern -> ?guard:expression -> expression -> case
    end

  (** Value declarations *)
  module Val:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        ?prim:string list -> str -> core_type -> value_description
    end

  (** Type declarations *)
  module Type:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?params:(core_type * variance) list ->
        ?cstrs:(core_type * core_type * loc) list ->
        ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
        type_declaration

      val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?args:constructor_arguments -> ?res:core_type -> str ->
        constructor_declaration
      val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?mut:mutable_flag -> str -> core_type -> label_declaration
    end

  (** Type extensions *)
  module Te:
    sig
      val mk: ?attrs:attrs -> ?docs:docs ->
        ?params:(core_type * variance) list -> ?priv:private_flag ->
        lid -> extension_constructor list -> type_extension

      val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> extension_constructor_kind -> extension_constructor

      val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        ?args:constructor_arguments -> ?res:core_type -> str ->
        extension_constructor
      val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> lid -> extension_constructor
    end

  (** {2 Module language} *)

  (** Module type expressions *)
  module Mty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
      val attr: module_type -> attribute -> module_type

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_type -> module_type
      val with_: ?loc:loc -> ?attrs:attrs -> module_type ->
        with_constraint list -> module_type
      val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
    end

  (** Module expressions *)
  module Mod:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
      val attr: module_expr -> attribute -> module_expr

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
      val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_expr -> module_expr
      val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr ->
        module_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type ->
        module_expr
      val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
    end

  (** Signature items *)
  module Sig:
    sig
      val mk: ?loc:loc -> signature_item_desc -> signature_item

      val value: ?loc:loc -> value_description -> signature_item
      val type_: ?loc:loc -> rec_flag -> type_declaration list -> signature_item
      val type_extension: ?loc:loc -> type_extension -> signature_item
      val exception_: ?loc:loc -> extension_constructor -> signature_item
      val module_: ?loc:loc -> module_declaration -> signature_item
      val rec_module: ?loc:loc -> module_declaration list -> signature_item
      val modtype: ?loc:loc -> module_type_declaration -> signature_item
      val open_: ?loc:loc -> open_description -> signature_item
      val include_: ?loc:loc -> include_description -> signature_item
      val class_: ?loc:loc -> class_description list -> signature_item
      val class_type: ?loc:loc -> class_type_declaration list -> signature_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
      val attribute: ?loc:loc -> attribute -> signature_item
      val text: text -> signature_item list
    end

  (** Structure items *)
  module Str:
    sig
      val mk: ?loc:loc -> structure_item_desc -> structure_item

      val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
      val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
      val primitive: ?loc:loc -> value_description -> structure_item
      val type_: ?loc:loc -> rec_flag -> type_declaration list -> structure_item
      val type_extension: ?loc:loc -> type_extension -> structure_item
      val exception_: ?loc:loc -> extension_constructor -> structure_item
      val module_: ?loc:loc -> module_binding -> structure_item
      val rec_module: ?loc:loc -> module_binding list -> structure_item
      val modtype: ?loc:loc -> module_type_declaration -> structure_item
      val open_: ?loc:loc -> open_description -> structure_item
      val class_: ?loc:loc -> class_declaration list -> structure_item
      val class_type: ?loc:loc -> class_type_declaration list -> structure_item
      val include_: ?loc:loc -> include_declaration -> structure_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
      val attribute: ?loc:loc -> attribute -> structure_item
      val text: text -> structure_item list
    end

  (** Module declarations *)
  module Md:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_type -> module_declaration
    end

  (** Module type declarations *)
  module Mtd:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?typ:module_type -> str -> module_type_declaration
    end

  (** Module bindings *)
  module Mb:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_expr -> module_binding
    end

  (* Opens *)
  module Opn:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
        ?override:override_flag -> lid -> open_description
    end

  (* Includes *)
  module Incl:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
    end

  (** Value bindings *)

  module Vb:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        pattern -> expression -> value_binding
    end


  (** {2 Class language} *)

  (** Class type expressions *)
  module Cty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
      val attr: class_type -> attribute -> class_type

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
      val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
      val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type ->
        class_type -> class_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    end

  (** Class type fields *)
  module Ctf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        class_type_field_desc -> class_type_field
      val attr: class_type_field -> attribute -> class_type_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
      val val_: ?loc:loc -> ?attrs:attrs -> string -> mutable_flag ->
        virtual_flag -> core_type -> class_type_field
      val method_: ?loc:loc -> ?attrs:attrs -> string -> private_flag ->
        virtual_flag -> core_type -> class_type_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
        class_type_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
      val attribute: ?loc:loc -> attribute -> class_type_field
      val text: text -> class_type_field list
    end

  (** Class expressions *)
  module Cl:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
      val attr: class_expr -> attribute -> class_expr

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
      val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
      val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option ->
        pattern -> class_expr -> class_expr
      val apply: ?loc:loc -> ?attrs:attrs -> class_expr ->
        (arg_label * expression) list -> class_expr
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list ->
        class_expr -> class_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type ->
        class_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    end

  (** Class fields *)
  module Cf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc ->
        class_field
      val attr: class_field -> attribute -> class_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr ->
        string option -> class_field
      val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
        class_field_kind -> class_field
      val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
        class_field_kind -> class_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
        class_field
      val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
      val attribute: ?loc:loc -> attribute -> class_field
      val text: text -> class_field list

      val virtual_: core_type -> class_field_kind
      val concrete: override_flag -> expression -> class_field_kind

    end

  (** Classes *)
  module Ci:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?virt:virtual_flag -> ?params:(core_type * variance) list ->
        str -> 'a -> 'a class_infos
    end

  (** Class signatures *)
  module Csig:
    sig
      val mk: core_type -> class_type_field list -> class_signature
    end

  (** Class structures *)
  module Cstr:
    sig
      val mk: pattern -> class_field list -> class_structure
    end

end = struct
  (** Helpers to produce Parsetree fragments *)

  open Asttypes
  open Parsetree
  open Docstrings

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  let default_loc = ref Location.none

  let with_default_loc l f =
    let old = !default_loc in
    default_loc := l;
    try let r = f () in default_loc := old; r
    with exn -> default_loc := old; raise exn

  module Const = struct
    let integer ?suffix i = Pconst_integer (i, suffix)
    let int ?suffix i = integer ?suffix (string_of_int i)
    let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
    let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
    let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
    let float ?suffix f = Pconst_float (f, suffix)
    let char c = Pconst_char c
    let string ?quotation_delimiter s = Pconst_string (s, quotation_delimiter)
  end

  module Typ = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
    let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
    let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
    let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
    let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
    let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
    let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

    let force_poly t =
      match t.ptyp_desc with
      | Ptyp_poly _ -> t
      | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)
  end

  module Pat = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
    let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
    let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
    let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
    let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
    let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
    let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
    let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
    let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
  end

  module Exp = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
    let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
    let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
    let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
    let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
    let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
    let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
    let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
    let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
    let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
    let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
    let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
    let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
    let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
    let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
    let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
    let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
    let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
    let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
    let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
    let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
    let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
    let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
    let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
    let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable

    let case lhs ?guard rhs =
      {
       pc_lhs = lhs;
       pc_guard = guard;
       pc_rhs = rhs;
      }
  end

  module Mty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
    let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
    let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
    let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
    let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
    let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
  end

  module Mod = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
    let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

    let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
    let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
    let functor_ ?loc ?attrs arg arg_ty body =
      mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
    let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
    let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
    let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
  end

  module Sig = struct
    let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

    let value ?loc a = mk ?loc (Psig_value a)
    let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Psig_typext a)
    let exception_ ?loc a = mk ?loc (Psig_exception a)
    let module_ ?loc a = mk ?loc (Psig_module a)
    let rec_module ?loc a = mk ?loc (Psig_recmodule a)
    let modtype ?loc a = mk ?loc (Psig_modtype a)
    let open_ ?loc a = mk ?loc (Psig_open a)
    let include_ ?loc a = mk ?loc (Psig_include a)
    let class_ ?loc a = mk ?loc (Psig_class a)
    let class_type ?loc a = mk ?loc (Psig_class_type a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Psig_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt
  end

  module Str = struct
    let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

    let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
    let value ?loc a b = mk ?loc (Pstr_value (a, b))
    let primitive ?loc a = mk ?loc (Pstr_primitive a)
    let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Pstr_typext a)
    let exception_ ?loc a = mk ?loc (Pstr_exception a)
    let module_ ?loc a = mk ?loc (Pstr_module a)
    let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
    let modtype ?loc a = mk ?loc (Pstr_modtype a)
    let open_ ?loc a = mk ?loc (Pstr_open a)
    let class_ ?loc a = mk ?loc (Pstr_class a)
    let class_type ?loc a = mk ?loc (Pstr_class_type a)
    let include_ ?loc a = mk ?loc (Pstr_include a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Pstr_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt
  end

  module Cl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcl_desc = d;
       pcl_loc = loc;
       pcl_attributes = attrs;
      }
    let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
    let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
  end

  module Cty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcty_desc = d;
       pcty_loc = loc;
       pcty_attributes = attrs;
      }
    let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
  end

  module Ctf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
             ?(docs = empty_docs) d =
      {
       pctf_desc = d;
       pctf_loc = loc;
       pctf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
    let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
    let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
    let attribute ?loc a = mk ?loc (Pctf_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt

    let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

  end

  module Cf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) d =
      {
       pcf_desc = d;
       pcf_loc = loc;
       pcf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
    let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
    let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
    let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
    let attribute ?loc a = mk ?loc (Pcf_attribute a)
    let text txt =
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        txt

    let virtual_ ct = Cfk_virtual ct
    let concrete o e = Cfk_concrete (o, e)

    let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  end

  module Val = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(prim = []) name typ =
      {
       pval_name = name;
       pval_type = typ;
       pval_attributes = add_docs_attrs docs attrs;
       pval_loc = loc;
       pval_prim = prim;
      }
  end

  module Md = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name typ =
      {
       pmd_name = name;
       pmd_type = typ;
       pmd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmd_loc = loc;
      }
  end

  module Mtd = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) ?typ name =
      {
       pmtd_name = name;
       pmtd_type = typ;
       pmtd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmtd_loc = loc;
      }
  end

  module Mb = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name expr =
      {
       pmb_name = name;
       pmb_expr = expr;
       pmb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmb_loc = loc;
      }
  end

  module Opn = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(override = Fresh) lid =
      {
       popen_lid = lid;
       popen_override = override;
       popen_loc = loc;
       popen_attributes = add_docs_attrs docs attrs;
      }
  end

  module Incl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
      {
       pincl_mod = mexpr;
       pincl_loc = loc;
       pincl_attributes = add_docs_attrs docs attrs;
      }

  end

  module Vb = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(text = []) pat expr =
      {
       pvb_pat = pat;
       pvb_expr = expr;
       pvb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pvb_loc = loc;
      }
  end

  module Ci = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
          ?(virt = Concrete) ?(params = []) name expr =
      {
       pci_virt = virt;
       pci_params = params;
       pci_name = name;
       pci_expr = expr;
       pci_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pci_loc = loc;
      }
  end

  module Type = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
        ?(params = [])
        ?(cstrs = [])
        ?(kind = Ptype_abstract)
        ?(priv = Public)
        ?manifest
        name =
      {
       ptype_name = name;
       ptype_params = params;
       ptype_cstrs = cstrs;
       ptype_kind = kind;
       ptype_private = priv;
       ptype_manifest = manifest;
       ptype_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       ptype_loc = loc;
      }

    let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(args = Pcstr_tuple []) ?res name =
      {
       pcd_name = name;
       pcd_args = args;
       pcd_res = res;
       pcd_loc = loc;
       pcd_attributes = add_info_attrs info attrs;
      }

    let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(mut = Immutable) name typ =
      {
       pld_name = name;
       pld_mutable = mut;
       pld_type = typ;
       pld_loc = loc;
       pld_attributes = add_info_attrs info attrs;
      }

  end

  (** Type extensions *)
  module Te = struct
    let mk ?(attrs = []) ?(docs = empty_docs)
          ?(params = []) ?(priv = Public) path constructors =
      {
       ptyext_path = path;
       ptyext_params = params;
       ptyext_constructors = constructors;
       ptyext_private = priv;
       ptyext_attributes = add_docs_attrs docs attrs;
      }

    let constructor ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name kind =
      {
       pext_name = name;
       pext_kind = kind;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let decl ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
               ?(info = empty_info) ?(args = Pcstr_tuple []) ?res name =
      {
       pext_name = name;
       pext_kind = Pext_decl(args, res);
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let rebind ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name lid =
      {
       pext_name = name;
       pext_kind = Pext_rebind lid;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

  end

  module Csig = struct
    let mk self fields =
      {
       pcsig_self = self;
       pcsig_fields = fields;
      }
  end

  module Cstr = struct
    let mk self fields =
      {
       pcstr_self = self;
       pcstr_fields = fields;
      }
  end

end

module Ast_mapper : sig
  (** The interface of a -ppx rewriter

    A -ppx rewriter is a program that accepts a serialized abstract syntax
    tree and outputs another, possibly modified, abstract syntax tree.
    This module encapsulates the interface between the compiler and
    the -ppx rewriters, handling such details as the serialization format,
    forwarding of command-line flags, and storing state.

    {!mapper} allows to implement AST rewriting using open recursion.
    A typical mapper would be based on {!default_mapper}, a deep
    identity mapper, and will fall back on it for handling the syntax it
    does not modify. For example:

    {[
  open Asttypes
  open Parsetree
  open Ast_mapper

  let test_mapper argv =
    { default_mapper with
      expr = fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
          Ast_helper.Exp.constant (Const_int 42)
        | other -> default_mapper.expr mapper other; }

  let () =
    register "ppx_test" test_mapper]}

    This -ppx rewriter, which replaces [[%test]] in expressions with
    the constant [42], can be compiled using
    [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

    *)

  open Parsetree

  (** {2 A generic Parsetree mapper} *)

  type mapper (*IF_CURRENT = Ast_mapper.mapper*) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }
  (** A mapper record implements one "method" per syntactic category,
      using an open recursion style: each method takes as its first
      argument the mapper to be applied to children in the syntax
      tree. *)

  val default_mapper: mapper
  (** A default mapper, which implements a "deep identity" mapping. *)

  (** {2 Convenience functions to write mappers} *)

  val map_opt: ('a -> 'b) -> 'a option -> 'b option

  val extension_of_error: Location.error -> extension
  (** Encode an error into an 'ocaml.error' extension node which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the error. *)

  val attribute_of_warning: Location.t -> string -> attribute
  (** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the warning. *)

end = struct
  (* A generic Parsetree mapping class *)

  (*
  [@@@ocaml.warning "+9"]
    (* Ensure that record patterns don't miss any field. *)
  *)


  open Parsetree
  open Ast_helper
  open Location

  type mapper (*IF_CURRENT = Ast_mapper.mapper*) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }

  let map_fst f (x, y) = (f x, y)
  let map_snd f (x, y) = (x, f y)
  let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
  let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
  let map_opt f = function None -> None | Some x -> Some (f x)

  let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

  module T = struct
    (* Type expressions for the core language *)

    let row_field sub = function
      | Rtag (l, attrs, b, tl) ->
          Rtag (l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
      | Rinherit t -> Rinherit (sub.typ sub t)

    let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
      let open Typ in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ptyp_any -> any ~loc ~attrs ()
      | Ptyp_var s -> var ~loc ~attrs s
      | Ptyp_arrow (lab, t1, t2) ->
          arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
      | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
      | Ptyp_constr (lid, tl) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_object (l, o) ->
          let f (s, a, t) = (s, sub.attributes sub a, sub.typ sub t) in
          object_ ~loc ~attrs (List.map f l) o
      | Ptyp_class (lid, tl) ->
          class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
      | Ptyp_variant (rl, b, ll) ->
          variant ~loc ~attrs (List.map (row_field sub) rl) b ll
      | Ptyp_poly (sl, t) -> poly ~loc ~attrs sl (sub.typ sub t)
      | Ptyp_package (lid, l) ->
          package ~loc ~attrs (map_loc sub lid)
            (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
      | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_type_declaration sub
        {ptype_name; ptype_params; ptype_cstrs;
         ptype_kind;
         ptype_private;
         ptype_manifest;
         ptype_attributes;
         ptype_loc} =
      Type.mk (map_loc sub ptype_name)
        ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
        ~priv:ptype_private
        ~cstrs:(List.map
                  (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                  ptype_cstrs)
        ~kind:(sub.type_kind sub ptype_kind)
        ?manifest:(map_opt (sub.typ sub) ptype_manifest)
        ~loc:(sub.location sub ptype_loc)
        ~attrs:(sub.attributes sub ptype_attributes)

    let map_type_kind sub = function
      | Ptype_abstract -> Ptype_abstract
      | Ptype_variant l ->
          Ptype_variant (List.map (sub.constructor_declaration sub) l)
      | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
      | Ptype_open -> Ptype_open

    let map_constructor_arguments sub = function
      | Pcstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
      | Pcstr_record l ->
          Pcstr_record (List.map (sub.label_declaration sub) l)

    let map_type_extension sub
        {ptyext_path; ptyext_params;
         ptyext_constructors;
         ptyext_private;
         ptyext_attributes} =
      Te.mk
        (map_loc sub ptyext_path)
        (List.map (sub.extension_constructor sub) ptyext_constructors)
        ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
        ~priv:ptyext_private
        ~attrs:(sub.attributes sub ptyext_attributes)

    let map_extension_constructor_kind sub = function
        Pext_decl(ctl, cto) ->
          Pext_decl(map_constructor_arguments sub ctl, map_opt (sub.typ sub) cto)
      | Pext_rebind li ->
          Pext_rebind (map_loc sub li)

    let map_extension_constructor sub
        {pext_name;
         pext_kind;
         pext_loc;
         pext_attributes} =
      Te.constructor
        (map_loc sub pext_name)
        (map_extension_constructor_kind sub pext_kind)
        ~loc:(sub.location sub pext_loc)
        ~attrs:(sub.attributes sub pext_attributes)

  end

  module CT = struct
    (* Type expressions for the class language *)

    let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
      let open Cty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcty_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
      | Pcty_arrow (lab, t, ct) ->
          arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
      | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
      =
      let open Ctf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
      | Pctf_val (s, m, v, t) -> val_ ~loc ~attrs s m v (sub.typ sub t)
      | Pctf_method (s, p, v, t) -> method_ ~loc ~attrs s p v (sub.typ sub t)
      | Pctf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_signature sub {pcsig_self; pcsig_fields} =
      Csig.mk
        (sub.typ sub pcsig_self)
        (List.map (sub.class_type_field sub) pcsig_fields)
  end

  module MT = struct
    (* Type expressions for the module language *)

    let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
      let open Mty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
      | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
      | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
      | Pmty_functor (s, mt1, mt2) ->
          functor_ ~loc ~attrs (map_loc sub s)
            (Misc.may_map (sub.module_type sub) mt1)
            (sub.module_type sub mt2)
      | Pmty_with (mt, l) ->
          with_ ~loc ~attrs (sub.module_type sub mt)
            (List.map (sub.with_constraint sub) l)
      | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
      | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_with_constraint sub = function
      | Pwith_type (lid, d) ->
          Pwith_type (map_loc sub lid, sub.type_declaration sub d)
      | Pwith_module (lid, lid2) ->
          Pwith_module (map_loc sub lid, map_loc sub lid2)
      | Pwith_typesubst d -> Pwith_typesubst (sub.type_declaration sub d)
      | Pwith_modsubst (s, lid) ->
          Pwith_modsubst (map_loc sub s, map_loc sub lid)

    let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
      let open Sig in
      let loc = sub.location sub loc in
      match desc with
      | Psig_value vd -> value ~loc (sub.value_description sub vd)
      | Psig_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
      | Psig_recmodule l ->
          rec_module ~loc (List.map (sub.module_declaration sub) l)
      | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Psig_open x -> open_ ~loc (sub.open_description sub x)
      | Psig_include x -> include_ ~loc (sub.include_description sub x)
      | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
      | Psig_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Psig_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
  end


  module M = struct
    (* Value expressions for the module language *)

    let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
      let open Mod in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
      | Pmod_functor (arg, arg_ty, body) ->
          functor_ ~loc ~attrs (map_loc sub arg)
            (Misc.may_map (sub.module_type sub) arg_ty)
            (sub.module_expr sub body)
      | Pmod_apply (m1, m2) ->
          apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
      | Pmod_constraint (m, mty) ->
          constraint_ ~loc ~attrs (sub.module_expr sub m)
                      (sub.module_type sub mty)
      | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
      | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
      let open Str in
      let loc = sub.location sub loc in
      match desc with
      | Pstr_eval (x, attrs) ->
          eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
      | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
      | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
      | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
      | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
      | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Pstr_open x -> open_ ~loc (sub.open_description sub x)
      | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
      | Pstr_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
      | Pstr_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
  end

  module E = struct
    (* Value expressions for the core language *)

    let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
      let open Exp in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pexp_constant x -> constant ~loc ~attrs x
      | Pexp_let (r, vbs, e) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.expr sub e)
      | Pexp_fun (lab, def, p, e) ->
          fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
            (sub.expr sub e)
      | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
      | Pexp_apply (e, l) ->
          apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
      | Pexp_match (e, pel) ->
          match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_construct (lid, arg) ->
          construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
      | Pexp_variant (lab, eo) ->
          variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
      | Pexp_record (l, eo) ->
          record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
            (map_opt (sub.expr sub) eo)
      | Pexp_field (e, lid) ->
          field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
      | Pexp_setfield (e1, lid, e2) ->
          setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
            (sub.expr sub e2)
      | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_ifthenelse (e1, e2, e3) ->
          ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
            (map_opt (sub.expr sub) e3)
      | Pexp_sequence (e1, e2) ->
          sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_while (e1, e2) ->
          while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_for (p, e1, e2, d, e3) ->
          for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
            (sub.expr sub e3)
      | Pexp_coerce (e, t1, t2) ->
          coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
            (sub.typ sub t2)
      | Pexp_constraint (e, t) ->
          constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
      | Pexp_send (e, s) -> send ~loc ~attrs (sub.expr sub e) s
      | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
      | Pexp_setinstvar (s, e) ->
          setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
      | Pexp_override sel ->
          override ~loc ~attrs
            (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
      | Pexp_letmodule (s, me, e) ->
          letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
            (sub.expr sub e)
      | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
      | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
      | Pexp_poly (e, t) ->
          poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
      | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
      | Pexp_newtype (s, e) -> newtype ~loc ~attrs s (sub.expr sub e)
      | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
      | Pexp_open (ovf, lid, e) ->
          open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
      | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
      | Pexp_unreachable -> unreachable ~loc ~attrs ()
  end

  module P = struct
    (* Patterns *)

    let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
      let open Pat in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ppat_any -> any ~loc ~attrs ()
      | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
      | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
      | Ppat_constant c -> constant ~loc ~attrs c
      | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
      | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_construct (l, p) ->
          construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
      | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
      | Ppat_record (lpl, cf) ->
          record ~loc ~attrs
                 (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
      | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
      | Ppat_constraint (p, t) ->
          constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
      | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
      | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
      | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
      | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
      | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
  end

  module CE = struct
    (* Value expressions for the class language *)

    let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
      let open Cl in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcl_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcl_structure s ->
          structure ~loc ~attrs (sub.class_structure sub s)
      | Pcl_fun (lab, e, p, ce) ->
          fun_ ~loc ~attrs lab
            (map_opt (sub.expr sub) e)
            (sub.pat sub p)
            (sub.class_expr sub ce)
      | Pcl_apply (ce, l) ->
          apply ~loc ~attrs (sub.class_expr sub ce)
            (List.map (map_snd (sub.expr sub)) l)
      | Pcl_let (r, vbs, ce) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.class_expr sub ce)
      | Pcl_constraint (ce, ct) ->
          constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
      | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_kind sub = function
      | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
      | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

    let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
      let open Cf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcf_inherit (o, ce, s) -> inherit_ ~loc ~attrs o (sub.class_expr sub ce) s
      | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
      | Pcf_method (s, p, k) ->
          method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
      | Pcf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
      | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure sub {pcstr_self; pcstr_fields} =
      {
        pcstr_self = sub.pat sub pcstr_self;
        pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
      }

    let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                           pci_loc; pci_attributes} =
      Ci.mk
       ~virt:pci_virt
       ~params:(List.map (map_fst (sub.typ sub)) pl)
        (map_loc sub pci_name)
        (f pci_expr)
        ~loc:(sub.location sub pci_loc)
        ~attrs:(sub.attributes sub pci_attributes)
  end

  (* Now, a generic AST mapper, to be extended to cover all kinds and
     cases of the OCaml grammar.  The default behavior of the mapper is
     the identity. *)

  let default_mapper =
    {
      structure = (fun this l -> List.map (this.structure_item this) l);
      structure_item = M.map_structure_item;
      module_expr = M.map;
      signature = (fun this l -> List.map (this.signature_item this) l);
      signature_item = MT.map_signature_item;
      module_type = MT.map;
      with_constraint = MT.map_with_constraint;
      class_declaration =
        (fun this -> CE.class_infos this (this.class_expr this));
      class_expr = CE.map;
      class_field = CE.map_field;
      class_structure = CE.map_structure;
      class_type = CT.map;
      class_type_field = CT.map_field;
      class_signature = CT.map_signature;
      class_type_declaration =
        (fun this -> CE.class_infos this (this.class_type this));
      class_description =
        (fun this -> CE.class_infos this (this.class_type this));
      type_declaration = T.map_type_declaration;
      type_kind = T.map_type_kind;
      typ = T.map;
      type_extension = T.map_type_extension;
      extension_constructor = T.map_extension_constructor;
      value_description =
        (fun this {pval_name; pval_type; pval_prim; pval_loc;
                   pval_attributes} ->
          Val.mk
            (map_loc this pval_name)
            (this.typ this pval_type)
            ~attrs:(this.attributes this pval_attributes)
            ~loc:(this.location this pval_loc)
            ~prim:pval_prim
        );

      pat = P.map;
      expr = E.map;

      module_declaration =
        (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
           Md.mk
             (map_loc this pmd_name)
             (this.module_type this pmd_type)
             ~attrs:(this.attributes this pmd_attributes)
             ~loc:(this.location this pmd_loc)
        );

      module_type_declaration =
        (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
           Mtd.mk
             (map_loc this pmtd_name)
             ?typ:(map_opt (this.module_type this) pmtd_type)
             ~attrs:(this.attributes this pmtd_attributes)
             ~loc:(this.location this pmtd_loc)
        );

      module_binding =
        (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
           Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
             ~attrs:(this.attributes this pmb_attributes)
             ~loc:(this.location this pmb_loc)
        );


      open_description =
        (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
           Opn.mk (map_loc this popen_lid)
             ~override:popen_override
             ~loc:(this.location this popen_loc)
             ~attrs:(this.attributes this popen_attributes)
        );


      include_description =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_type this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );

      include_declaration =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_expr this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );


      value_binding =
        (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
           Vb.mk
             (this.pat this pvb_pat)
             (this.expr this pvb_expr)
             ~loc:(this.location this pvb_loc)
             ~attrs:(this.attributes this pvb_attributes)
        );


      constructor_declaration =
        (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
          Type.constructor
            (map_loc this pcd_name)
            ~args:(T.map_constructor_arguments this pcd_args)
            ?res:(map_opt (this.typ this) pcd_res)
            ~loc:(this.location this pcd_loc)
            ~attrs:(this.attributes this pcd_attributes)
        );

      label_declaration =
        (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
           Type.field
             (map_loc this pld_name)
             (this.typ this pld_type)
             ~mut:pld_mutable
             ~loc:(this.location this pld_loc)
             ~attrs:(this.attributes this pld_attributes)
        );

      cases = (fun this l -> List.map (this.case this) l);
      case =
        (fun this {pc_lhs; pc_guard; pc_rhs} ->
           {
             pc_lhs = this.pat this pc_lhs;
             pc_guard = map_opt (this.expr this) pc_guard;
             pc_rhs = this.expr this pc_rhs;
           }
        );



      location = (fun _this l -> l);

      extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attributes = (fun this l -> List.map (this.attribute this) l);
      payload =
        (fun this -> function
           | PStr x -> PStr (this.structure this x)
           | PSig x -> PSig (this.signature this x)
           | PTyp x -> PTyp (this.typ this x)
           | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
        );
    }

  let rec extension_of_error {loc; msg; if_highlight; sub} =
    { loc; txt = "ocaml.error" },
    PStr ([Str.eval (Exp.constant (Pconst_string (msg, None)));
           Str.eval (Exp.constant (Pconst_string (if_highlight, None)))] @
          (List.map (fun ext -> Str.extension (extension_of_error ext)) sub))

  let attribute_of_warning loc s =
    { loc; txt = "ocaml.ppwarning" },
    PStr ([Str.eval ~loc (Exp.constant (Pconst_string (s, None)))])

end

module Outcometree = struct
  (* Module [Outcometree]: results displayed by the toplevel *)

  (* These types represent messages that the toplevel displays as normal
     results or errors. The real displaying is customisable using the hooks:
        [Toploop.print_out_value]
        [Toploop.print_out_type]
        [Toploop.print_out_sig_item]
        [Toploop.print_out_phrase] *)

  type out_ident (*IF_CURRENT = Outcometree.out_ident *) =
    | Oide_apply of out_ident * out_ident
    | Oide_dot of out_ident * string
    | Oide_ident of string

  type out_attribute (*IF_CURRENT = Outcometree.out_attribute *) =
    { oattr_name: string }

  type out_value (*IF_CURRENT = Outcometree.out_value *) =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option

  type out_type (*IF_CURRENT = Outcometree.out_type *) =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type * string
    | Otyp_arrow of string * out_type * out_type
    | Otyp_class of bool * out_ident * out_type list
    | Otyp_constr of out_ident * out_type list
    | Otyp_manifest of out_type * out_type
    | Otyp_object of (string * out_type) list * bool option
    | Otyp_record of (string * bool * out_type) list
    | Otyp_stuff of string
    | Otyp_sum of (string * out_type list * out_type option) list
    | Otyp_tuple of out_type list
    | Otyp_var of bool * string
    | Otyp_variant of
        bool * out_variant * bool * (string list) option
    | Otyp_poly of string list * out_type
    | Otyp_module of string * string list * out_type list
    | Otyp_attribute of out_type * out_attribute

  and out_variant (*IF_CURRENT = Outcometree.out_variant *) =
    | Ovar_fields of (string * bool * out_type list) list
    | Ovar_name of out_ident * out_type list

  type out_class_type (*IF_CURRENT = Outcometree.out_class_type *) =
    | Octy_constr of out_ident * out_type list
    | Octy_arrow of string * out_type * out_class_type
    | Octy_signature of out_type option * out_class_sig_item list
  and out_class_sig_item (*IF_CURRENT = Outcometree.out_class_sig_item *) =
    | Ocsg_constraint of out_type * out_type
    | Ocsg_method of string * bool * bool * out_type
    | Ocsg_value of string * bool * bool * out_type

  type out_module_type (*IF_CURRENT = Outcometree.out_module_type *) =
    | Omty_abstract
    | Omty_functor of string * out_module_type option * out_module_type
    | Omty_ident of out_ident
    | Omty_signature of out_sig_item list
    | Omty_alias of out_ident
  and out_sig_item (*IF_CURRENT = Outcometree.out_sig_item *) =
    | Osig_class of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_class_type of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_typext of out_extension_constructor * out_ext_status
    | Osig_modtype of string * out_module_type
    | Osig_module of string * out_module_type * out_rec_status
    | Osig_type of out_type_decl * out_rec_status
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl (*IF_CURRENT = Outcometree.out_type_decl *) =
    { otype_name: string;
      otype_params: (string * (bool * bool)) list;
      otype_type: out_type;
      otype_private: Asttypes.private_flag;
      otype_immediate: bool;
      otype_cstrs: (out_type * out_type) list }
  and out_extension_constructor (*IF_CURRENT = Outcometree.out_extension_constructor *) =
    { oext_name: string;
      oext_type_name: string;
      oext_type_params: string list;
      oext_args: out_type list;
      oext_ret_type: out_type option;
      oext_private: Asttypes.private_flag }
  and out_type_extension (*IF_CURRENT = Outcometree.out_type_extension *) =
    { otyext_name: string;
      otyext_params: string list;
      otyext_constructors: (string * out_type list * out_type option) list;
      otyext_private: Asttypes.private_flag }
  and out_val_decl (*IF_CURRENT = Outcometree.out_val_decl *) =
    { oval_name: string;
      oval_type: out_type;
      oval_prims: string list;
      oval_attributes: out_attribute list }
  and out_rec_status (*IF_CURRENT = Outcometree.out_rec_status *) =
    | Orec_not
    | Orec_first
    | Orec_next
  and out_ext_status (*IF_CURRENT = Outcometree.out_ext_status *) =
    | Oext_first
    | Oext_next
    | Oext_exception

  type out_phrase (*IF_CURRENT = Outcometree.out_phrase *) =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)

end

module Config = struct
  let ast_impl_magic_number = "Caml1999M019"
  let ast_intf_magic_number = "Caml1999N018"
end

let map_signature mapper = mapper.Ast_mapper.signature mapper
let map_structure mapper = mapper.Ast_mapper.structure mapper

let failing_mapper =
  let fail _ _ =
    invalid_arg "failing_mapper: this mapper function should never get called"
  in
  {
    Ast_mapper.
    structure               = fail;
    structure_item          = fail;
    module_expr             = fail;
    signature               = fail;
    signature_item          = fail;
    module_type             = fail;
    with_constraint         = fail;
    class_declaration       = fail;
    class_expr              = fail;
    class_field             = fail;
    class_structure         = fail;
    class_type              = fail;
    class_type_field        = fail;
    class_signature         = fail;
    class_type_declaration  = fail;
    class_description       = fail;
    type_declaration        = fail;
    type_kind               = fail;
    typ                     = fail;
    type_extension          = fail;
    extension_constructor   = fail;
    value_description       = fail;
    pat                     = fail;
    expr                    = fail;
    module_declaration      = fail;
    module_type_declaration = fail;
    module_binding          = fail;
    open_description        = fail;
    include_description     = fail;
    include_declaration     = fail;
    value_binding           = fail;
    constructor_declaration = fail;
    label_declaration       = fail;
    cases                   = fail;
    case                    = fail;
    location                = fail;
    extension               = fail;
    attribute               = fail;
    attributes              = fail;
    payload                 = fail;
  }

let make_top_mapper ~signature ~structure =
  {failing_mapper with Ast_mapper.
                    signature = (fun _ x -> signature x);
                    structure = (fun _ x -> structure x) }

end
module Ast_405
= struct
#1 "ast_405.ml"
# 1 "src/ast_405.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*            Jérémie Dimino and Leo White, Jane Street Europe            *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                         Alain Frisch, LexiFi                           *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Location = Location
module Longident = Longident

module Asttypes = struct
  (** Auxiliary AST types used by parsetree and typedtree. *)

  type constant (*IF_CURRENT = Asttypes.constant *) =
      Const_int of int
    | Const_char of char
    | Const_string of string * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint

  type rec_flag (*IF_CURRENT = Asttypes.rec_flag *) = Nonrecursive | Recursive

  type direction_flag (*IF_CURRENT = Asttypes.direction_flag *) = Upto | Downto

  (* Order matters, used in polymorphic comparison *)
  type private_flag (*IF_CURRENT = Asttypes.private_flag *) = Private | Public

  type mutable_flag (*IF_CURRENT = Asttypes.mutable_flag *) = Immutable | Mutable

  type virtual_flag (*IF_CURRENT = Asttypes.virtual_flag *) = Virtual | Concrete

  type override_flag (*IF_CURRENT = Asttypes.override_flag *) = Override | Fresh

  type closed_flag (*IF_CURRENT = Asttypes.closed_flag *) = Closed | Open

  type label = string

  type arg_label (*IF_CURRENT = Asttypes.arg_label *) =
      Nolabel
    | Labelled of string (*  label:T -> ... *)
    | Optional of string (* ?label:T -> ... *)

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }


  type variance (*IF_CURRENT = Asttypes.variance *) =
    | Covariant
    | Contravariant
    | Invariant
end

module Parsetree = struct
  (** Abstract syntax tree produced by parsing *)

  open Asttypes

  type constant (*IF_CURRENT = Parsetree.constant *) =
      Pconst_integer of string * char option
    (* 3 3l 3L 3n

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
    *)
    | Pconst_char of char
    (* 'c' *)
    | Pconst_string of string * string option
    (* "constant"
       {delim|other constant|delim}
    *)
    | Pconst_float of string * char option
    (* 3.4 2e5 1.4e-4

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes are rejected by the typechecker.
    *)

  (** {2 Extension points} *)

  type attribute = string loc * payload
         (* [@id ARG]
            [@@id ARG]

            Metadata containers passed around within the AST.
            The compiler ignores unknown attributes.
         *)

  and extension = string loc * payload
        (* [%id ARG]
           [%%id ARG]

           Sub-language placeholder -- rejected by the typechecker.
        *)

  and attributes = attribute list

  and payload (*IF_CURRENT = Parsetree.payload *) =
    | PStr of structure
    | PSig of signature (* : SIG *)
    | PTyp of core_type  (* : T *)
    | PPat of pattern * expression option  (* ? P  or  ? P when E *)

  (** {2 Core language} *)

  (* Type expressions *)

  and core_type (*IF_CURRENT = Parsetree.core_type *) =
      {
       ptyp_desc: core_type_desc;
       ptyp_loc: Location.t;
       ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and core_type_desc (*IF_CURRENT = Parsetree.core_type_desc *) =
    | Ptyp_any
          (*  _ *)
    | Ptyp_var of string
          (* 'a *)
    | Ptyp_arrow of arg_label * core_type * core_type
          (* T1 -> T2       Simple
             ~l:T1 -> T2    Labelled
             ?l:T1 -> T2    Otional
           *)
    | Ptyp_tuple of core_type list
          (* T1 * ... * Tn

             Invariant: n >= 2
          *)
    | Ptyp_constr of Longident.t loc * core_type list
          (* tconstr
             T tconstr
             (T1, ..., Tn) tconstr
           *)
    | Ptyp_object of (string loc * attributes * core_type) list * closed_flag
          (* < l1:T1; ...; ln:Tn >     (flag = Closed)
             < l1:T1; ...; ln:Tn; .. > (flag = Open)
           *)
    | Ptyp_class of Longident.t loc * core_type list
          (* #tconstr
             T #tconstr
             (T1, ..., Tn) #tconstr
           *)
    | Ptyp_alias of core_type * string
          (* T as 'a *)
    | Ptyp_variant of row_field list * closed_flag * label list option
          (* [ `A|`B ]         (flag = Closed; labels = None)
             [> `A|`B ]        (flag = Open;   labels = None)
             [< `A|`B ]        (flag = Closed; labels = Some [])
             [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
           *)
    | Ptyp_poly of string loc list * core_type
          (* 'a1 ... 'an. T

             Can only appear in the following context:

             - As the core_type of a Ppat_constraint node corresponding
               to a constraint on a let-binding: let x : 'a1 ... 'an. T
               = e ...

             - Under Cfk_virtual for methods (not values).

             - As the core_type of a Pctf_method node.

             - As the core_type of a Pexp_poly node.

             - As the pld_type field of a label_declaration.

             - As a core_type of a Ptyp_object node.
           *)

    | Ptyp_package of package_type
          (* (module S) *)
    | Ptyp_extension of extension
          (* [%id] *)

  and package_type = Longident.t loc * (Longident.t loc * core_type) list
        (*
          (module S)
          (module S with type t1 = T1 and ... and tn = Tn)
         *)

  and row_field (*IF_CURRENT = Parsetree.row_field *) =
    | Rtag of label * attributes * bool * core_type list
          (* [`A]                   ( true,  [] )
             [`A of T]              ( false, [T] )
             [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
             [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

            - The 2nd field is true if the tag contains a
              constant (empty) constructor.
            - '&' occurs when several types are used for the same constructor
              (see 4.2 in the manual)

            - TODO: switch to a record representation, and keep location
          *)
    | Rinherit of core_type
          (* [ T ] *)

  (* Patterns *)

  and pattern (*IF_CURRENT = Parsetree.pattern *) =
      {
       ppat_desc: pattern_desc;
       ppat_loc: Location.t;
       ppat_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and pattern_desc (*IF_CURRENT = Parsetree.pattern_desc *) =
    | Ppat_any
          (* _ *)
    | Ppat_var of string loc
          (* x *)
    | Ppat_alias of pattern * string loc
          (* P as 'a *)
    | Ppat_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Ppat_interval of constant * constant
          (* 'a'..'z'

             Other forms of interval are recognized by the parser
             but rejected by the type-checker. *)
    | Ppat_tuple of pattern list
          (* (P1, ..., Pn)

             Invariant: n >= 2
          *)
    | Ppat_construct of Longident.t loc * pattern option
          (* C                None
             C P              Some P
             C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
           *)
    | Ppat_variant of label * pattern option
          (* `A             (None)
             `A P           (Some P)
           *)
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
          (* { l1=P1; ...; ln=Pn }     (flag = Closed)
             { l1=P1; ...; ln=Pn; _}   (flag = Open)

             Invariant: n > 0
           *)
    | Ppat_array of pattern list
          (* [| P1; ...; Pn |] *)
    | Ppat_or of pattern * pattern
          (* P1 | P2 *)
    | Ppat_constraint of pattern * core_type
          (* (P : T) *)
    | Ppat_type of Longident.t loc
          (* #tconst *)
    | Ppat_lazy of pattern
          (* lazy P *)
    | Ppat_unpack of string loc
          (* (module P)
             Note: (module P : S) is represented as
             Ppat_constraint(Ppat_unpack, Ptyp_package)
           *)
    | Ppat_exception of pattern
          (* exception P *)
    | Ppat_extension of extension
          (* [%id] *)
    | Ppat_open of Longident.t loc * pattern
          (* M.(P) *)

  (* Value expressions *)

  and expression (*IF_CURRENT = Parsetree.expression *) =
      {
       pexp_desc: expression_desc;
       pexp_loc: Location.t;
       pexp_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and expression_desc (*IF_CURRENT = Parsetree.expression_desc *) =
    | Pexp_ident of Longident.t loc
          (* x
             M.x
           *)
    | Pexp_constant of constant
          (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Pexp_let of rec_flag * value_binding list * expression
          (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
           *)
    | Pexp_function of case list
          (* function P1 -> E1 | ... | Pn -> En *)
    | Pexp_fun of arg_label * expression option * pattern * expression
          (* fun P -> E1                          (Simple, None)
             fun ~l:P -> E1                       (Labelled l, None)
             fun ?l:P -> E1                       (Optional l, None)
             fun ?l:(P = E0) -> E1                (Optional l, Some E0)

             Notes:
             - If E0 is provided, only Optional is allowed.
             - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
             - "let f P = E" is represented using Pexp_fun.
           *)
    | Pexp_apply of expression * (arg_label * expression) list
          (* E0 ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pexp_match of expression * case list
          (* match E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_try of expression * case list
          (* try E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_tuple of expression list
          (* (E1, ..., En)

             Invariant: n >= 2
          *)
    | Pexp_construct of Longident.t loc * expression option
          (* C                None
             C E              Some E
             C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
          *)
    | Pexp_variant of label * expression option
          (* `A             (None)
             `A E           (Some E)
           *)
    | Pexp_record of (Longident.t loc * expression) list * expression option
          (* { l1=P1; ...; ln=Pn }     (None)
             { E0 with l1=P1; ...; ln=Pn }   (Some E0)

             Invariant: n > 0
           *)
    | Pexp_field of expression * Longident.t loc
          (* E.l *)
    | Pexp_setfield of expression * Longident.t loc * expression
          (* E1.l <- E2 *)
    | Pexp_array of expression list
          (* [| E1; ...; En |] *)
    | Pexp_ifthenelse of expression * expression * expression option
          (* if E1 then E2 else E3 *)
    | Pexp_sequence of expression * expression
          (* E1; E2 *)
    | Pexp_while of expression * expression
          (* while E1 do E2 done *)
    | Pexp_for of
        pattern *  expression * expression * direction_flag * expression
          (* for i = E1 to E2 do E3 done      (flag = Upto)
             for i = E1 downto E2 do E3 done  (flag = Downto)
           *)
    | Pexp_constraint of expression * core_type
          (* (E : T) *)
    | Pexp_coerce of expression * core_type option * core_type
          (* (E :> T)        (None, T)
             (E : T0 :> T)   (Some T0, T)
           *)
    | Pexp_send of expression * string loc
          (*  E # m *)
    | Pexp_new of Longident.t loc
          (* new M.c *)
    | Pexp_setinstvar of string loc * expression
          (* x <- 2 *)
    | Pexp_override of (string loc * expression) list
          (* {< x1 = E1; ...; Xn = En >} *)
    | Pexp_letmodule of string loc * module_expr * expression
          (* let module M = ME in E *)
    | Pexp_letexception of extension_constructor * expression
          (* let exception C in E *)
    | Pexp_assert of expression
          (* assert E
             Note: "assert false" is treated in a special way by the
             type-checker. *)
    | Pexp_lazy of expression
          (* lazy E *)
    | Pexp_poly of expression * core_type option
          (* Used for method bodies.

             Can only be used as the expression under Cfk_concrete
             for methods (not values). *)
    | Pexp_object of class_structure
          (* object ... end *)
    | Pexp_newtype of string loc * expression
          (* fun (type t) -> E *)
    | Pexp_pack of module_expr
          (* (module ME)

             (module ME : S) is represented as
             Pexp_constraint(Pexp_pack, Ptyp_package S) *)
    | Pexp_open of override_flag * Longident.t loc * expression
          (* M.(E)
             let open M in E
             let! open M in E *)
    | Pexp_extension of extension
          (* [%id] *)
    | Pexp_unreachable
          (* . *)

  and case (*IF_CURRENT = Parsetree.case *) =   (* (P -> E) or (P when E0 -> E) *)
      {
       pc_lhs: pattern;
       pc_guard: expression option;
       pc_rhs: expression;
      }

  (* Value descriptions *)

  and value_description (*IF_CURRENT = Parsetree.value_description *) =
      {
       pval_name: string loc;
       pval_type: core_type;
       pval_prim: string list;
       pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
       pval_loc: Location.t;
      }

  (*
    val x: T                            (prim = [])
    external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
  *)

  (* Type declarations *)

  and type_declaration (*IF_CURRENT = Parsetree.type_declaration *) =
      {
       ptype_name: string loc;
       ptype_params: (core_type * variance) list;
             (* ('a1,...'an) t; None represents  _*)
       ptype_cstrs: (core_type * core_type * Location.t) list;
             (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
       ptype_kind: type_kind;
       ptype_private: private_flag;   (* = private ... *)
       ptype_manifest: core_type option;  (* = T *)
       ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
       ptype_loc: Location.t;
      }

  (*
    type t                     (abstract, no manifest)
    type t = T0                (abstract, manifest=T0)
    type t = C of T | ...      (variant,  no manifest)
    type t = T0 = C of T | ... (variant,  manifest=T0)
    type t = {l: T; ...}       (record,   no manifest)
    type t = T0 = {l : T; ...} (record,   manifest=T0)
    type t = ..                (open,     no manifest)
  *)

  and type_kind (*IF_CURRENT = Parsetree.type_kind *) =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
          (* Invariant: non-empty list *)
    | Ptype_record of label_declaration list
          (* Invariant: non-empty list *)
    | Ptype_open

  and label_declaration (*IF_CURRENT = Parsetree.label_declaration *) =
      {
       pld_name: string loc;
       pld_mutable: mutable_flag;
       pld_type: core_type;
       pld_loc: Location.t;
       pld_attributes: attributes; (* l [@id1] [@id2] : T *)
      }

  (*  { ...; l: T; ... }            (mutable=Immutable)
      { ...; mutable l: T; ... }    (mutable=Mutable)

      Note: T can be a Ptyp_poly.
  *)

  and constructor_declaration (*IF_CURRENT = Parsetree.constructor_declaration *) =
      {
       pcd_name: string loc;
       pcd_args: constructor_arguments;
       pcd_res: core_type option;
       pcd_loc: Location.t;
       pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }

  and constructor_arguments (*IF_CURRENT = Parsetree.constructor_arguments *) =
    | Pcstr_tuple of core_type list
    | Pcstr_record of label_declaration list

  (*
    | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
    | C: T0                  (res = Some T0, args = [])
    | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
    | C of {...}             (res = None,    args = Pcstr_record)
    | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
    | C of {...} as t        (res = None,    args = Pcstr_record)
  *)

  and type_extension (*IF_CURRENT = Parsetree.type_extension *) =
      {
       ptyext_path: Longident.t loc;
       ptyext_params: (core_type * variance) list;
       ptyext_constructors: extension_constructor list;
       ptyext_private: private_flag;
       ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
      }
  (*
    type t += ...
  *)

  and extension_constructor (*IF_CURRENT = Parsetree.extension_constructor *) =
      {
       pext_name: string loc;
       pext_kind : extension_constructor_kind;
       pext_loc : Location.t;
       pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
      }

  and extension_constructor_kind (*IF_CURRENT = Parsetree.extension_constructor_kind *) =
      Pext_decl of constructor_arguments * core_type option
        (*
           | C of T1 * ... * Tn     ([T1; ...; Tn], None)
           | C: T0                  ([], Some T0)
           | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
         *)
    | Pext_rebind of Longident.t loc
        (*
           | C = D
         *)

  (** {2 Class language} *)

  (* Type expressions for the class language *)

  and class_type (*IF_CURRENT = Parsetree.class_type *) =
      {
       pcty_desc: class_type_desc;
       pcty_loc: Location.t;
       pcty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_type_desc (*IF_CURRENT = Parsetree.class_type_desc *) =
    | Pcty_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcty_signature of class_signature
          (* object ... end *)
    | Pcty_arrow of arg_label * core_type * class_type
          (* T -> CT       Simple
             ~l:T -> CT    Labelled l
             ?l:T -> CT    Optional l
           *)
    | Pcty_extension of extension
          (* [%id] *)

  and class_signature (*IF_CURRENT = Parsetree.class_signature *) =
      {
       pcsig_self: core_type;
       pcsig_fields: class_type_field list;
      }
  (* object('selfpat) ... end
     object ... end             (self = Ptyp_any)
   *)

  and class_type_field (*IF_CURRENT = Parsetree.class_type_field *) =
      {
       pctf_desc: class_type_field_desc;
       pctf_loc: Location.t;
       pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_type_field_desc (*IF_CURRENT = Parsetree.class_type_field_desc *) =
    | Pctf_inherit of class_type
          (* inherit CT *)
    | Pctf_val of (string loc * mutable_flag * virtual_flag * core_type)
          (* val x: T *)
    | Pctf_method  of (string loc * private_flag * virtual_flag * core_type)
          (* method x: T

             Note: T can be a Ptyp_poly.
           *)
    | Pctf_constraint  of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pctf_attribute of attribute
          (* [@@@id] *)
    | Pctf_extension of extension
          (* [%%id] *)

  and 'a class_infos (*IF_CURRENT = 'a Parsetree.class_infos *) =
      {
       pci_virt: virtual_flag;
       pci_params: (core_type * variance) list;
       pci_name: string loc;
       pci_expr: 'a;
       pci_loc: Location.t;
       pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
      }
  (* class c = ...
     class ['a1,...,'an] c = ...
     class virtual c = ...

     Also used for "class type" declaration.
  *)

  and class_description = class_type class_infos

  and class_type_declaration = class_type class_infos

  (* Value expressions for the class language *)

  and class_expr (*IF_CURRENT = Parsetree.class_expr *) =
      {
       pcl_desc: class_expr_desc;
       pcl_loc: Location.t;
       pcl_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and class_expr_desc (*IF_CURRENT = Parsetree.class_expr_desc *) =
    | Pcl_constr of Longident.t loc * core_type list
          (* c
             ['a1, ..., 'an] c *)
    | Pcl_structure of class_structure
          (* object ... end *)
    | Pcl_fun of arg_label * expression option * pattern * class_expr
          (* fun P -> CE                          (Simple, None)
             fun ~l:P -> CE                       (Labelled l, None)
             fun ?l:P -> CE                       (Optional l, None)
             fun ?l:(P = E0) -> CE                (Optional l, Some E0)
           *)
    | Pcl_apply of class_expr * (arg_label * expression) list
          (* CE ~l1:E1 ... ~ln:En
             li can be empty (non labeled argument) or start with '?'
             (optional argument).

             Invariant: n > 0
           *)
    | Pcl_let of rec_flag * value_binding list * class_expr
          (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
           *)
    | Pcl_constraint of class_expr * class_type
          (* (CE : CT) *)
    | Pcl_extension of extension
          (* [%id] *)

  and class_structure (*IF_CURRENT = Parsetree.class_structure *) =
      {
       pcstr_self: pattern;
       pcstr_fields: class_field list;
      }
  (* object(selfpat) ... end
     object ... end           (self = Ppat_any)
   *)

  and class_field (*IF_CURRENT = Parsetree.class_field *) =
      {
       pcf_desc: class_field_desc;
       pcf_loc: Location.t;
       pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
      }

  and class_field_desc (*IF_CURRENT = Parsetree.class_field_desc *) =
    | Pcf_inherit of override_flag * class_expr * string loc option
          (* inherit CE
             inherit CE as x
             inherit! CE
             inherit! CE as x
           *)
    | Pcf_val of (string loc * mutable_flag * class_field_kind)
          (* val x = E
             val virtual x: T
           *)
    | Pcf_method of (string loc * private_flag * class_field_kind)
          (* method x = E            (E can be a Pexp_poly)
             method virtual x: T     (T can be a Ptyp_poly)
           *)
    | Pcf_constraint of (core_type * core_type)
          (* constraint T1 = T2 *)
    | Pcf_initializer of expression
          (* initializer E *)
    | Pcf_attribute of attribute
          (* [@@@id] *)
    | Pcf_extension of extension
          (* [%%id] *)

  and class_field_kind (*IF_CURRENT = Parsetree.class_field_kind *) =
    | Cfk_virtual of core_type
    | Cfk_concrete of override_flag * expression

  and class_declaration = class_expr class_infos

  (** {2 Module language} *)

  (* Type expressions for the module language *)

  and module_type (*IF_CURRENT = Parsetree.module_type *) =
      {
       pmty_desc: module_type_desc;
       pmty_loc: Location.t;
       pmty_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_type_desc (*IF_CURRENT = Parsetree.module_type_desc *) =
    | Pmty_ident of Longident.t loc
          (* S *)
    | Pmty_signature of signature
          (* sig ... end *)
    | Pmty_functor of string loc * module_type option * module_type
          (* functor(X : MT1) -> MT2 *)
    | Pmty_with of module_type * with_constraint list
          (* MT with ... *)
    | Pmty_typeof of module_expr
          (* module type of ME *)
    | Pmty_extension of extension
          (* [%id] *)
    | Pmty_alias of Longident.t loc
          (* (module M) *)

  and signature = signature_item list

  and signature_item (*IF_CURRENT = Parsetree.signature_item *) =
      {
       psig_desc: signature_item_desc;
       psig_loc: Location.t;
      }

  and signature_item_desc (*IF_CURRENT = Parsetree.signature_item_desc *) =
    | Psig_value of value_description
          (*
            val x: T
            external x: T = "s1" ... "sn"
           *)
    | Psig_type of rec_flag * type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Psig_typext of type_extension
          (* type t1 += ... *)
    | Psig_exception of extension_constructor
          (* exception C of T *)
    | Psig_module of module_declaration
          (* module X : MT *)
    | Psig_recmodule of module_declaration list
          (* module rec X1 : MT1 and ... and Xn : MTn *)
    | Psig_modtype of module_type_declaration
          (* module type S = MT
             module type S *)
    | Psig_open of open_description
          (* open X *)
    | Psig_include of include_description
          (* include MT *)
    | Psig_class of class_description list
          (* class c1 : ... and ... and cn : ... *)
    | Psig_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Psig_attribute of attribute
          (* [@@@id] *)
    | Psig_extension of extension * attributes
          (* [%%id] *)

  and module_declaration (*IF_CURRENT = Parsetree.module_declaration *) =
      {
       pmd_name: string loc;
       pmd_type: module_type;
       pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmd_loc: Location.t;
      }
  (* S : MT *)

  and module_type_declaration (*IF_CURRENT = Parsetree.module_type_declaration *) =
      {
       pmtd_name: string loc;
       pmtd_type: module_type option;
       pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
       pmtd_loc: Location.t;
      }
  (* S = MT
     S       (abstract module type declaration, pmtd_type = None)
  *)

  and open_description (*IF_CURRENT = Parsetree.open_description *) =
      {
       popen_lid: Longident.t loc;
       popen_override: override_flag;
       popen_loc: Location.t;
       popen_attributes: attributes;
      }
  (* open! X - popen_override = Override (silences the 'used identifier
                                shadowing' warning)
     open  X - popen_override = Fresh
   *)

  and 'a include_infos (*IF_CURRENT = 'a Parsetree.include_infos *) =
      {
       pincl_mod: 'a;
       pincl_loc: Location.t;
       pincl_attributes: attributes;
      }

  and include_description = module_type include_infos
  (* include MT *)

  and include_declaration = module_expr include_infos
  (* include ME *)

  and with_constraint (*IF_CURRENT = Parsetree.with_constraint *) =
    | Pwith_type of Longident.t loc * type_declaration
          (* with type X.t = ...

             Note: the last component of the longident must match
             the name of the type_declaration. *)
    | Pwith_module of Longident.t loc * Longident.t loc
          (* with module X.Y = Z *)
    | Pwith_typesubst of type_declaration
          (* with type t := ... *)
    | Pwith_modsubst of string loc * Longident.t loc
          (* with module X := Z *)

  (* Value expressions for the module language *)

  and module_expr (*IF_CURRENT = Parsetree.module_expr *) =
      {
       pmod_desc: module_expr_desc;
       pmod_loc: Location.t;
       pmod_attributes: attributes; (* ... [@id1] [@id2] *)
      }

  and module_expr_desc (*IF_CURRENT = Parsetree.module_expr_desc *) =
    | Pmod_ident of Longident.t loc
          (* X *)
    | Pmod_structure of structure
          (* struct ... end *)
    | Pmod_functor of string loc * module_type option * module_expr
          (* functor(X : MT1) -> ME *)
    | Pmod_apply of module_expr * module_expr
          (* ME1(ME2) *)
    | Pmod_constraint of module_expr * module_type
          (* (ME : MT) *)
    | Pmod_unpack of expression
          (* (val E) *)
    | Pmod_extension of extension
          (* [%id] *)

  and structure = structure_item list

  and structure_item (*IF_CURRENT = Parsetree.structure_item *) =
      {
       pstr_desc: structure_item_desc;
       pstr_loc: Location.t;
      }

  and structure_item_desc (*IF_CURRENT = Parsetree.structure_item_desc *) =
    | Pstr_eval of expression * attributes
          (* E *)
    | Pstr_value of rec_flag * value_binding list
          (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
             let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
           *)
    | Pstr_primitive of value_description
          (*  val x: T
              external x: T = "s1" ... "sn" *)
    | Pstr_type of rec_flag * type_declaration list
          (* type t1 = ... and ... and tn = ... *)
    | Pstr_typext of type_extension
          (* type t1 += ... *)
    | Pstr_exception of extension_constructor
          (* exception C of T
             exception C = M.X *)
    | Pstr_module of module_binding
          (* module X = ME *)
    | Pstr_recmodule of module_binding list
          (* module rec X1 = ME1 and ... and Xn = MEn *)
    | Pstr_modtype of module_type_declaration
          (* module type S = MT *)
    | Pstr_open of open_description
          (* open X *)
    | Pstr_class of class_declaration list
          (* class c1 = ... and ... and cn = ... *)
    | Pstr_class_type of class_type_declaration list
          (* class type ct1 = ... and ... and ctn = ... *)
    | Pstr_include of include_declaration
          (* include ME *)
    | Pstr_attribute of attribute
          (* [@@@id] *)
    | Pstr_extension of extension * attributes
          (* [%%id] *)

  and value_binding (*IF_CURRENT = Parsetree.value_binding *) =
    {
      pvb_pat: pattern;
      pvb_expr: expression;
      pvb_attributes: attributes;
      pvb_loc: Location.t;
    }

  and module_binding (*IF_CURRENT = Parsetree.module_binding *) =
      {
       pmb_name: string loc;
       pmb_expr: module_expr;
       pmb_attributes: attributes;
       pmb_loc: Location.t;
      }
  (* X = ME *)

  (** {2 Toplevel} *)

  (* Toplevel phrases *)

  type toplevel_phrase (*IF_CURRENT = Parsetree.toplevel_phrase *) =
    | Ptop_def of structure
    | Ptop_dir of string * directive_argument
       (* #use, #load ... *)

  and directive_argument (*IF_CURRENT = Parsetree.directive_argument *) =
    | Pdir_none
    | Pdir_string of string
    | Pdir_int of string * char option
    | Pdir_ident of Longident.t
    | Pdir_bool of bool

end

module Docstrings : sig
  (** {3 Docstrings} *)

  (** Documentation comments *)
  type docstring

  (** Create a docstring *)
  val docstring : string -> Location.t -> docstring

  (** Get the text of a docstring *)
  val docstring_body : docstring -> string

  (** Get the location of a docstring *)
  val docstring_loc : docstring -> Location.t

  (** {3 Items}

      The {!docs} type represents documentation attached to an item. *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  val empty_docs : docs

  val docs_attr : docstring -> Parsetree.attribute

  (** Convert item documentation to attributes and add them to an
      attribute list *)
  val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Fields and constructors}

      The {!info} type represents documentation attached to a field or
      constructor. *)

  type info = docstring option

  val empty_info : info

  val info_attr : docstring -> Parsetree.attribute

  (** Convert field info to attributes and add them to an
      attribute list *)
  val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

  (** {3 Unattached comments}

      The {!text} type represents documentation which is not attached to
      anything. *)

  type text = docstring list

  val empty_text : text

  val text_attr : docstring -> Parsetree.attribute

  (** Convert text to attributes and add them to an attribute list *)
  val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

end = struct
  open Location

  (* Docstrings *)

  type docstring =
    { ds_body: string;
      ds_loc: Location.t;
    }

  (* Docstring constructors and destructors *)

  let docstring body loc =
    let ds =
      { ds_body = body;
        ds_loc = loc;
      }
    in
    ds

  let docstring_body ds = ds.ds_body

  let docstring_loc ds = ds.ds_loc

  (* Docstrings attached to items *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  let empty_docs = { docs_pre = None; docs_post = None }

  let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

  let docs_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (doc_loc, PStr [item])

  let add_docs_attrs docs attrs =
    let attrs =
      match docs.docs_pre with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> docs_attr ds :: attrs
    in
    let attrs =
      match docs.docs_post with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> attrs @ [docs_attr ds]
    in
    attrs

  (* Docstrings attached to constructors or fields *)

  type info = docstring option

  let empty_info = None

  let info_attr = docs_attr

  let add_info_attrs info attrs =
    match info with
    | None | Some {ds_body=""; _} -> attrs
    | Some ds -> attrs @ [info_attr ds]

  (* Docstrings not attached to a specific item *)

  type text = docstring list

  let empty_text = []

  let text_loc = {txt = "ocaml.text"; loc = Location.none}

  let text_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
        pexp_loc = ds.ds_loc;
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
      (text_loc, PStr [item])

  let add_text_attrs dsl attrs =
    let fdsl = List.filter (function {ds_body=""; _} -> false| _ ->true) dsl in
    (List.map text_attr fdsl) @ attrs

end

module Ast_helper : sig

  (** Helpers to produce Parsetree fragments *)

  open Asttypes
  open Docstrings
  open Parsetree

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  (** {2 Default locations} *)

  val default_loc: loc ref
      (** Default value for all optional location arguments. *)

  val with_default_loc: loc -> (unit -> 'a) -> 'a
      (** Set the [default_loc] within the scope of the execution
          of the provided function. *)

  (** {2 Constants} *)

  module Const : sig
    val char : char -> constant
    val string : ?quotation_delimiter:string -> string -> constant
    val integer : ?suffix:char -> string -> constant
    val int : ?suffix:char -> int -> constant
    val int32 : ?suffix:char -> int32 -> constant
    val int64 : ?suffix:char -> int64 -> constant
    val nativeint : ?suffix:char -> nativeint -> constant
    val float : ?suffix:char -> string -> constant
  end

  (** {2 Core language} *)

  (** Type expressions *)
  module Typ :
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
      val attr: core_type -> attribute -> core_type

      val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
      val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
      val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type -> core_type
                 -> core_type
      val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val object_: ?loc:loc -> ?attrs:attrs ->
                    (str * attributes * core_type) list -> closed_flag ->
                    core_type
      val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
      val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
      val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
                   -> label list option -> core_type
      val poly: ?loc:loc -> ?attrs:attrs -> str list -> core_type -> core_type
      val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
                   -> core_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

      val force_poly: core_type -> core_type

      val varify_constructors: str list -> core_type -> core_type
      (** [varify_constructors newtypes te] is type expression [te], of which
          any of nullary type constructor [tc] is replaced by type variable of
          the same name, if [tc]'s name appears in [newtypes].
          Raise [Syntaxerr.Variable_in_scope] if any type variable inside [te]
          appears in [newtypes].
          @since 4.05
       *)
    end

  (** Patterns *)
  module Pat:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
      val attr:pattern -> attribute -> pattern

      val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
      val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
      val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
      val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
      val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
      val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
                  -> pattern
      val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
      val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
      val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
      val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
      val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val unpack: ?loc:loc -> ?attrs:attrs -> str -> pattern
      val open_: ?loc:loc -> ?attrs:attrs  -> lid -> pattern -> pattern
      val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
    end

  (** Expressions *)
  module Exp:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
      val attr: expression -> attribute -> expression

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
                -> expression -> expression
      val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option
                -> pattern -> expression -> expression
      val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
      val apply: ?loc:loc -> ?attrs:attrs -> expression
                 -> (arg_label * expression) list -> expression
      val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
                  -> expression
      val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
      val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                     -> expression
      val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
                   -> expression
      val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                  -> expression option -> expression
      val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
      val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                    -> expression
      val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
      val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                      -> expression option -> expression
      val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression
      val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
      val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
                -> direction_flag -> expression -> expression
      val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                  -> core_type -> expression
      val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                       -> expression
      val send: ?loc:loc -> ?attrs:attrs -> expression -> str -> expression
      val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
      val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
      val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                    -> expression
      val letmodule: ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression
                     -> expression
      val letexception:
        ?loc:loc -> ?attrs:attrs -> extension_constructor -> expression
        -> expression
      val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                -> expression
      val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
      val newtype: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
      val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
      val open_: ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> expression
                 -> expression
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
      val unreachable: ?loc:loc -> ?attrs:attrs -> unit -> expression

      val case: pattern -> ?guard:expression -> expression -> case
    end

  (** Value declarations *)
  module Val:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        ?prim:string list -> str -> core_type -> value_description
    end

  (** Type declarations *)
  module Type:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?params:(core_type * variance) list ->
        ?cstrs:(core_type * core_type * loc) list ->
        ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
        type_declaration

      val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?args:constructor_arguments -> ?res:core_type -> str ->
        constructor_declaration
      val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
        ?mut:mutable_flag -> str -> core_type -> label_declaration
    end

  (** Type extensions *)
  module Te:
    sig
      val mk: ?attrs:attrs -> ?docs:docs ->
        ?params:(core_type * variance) list -> ?priv:private_flag ->
        lid -> extension_constructor list -> type_extension

      val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> extension_constructor_kind -> extension_constructor

      val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        ?args:constructor_arguments -> ?res:core_type -> str ->
        extension_constructor
      val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
        str -> lid -> extension_constructor
    end

  (** {2 Module language} *)

  (** Module type expressions *)
  module Mty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
      val attr: module_type -> attribute -> module_type

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
      val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_type -> module_type
      val with_: ?loc:loc -> ?attrs:attrs -> module_type ->
        with_constraint list -> module_type
      val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
    end

  (** Module expressions *)
  module Mod:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
      val attr: module_expr -> attribute -> module_expr

      val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
      val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
      val functor_: ?loc:loc -> ?attrs:attrs ->
        str -> module_type option -> module_expr -> module_expr
      val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr ->
        module_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type ->
        module_expr
      val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
    end

  (** Signature items *)
  module Sig:
    sig
      val mk: ?loc:loc -> signature_item_desc -> signature_item

      val value: ?loc:loc -> value_description -> signature_item
      val type_: ?loc:loc -> rec_flag -> type_declaration list -> signature_item
      val type_extension: ?loc:loc -> type_extension -> signature_item
      val exception_: ?loc:loc -> extension_constructor -> signature_item
      val module_: ?loc:loc -> module_declaration -> signature_item
      val rec_module: ?loc:loc -> module_declaration list -> signature_item
      val modtype: ?loc:loc -> module_type_declaration -> signature_item
      val open_: ?loc:loc -> open_description -> signature_item
      val include_: ?loc:loc -> include_description -> signature_item
      val class_: ?loc:loc -> class_description list -> signature_item
      val class_type: ?loc:loc -> class_type_declaration list -> signature_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
      val attribute: ?loc:loc -> attribute -> signature_item
      val text: text -> signature_item list
    end

  (** Structure items *)
  module Str:
    sig
      val mk: ?loc:loc -> structure_item_desc -> structure_item

      val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
      val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
      val primitive: ?loc:loc -> value_description -> structure_item
      val type_: ?loc:loc -> rec_flag -> type_declaration list -> structure_item
      val type_extension: ?loc:loc -> type_extension -> structure_item
      val exception_: ?loc:loc -> extension_constructor -> structure_item
      val module_: ?loc:loc -> module_binding -> structure_item
      val rec_module: ?loc:loc -> module_binding list -> structure_item
      val modtype: ?loc:loc -> module_type_declaration -> structure_item
      val open_: ?loc:loc -> open_description -> structure_item
      val class_: ?loc:loc -> class_declaration list -> structure_item
      val class_type: ?loc:loc -> class_type_declaration list -> structure_item
      val include_: ?loc:loc -> include_declaration -> structure_item
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
      val attribute: ?loc:loc -> attribute -> structure_item
      val text: text -> structure_item list
    end

  (** Module declarations *)
  module Md:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_type -> module_declaration
    end

  (** Module type declarations *)
  module Mtd:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?typ:module_type -> str -> module_type_declaration
    end

  (** Module bindings *)
  module Mb:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        str -> module_expr -> module_binding
    end

  (** Opens *)
  module Opn:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
        ?override:override_flag -> lid -> open_description
    end

  (** Includes *)
  module Incl:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
    end

  (** Value bindings *)
  module Vb:
    sig
      val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        pattern -> expression -> value_binding
    end


  (** {2 Class language} *)

  (** Class type expressions *)
  module Cty:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
      val attr: class_type -> attribute -> class_type

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
      val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
      val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type ->
        class_type -> class_type
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    end

  (** Class type fields *)
  module Ctf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
        class_type_field_desc -> class_type_field
      val attr: class_type_field -> attribute -> class_type_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
      val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
        virtual_flag -> core_type -> class_type_field
      val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
        virtual_flag -> core_type -> class_type_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
        class_type_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
      val attribute: ?loc:loc -> attribute -> class_type_field
      val text: text -> class_type_field list
    end

  (** Class expressions *)
  module Cl:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
      val attr: class_expr -> attribute -> class_expr

      val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
      val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
      val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option ->
        pattern -> class_expr -> class_expr
      val apply: ?loc:loc -> ?attrs:attrs -> class_expr ->
        (arg_label * expression) list -> class_expr
      val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list ->
        class_expr -> class_expr
      val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type ->
        class_expr
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    end

  (** Class fields *)
  module Cf:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc ->
        class_field
      val attr: class_field -> attribute -> class_field

      val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr ->
        str option -> class_field
      val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
        class_field_kind -> class_field
      val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
        class_field_kind -> class_field
      val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
        class_field
      val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
      val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
      val attribute: ?loc:loc -> attribute -> class_field
      val text: text -> class_field list

      val virtual_: core_type -> class_field_kind
      val concrete: override_flag -> expression -> class_field_kind

    end

  (** Classes *)
  module Ci:
    sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?virt:virtual_flag -> ?params:(core_type * variance) list ->
        str -> 'a -> 'a class_infos
    end

  (** Class signatures *)
  module Csig:
    sig
      val mk: core_type -> class_type_field list -> class_signature
    end

  (** Class structures *)
  module Cstr:
    sig
      val mk: pattern -> class_field list -> class_structure
    end

end = struct
  (** Helpers to produce Parsetree fragments *)

  open Asttypes
  open Parsetree
  open Docstrings

  type lid = Longident.t loc
  type str = string loc
  type loc = Location.t
  type attrs = attribute list

  let default_loc = ref Location.none

  let with_default_loc l f =
    let old = !default_loc in
    default_loc := l;
    try let r = f () in default_loc := old; r
    with exn -> default_loc := old; raise exn

  module Const = struct
    let integer ?suffix i = Pconst_integer (i, suffix)
    let int ?suffix i = integer ?suffix (string_of_int i)
    let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
    let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
    let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
    let float ?suffix f = Pconst_float (f, suffix)
    let char c = Pconst_char c
    let string ?quotation_delimiter s = Pconst_string (s, quotation_delimiter)
  end

  module Typ = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
    let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
    let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
    let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
    let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
    let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
    let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

    let force_poly t =
      match t.ptyp_desc with
      | Ptyp_poly _ -> t
      | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)

    let varify_constructors var_names t =
      let check_variable vl loc v =
        if List.mem v vl then
          raise Syntaxerr.(Error(Variable_in_scope(loc,v))) in
      let var_names = List.map (fun v -> v.txt) var_names in
      let rec loop t =
        let desc =
          match t.ptyp_desc with
          | Ptyp_any -> Ptyp_any
          | Ptyp_var x ->
              check_variable var_names t.ptyp_loc x;
              Ptyp_var x
          | Ptyp_arrow (label,core_type,core_type') ->
              Ptyp_arrow(label, loop core_type, loop core_type')
          | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
          | Ptyp_constr( { txt = Longident.Lident s; _ }, [])
            when List.mem s var_names ->
              Ptyp_var s
          | Ptyp_constr(longident, lst) ->
              Ptyp_constr(longident, List.map loop lst)
          | Ptyp_object (lst, o) ->
              Ptyp_object
                (List.map (fun (s, attrs, t) -> (s, attrs, loop t)) lst, o)
          | Ptyp_class (longident, lst) ->
              Ptyp_class (longident, List.map loop lst)
          | Ptyp_alias(core_type, string) ->
              check_variable var_names t.ptyp_loc string;
              Ptyp_alias(loop core_type, string)
          | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
              Ptyp_variant(List.map loop_row_field row_field_list,
                           flag, lbl_lst_option)
          | Ptyp_poly(string_lst, core_type) ->
            List.iter (fun v ->
              check_variable var_names t.ptyp_loc v.txt) string_lst;
              Ptyp_poly(string_lst, loop core_type)
          | Ptyp_package(longident,lst) ->
              Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
          | Ptyp_extension (s, arg) ->
              Ptyp_extension (s, arg)
        in
        {t with ptyp_desc = desc}
      and loop_row_field  =
        function
          | Rtag(label,attrs,flag,lst) ->
              Rtag(label,attrs,flag,List.map loop lst)
          | Rinherit t ->
              Rinherit (loop t)
      in
      loop t

  end

  module Pat = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
    let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
    let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
    let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
    let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
    let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
    let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
    let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
    let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
  end

  module Exp = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
    let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
    let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
    let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
    let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
    let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
    let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
    let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
    let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
    let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
    let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
    let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
    let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
    let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
    let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
    let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
    let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
    let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
    let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
    let letexception ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letexception (a, b))
    let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
    let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
    let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
    let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
    let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
    let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable

    let case lhs ?guard rhs =
      {
       pc_lhs = lhs;
       pc_guard = guard;
       pc_rhs = rhs;
      }
  end

  module Mty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
    let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
    let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
    let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
    let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
    let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
  end

  module Mod = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
    let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

    let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
    let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
    let functor_ ?loc ?attrs arg arg_ty body =
      mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
    let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
    let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
    let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
  end

  module Sig = struct
    let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

    let value ?loc a = mk ?loc (Psig_value a)
    let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Psig_typext a)
    let exception_ ?loc a = mk ?loc (Psig_exception a)
    let module_ ?loc a = mk ?loc (Psig_module a)
    let rec_module ?loc a = mk ?loc (Psig_recmodule a)
    let modtype ?loc a = mk ?loc (Psig_modtype a)
    let open_ ?loc a = mk ?loc (Psig_open a)
    let include_ ?loc a = mk ?loc (Psig_include a)
    let class_ ?loc a = mk ?loc (Psig_class a)
    let class_type ?loc a = mk ?loc (Psig_class_type a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Psig_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Str = struct
    let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

    let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
    let value ?loc a b = mk ?loc (Pstr_value (a, b))
    let primitive ?loc a = mk ?loc (Pstr_primitive a)
    let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Pstr_typext a)
    let exception_ ?loc a = mk ?loc (Pstr_exception a)
    let module_ ?loc a = mk ?loc (Pstr_module a)
    let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
    let modtype ?loc a = mk ?loc (Pstr_modtype a)
    let open_ ?loc a = mk ?loc (Pstr_open a)
    let class_ ?loc a = mk ?loc (Pstr_class a)
    let class_type ?loc a = mk ?loc (Pstr_class_type a)
    let include_ ?loc a = mk ?loc (Pstr_include a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Pstr_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Cl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcl_desc = d;
       pcl_loc = loc;
       pcl_attributes = attrs;
      }
    let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
    let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
  end

  module Cty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
       pcty_desc = d;
       pcty_loc = loc;
       pcty_attributes = attrs;
      }
    let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
  end

  module Ctf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
             ?(docs = empty_docs) d =
      {
       pctf_desc = d;
       pctf_loc = loc;
       pctf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
    let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
    let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
    let attribute ?loc a = mk ?loc (Pctf_attribute a)
    let text txt =
     let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
       List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

  end

  module Cf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) d =
      {
       pcf_desc = d;
       pcf_loc = loc;
       pcf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
    let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
    let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
    let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
    let attribute ?loc a = mk ?loc (Pcf_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let virtual_ ct = Cfk_virtual ct
    let concrete o e = Cfk_concrete (o, e)

    let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  end

  module Val = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(prim = []) name typ =
      {
       pval_name = name;
       pval_type = typ;
       pval_attributes = add_docs_attrs docs attrs;
       pval_loc = loc;
       pval_prim = prim;
      }
  end

  module Md = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name typ =
      {
       pmd_name = name;
       pmd_type = typ;
       pmd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmd_loc = loc;
      }
  end

  module Mtd = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) ?typ name =
      {
       pmtd_name = name;
       pmtd_type = typ;
       pmtd_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmtd_loc = loc;
      }
  end

  module Mb = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = []) name expr =
      {
       pmb_name = name;
       pmb_expr = expr;
       pmb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pmb_loc = loc;
      }
  end

  module Opn = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(override = Fresh) lid =
      {
       popen_lid = lid;
       popen_override = override;
       popen_loc = loc;
       popen_attributes = add_docs_attrs docs attrs;
      }
  end

  module Incl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
      {
       pincl_mod = mexpr;
       pincl_loc = loc;
       pincl_attributes = add_docs_attrs docs attrs;
      }

  end

  module Vb = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
          ?(text = []) pat expr =
      {
       pvb_pat = pat;
       pvb_expr = expr;
       pvb_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pvb_loc = loc;
      }
  end

  module Ci = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
          ?(virt = Concrete) ?(params = []) name expr =
      {
       pci_virt = virt;
       pci_params = params;
       pci_name = name;
       pci_expr = expr;
       pci_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       pci_loc = loc;
      }
  end

  module Type = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(text = [])
        ?(params = [])
        ?(cstrs = [])
        ?(kind = Ptype_abstract)
        ?(priv = Public)
        ?manifest
        name =
      {
       ptype_name = name;
       ptype_params = params;
       ptype_cstrs = cstrs;
       ptype_kind = kind;
       ptype_private = priv;
       ptype_manifest = manifest;
       ptype_attributes =
         add_text_attrs text (add_docs_attrs docs attrs);
       ptype_loc = loc;
      }

    let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(args = Pcstr_tuple []) ?res name =
      {
       pcd_name = name;
       pcd_args = args;
       pcd_res = res;
       pcd_loc = loc;
       pcd_attributes = add_info_attrs info attrs;
       }

    let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
          ?(mut = Immutable) name typ =
      {
       pld_name = name;
       pld_mutable = mut;
       pld_type = typ;
       pld_loc = loc;
       pld_attributes = add_info_attrs info attrs;
      }

  end

  (** Type extensions *)
  module Te = struct
    let mk ?(attrs = []) ?(docs = empty_docs)
          ?(params = []) ?(priv = Public) path constructors =
      {
       ptyext_path = path;
       ptyext_params = params;
       ptyext_constructors = constructors;
       ptyext_private = priv;
       ptyext_attributes = add_docs_attrs docs attrs;
      }

    let constructor ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name kind =
      {
       pext_name = name;
       pext_kind = kind;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let decl ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
               ?(info = empty_info) ?(args = Pcstr_tuple []) ?res name =
      {
       pext_name = name;
       pext_kind = Pext_decl(args, res);
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let rebind ?(loc = !default_loc) ?(attrs = [])
          ?(docs = empty_docs) ?(info = empty_info) name lid =
      {
       pext_name = name;
       pext_kind = Pext_rebind lid;
       pext_loc = loc;
       pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

  end

  module Csig = struct
    let mk self fields =
      {
       pcsig_self = self;
       pcsig_fields = fields;
      }
  end

  module Cstr = struct
    let mk self fields =
      {
       pcstr_self = self;
       pcstr_fields = fields;
      }
  end

end

module Ast_mapper : sig
  (** The interface of a -ppx rewriter

    A -ppx rewriter is a program that accepts a serialized abstract syntax
    tree and outputs another, possibly modified, abstract syntax tree.
    This module encapsulates the interface between the compiler and
    the -ppx rewriters, handling such details as the serialization format,
    forwarding of command-line flags, and storing state.

    {!mapper} allows to implement AST rewriting using open recursion.
    A typical mapper would be based on {!default_mapper}, a deep
    identity mapper, and will fall back on it for handling the syntax it
    does not modify. For example:

    {[
  open Asttypes
  open Parsetree
  open Ast_mapper

  let test_mapper argv =
    { default_mapper with
      expr = fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
          Ast_helper.Exp.constant (Const_int 42)
        | other -> default_mapper.expr mapper other; }

  let () =
    register "ppx_test" test_mapper]}

    This -ppx rewriter, which replaces [[%test]] in expressions with
    the constant [42], can be compiled using
    [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

    *)

  open Parsetree

  (** {2 A generic Parsetree mapper} *)

  type mapper (*IF_CURRENT = Ast_mapper.mapper*) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }
  (** A mapper record implements one "method" per syntactic category,
      using an open recursion style: each method takes as its first
      argument the mapper to be applied to children in the syntax
      tree. *)

  val default_mapper: mapper
  (** A default mapper, which implements a "deep identity" mapping. *)

  (** {2 Convenience functions to write mappers} *)

  val map_opt: ('a -> 'b) -> 'a option -> 'b option

  val extension_of_error: Location.error -> extension
  (** Encode an error into an 'ocaml.error' extension node which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the error. *)

  val attribute_of_warning: Location.t -> string -> attribute
  (** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the warning. *)

end = struct
  (* A generic Parsetree mapping class *)

  (*
  [@@@ocaml.warning "+9"]
    (* Ensure that record patterns don't miss any field. *)
  *)


  open Parsetree
  open Ast_helper
  open Location

  type mapper (*IF_CURRENT = Ast_mapper.mapper*) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }

  let map_fst f (x, y) = (f x, y)
  let map_snd f (x, y) = (x, f y)
  let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
  let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
  let map_opt f = function None -> None | Some x -> Some (f x)

  let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

  module T = struct
    (* Type expressions for the core language *)

    let row_field sub = function
      | Rtag (l, attrs, b, tl) ->
          Rtag (l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
      | Rinherit t -> Rinherit (sub.typ sub t)

    let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
      let open Typ in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ptyp_any -> any ~loc ~attrs ()
      | Ptyp_var s -> var ~loc ~attrs s
      | Ptyp_arrow (lab, t1, t2) ->
          arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
      | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
      | Ptyp_constr (lid, tl) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_object (l, o) ->
          let f (s, a, t) =
            (map_loc sub s, sub.attributes sub a, sub.typ sub t) in
          object_ ~loc ~attrs (List.map f l) o
      | Ptyp_class (lid, tl) ->
          class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
      | Ptyp_variant (rl, b, ll) ->
          variant ~loc ~attrs (List.map (row_field sub) rl) b ll
      | Ptyp_poly (sl, t) -> poly ~loc ~attrs
                               (List.map (map_loc sub) sl) (sub.typ sub t)
      | Ptyp_package (lid, l) ->
          package ~loc ~attrs (map_loc sub lid)
            (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
      | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_type_declaration sub
        {ptype_name; ptype_params; ptype_cstrs;
         ptype_kind;
         ptype_private;
         ptype_manifest;
         ptype_attributes;
         ptype_loc} =
      Type.mk (map_loc sub ptype_name)
        ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
        ~priv:ptype_private
        ~cstrs:(List.map
                  (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                  ptype_cstrs)
        ~kind:(sub.type_kind sub ptype_kind)
        ?manifest:(map_opt (sub.typ sub) ptype_manifest)
        ~loc:(sub.location sub ptype_loc)
        ~attrs:(sub.attributes sub ptype_attributes)

    let map_type_kind sub = function
      | Ptype_abstract -> Ptype_abstract
      | Ptype_variant l ->
          Ptype_variant (List.map (sub.constructor_declaration sub) l)
      | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
      | Ptype_open -> Ptype_open

    let map_constructor_arguments sub = function
      | Pcstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
      | Pcstr_record l ->
          Pcstr_record (List.map (sub.label_declaration sub) l)

    let map_type_extension sub
        {ptyext_path; ptyext_params;
         ptyext_constructors;
         ptyext_private;
         ptyext_attributes} =
      Te.mk
        (map_loc sub ptyext_path)
        (List.map (sub.extension_constructor sub) ptyext_constructors)
        ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
        ~priv:ptyext_private
        ~attrs:(sub.attributes sub ptyext_attributes)

    let map_extension_constructor_kind sub = function
        Pext_decl(ctl, cto) ->
          Pext_decl(map_constructor_arguments sub ctl, map_opt (sub.typ sub) cto)
      | Pext_rebind li ->
          Pext_rebind (map_loc sub li)

    let map_extension_constructor sub
        {pext_name;
         pext_kind;
         pext_loc;
         pext_attributes} =
      Te.constructor
        (map_loc sub pext_name)
        (map_extension_constructor_kind sub pext_kind)
        ~loc:(sub.location sub pext_loc)
        ~attrs:(sub.attributes sub pext_attributes)

  end

  module CT = struct
    (* Type expressions for the class language *)

    let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
      let open Cty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcty_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
      | Pcty_arrow (lab, t, ct) ->
          arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
      | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
      =
      let open Ctf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
      | Pctf_val (s, m, v, t) ->
          val_ ~loc ~attrs (map_loc sub s) m v (sub.typ sub t)
      | Pctf_method (s, p, v, t) ->
          method_ ~loc ~attrs (map_loc sub s) p v (sub.typ sub t)
      | Pctf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_signature sub {pcsig_self; pcsig_fields} =
      Csig.mk
        (sub.typ sub pcsig_self)
        (List.map (sub.class_type_field sub) pcsig_fields)
  end

  module MT = struct
    (* Type expressions for the module language *)

    let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
      let open Mty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
      | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
      | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
      | Pmty_functor (s, mt1, mt2) ->
          functor_ ~loc ~attrs (map_loc sub s)
            (Misc.may_map (sub.module_type sub) mt1)
            (sub.module_type sub mt2)
      | Pmty_with (mt, l) ->
          with_ ~loc ~attrs (sub.module_type sub mt)
            (List.map (sub.with_constraint sub) l)
      | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
      | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_with_constraint sub = function
      | Pwith_type (lid, d) ->
          Pwith_type (map_loc sub lid, sub.type_declaration sub d)
      | Pwith_module (lid, lid2) ->
          Pwith_module (map_loc sub lid, map_loc sub lid2)
      | Pwith_typesubst d -> Pwith_typesubst (sub.type_declaration sub d)
      | Pwith_modsubst (s, lid) ->
          Pwith_modsubst (map_loc sub s, map_loc sub lid)

    let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
      let open Sig in
      let loc = sub.location sub loc in
      match desc with
      | Psig_value vd -> value ~loc (sub.value_description sub vd)
      | Psig_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
      | Psig_recmodule l ->
          rec_module ~loc (List.map (sub.module_declaration sub) l)
      | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Psig_open x -> open_ ~loc (sub.open_description sub x)
      | Psig_include x -> include_ ~loc (sub.include_description sub x)
      | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
      | Psig_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Psig_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
  end


  module M = struct
    (* Value expressions for the module language *)

    let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
      let open Mod in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
      | Pmod_functor (arg, arg_ty, body) ->
          functor_ ~loc ~attrs (map_loc sub arg)
            (Misc.may_map (sub.module_type sub) arg_ty)
            (sub.module_expr sub body)
      | Pmod_apply (m1, m2) ->
          apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
      | Pmod_constraint (m, mty) ->
          constraint_ ~loc ~attrs (sub.module_expr sub m)
                      (sub.module_type sub mty)
      | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
      | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
      let open Str in
      let loc = sub.location sub loc in
      match desc with
      | Pstr_eval (x, attrs) ->
          eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
      | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
      | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
      | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
      | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
      | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
      | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Pstr_open x -> open_ ~loc (sub.open_description sub x)
      | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
      | Pstr_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
      | Pstr_extension (x, attrs) ->
          extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
      | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
  end

  module E = struct
    (* Value expressions for the core language *)

    let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
      let open Exp in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pexp_constant x -> constant ~loc ~attrs x
      | Pexp_let (r, vbs, e) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.expr sub e)
      | Pexp_fun (lab, def, p, e) ->
          fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
            (sub.expr sub e)
      | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
      | Pexp_apply (e, l) ->
          apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
      | Pexp_match (e, pel) ->
          match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_construct (lid, arg) ->
          construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
      | Pexp_variant (lab, eo) ->
          variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
      | Pexp_record (l, eo) ->
          record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
            (map_opt (sub.expr sub) eo)
      | Pexp_field (e, lid) ->
          field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
      | Pexp_setfield (e1, lid, e2) ->
          setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
            (sub.expr sub e2)
      | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_ifthenelse (e1, e2, e3) ->
          ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
            (map_opt (sub.expr sub) e3)
      | Pexp_sequence (e1, e2) ->
          sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_while (e1, e2) ->
          while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_for (p, e1, e2, d, e3) ->
          for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
            (sub.expr sub e3)
      | Pexp_coerce (e, t1, t2) ->
          coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
            (sub.typ sub t2)
      | Pexp_constraint (e, t) ->
          constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
      | Pexp_send (e, s) ->
          send ~loc ~attrs (sub.expr sub e) (map_loc sub s)
      | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
      | Pexp_setinstvar (s, e) ->
          setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
      | Pexp_override sel ->
          override ~loc ~attrs
            (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
      | Pexp_letmodule (s, me, e) ->
          letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
            (sub.expr sub e)
      | Pexp_letexception (cd, e) ->
          letexception ~loc ~attrs
            (sub.extension_constructor sub cd)
            (sub.expr sub e)
      | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
      | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
      | Pexp_poly (e, t) ->
          poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
      | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
      | Pexp_newtype (s, e) ->
          newtype ~loc ~attrs (map_loc sub s) (sub.expr sub e)
      | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
      | Pexp_open (ovf, lid, e) ->
          open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
      | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
      | Pexp_unreachable -> unreachable ~loc ~attrs ()
  end

  module P = struct
    (* Patterns *)

    let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
      let open Pat in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ppat_any -> any ~loc ~attrs ()
      | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
      | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
      | Ppat_constant c -> constant ~loc ~attrs c
      | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
      | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_construct (l, p) ->
          construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
      | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
      | Ppat_record (lpl, cf) ->
          record ~loc ~attrs
                 (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
      | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
      | Ppat_constraint (p, t) ->
          constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
      | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
      | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
      | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
      | Ppat_open (lid,p) -> open_ ~loc ~attrs (map_loc sub lid) (sub.pat sub p)
      | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
      | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
  end

  module CE = struct
    (* Value expressions for the class language *)

    let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
      let open Cl in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcl_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcl_structure s ->
          structure ~loc ~attrs (sub.class_structure sub s)
      | Pcl_fun (lab, e, p, ce) ->
          fun_ ~loc ~attrs lab
            (map_opt (sub.expr sub) e)
            (sub.pat sub p)
            (sub.class_expr sub ce)
      | Pcl_apply (ce, l) ->
          apply ~loc ~attrs (sub.class_expr sub ce)
            (List.map (map_snd (sub.expr sub)) l)
      | Pcl_let (r, vbs, ce) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.class_expr sub ce)
      | Pcl_constraint (ce, ct) ->
          constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
      | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_kind sub = function
      | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
      | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

    let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
      let open Cf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcf_inherit (o, ce, s) ->
          inherit_ ~loc ~attrs o (sub.class_expr sub ce)
            (map_opt (map_loc sub) s)
      | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
      | Pcf_method (s, p, k) ->
          method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
      | Pcf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
      | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure sub {pcstr_self; pcstr_fields} =
      {
        pcstr_self = sub.pat sub pcstr_self;
        pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
      }

    let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                           pci_loc; pci_attributes} =
      Ci.mk
       ~virt:pci_virt
       ~params:(List.map (map_fst (sub.typ sub)) pl)
        (map_loc sub pci_name)
        (f pci_expr)
        ~loc:(sub.location sub pci_loc)
        ~attrs:(sub.attributes sub pci_attributes)
  end

  (* Now, a generic AST mapper, to be extended to cover all kinds and
     cases of the OCaml grammar.  The default behavior of the mapper is
     the identity. *)

  let default_mapper =
    {
      structure = (fun this l -> List.map (this.structure_item this) l);
      structure_item = M.map_structure_item;
      module_expr = M.map;
      signature = (fun this l -> List.map (this.signature_item this) l);
      signature_item = MT.map_signature_item;
      module_type = MT.map;
      with_constraint = MT.map_with_constraint;
      class_declaration =
        (fun this -> CE.class_infos this (this.class_expr this));
      class_expr = CE.map;
      class_field = CE.map_field;
      class_structure = CE.map_structure;
      class_type = CT.map;
      class_type_field = CT.map_field;
      class_signature = CT.map_signature;
      class_type_declaration =
        (fun this -> CE.class_infos this (this.class_type this));
      class_description =
        (fun this -> CE.class_infos this (this.class_type this));
      type_declaration = T.map_type_declaration;
      type_kind = T.map_type_kind;
      typ = T.map;
      type_extension = T.map_type_extension;
      extension_constructor = T.map_extension_constructor;
      value_description =
        (fun this {pval_name; pval_type; pval_prim; pval_loc;
                   pval_attributes} ->
          Val.mk
            (map_loc this pval_name)
            (this.typ this pval_type)
            ~attrs:(this.attributes this pval_attributes)
            ~loc:(this.location this pval_loc)
            ~prim:pval_prim
        );

      pat = P.map;
      expr = E.map;

      module_declaration =
        (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
           Md.mk
             (map_loc this pmd_name)
             (this.module_type this pmd_type)
             ~attrs:(this.attributes this pmd_attributes)
             ~loc:(this.location this pmd_loc)
        );

      module_type_declaration =
        (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
           Mtd.mk
             (map_loc this pmtd_name)
             ?typ:(map_opt (this.module_type this) pmtd_type)
             ~attrs:(this.attributes this pmtd_attributes)
             ~loc:(this.location this pmtd_loc)
        );

      module_binding =
        (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
           Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
             ~attrs:(this.attributes this pmb_attributes)
             ~loc:(this.location this pmb_loc)
        );


      open_description =
        (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
           Opn.mk (map_loc this popen_lid)
             ~override:popen_override
             ~loc:(this.location this popen_loc)
             ~attrs:(this.attributes this popen_attributes)
        );


      include_description =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_type this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );

      include_declaration =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_expr this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );


      value_binding =
        (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
           Vb.mk
             (this.pat this pvb_pat)
             (this.expr this pvb_expr)
             ~loc:(this.location this pvb_loc)
             ~attrs:(this.attributes this pvb_attributes)
        );


      constructor_declaration =
        (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
          Type.constructor
            (map_loc this pcd_name)
            ~args:(T.map_constructor_arguments this pcd_args)
            ?res:(map_opt (this.typ this) pcd_res)
            ~loc:(this.location this pcd_loc)
            ~attrs:(this.attributes this pcd_attributes)
        );

      label_declaration =
        (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
           Type.field
             (map_loc this pld_name)
             (this.typ this pld_type)
             ~mut:pld_mutable
             ~loc:(this.location this pld_loc)
             ~attrs:(this.attributes this pld_attributes)
        );

      cases = (fun this l -> List.map (this.case this) l);
      case =
        (fun this {pc_lhs; pc_guard; pc_rhs} ->
           {
             pc_lhs = this.pat this pc_lhs;
             pc_guard = map_opt (this.expr this) pc_guard;
             pc_rhs = this.expr this pc_rhs;
           }
        );



      location = (fun _this l -> l);

      extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attributes = (fun this l -> List.map (this.attribute this) l);
      payload =
        (fun this -> function
           | PStr x -> PStr (this.structure this x)
           | PSig x -> PSig (this.signature this x)
           | PTyp x -> PTyp (this.typ this x)
           | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
        );
    }

  let rec extension_of_error {loc; msg; if_highlight; sub} =
    { loc; txt = "ocaml.error" },
    PStr ([Str.eval (Exp.constant (Pconst_string (msg, None)));
           Str.eval (Exp.constant (Pconst_string (if_highlight, None)))] @
          (List.map (fun ext -> Str.extension (extension_of_error ext)) sub))

  let attribute_of_warning loc s =
    { loc; txt = "ocaml.ppwarning" },
    PStr ([Str.eval ~loc (Exp.constant (Pconst_string (s, None)))])

end

module Outcometree = struct
  (* Module [Outcometree]: results displayed by the toplevel *)

  (* These types represent messages that the toplevel displays as normal
     results or errors. The real displaying is customisable using the hooks:
        [Toploop.print_out_value]
        [Toploop.print_out_type]
        [Toploop.print_out_sig_item]
        [Toploop.print_out_phrase] *)

  type out_ident (*IF_CURRENT = Outcometree.out_ident *) =
    | Oide_apply of out_ident * out_ident
    | Oide_dot of out_ident * string
    | Oide_ident of string

  type out_attribute (*IF_CURRENT = Outcometree.out_attribute *) =
    { oattr_name: string }

  type out_value (*IF_CURRENT = Outcometree.out_value *) =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option

  type out_type (*IF_CURRENT = Outcometree.out_type *) =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type * string
    | Otyp_arrow of string * out_type * out_type
    | Otyp_class of bool * out_ident * out_type list
    | Otyp_constr of out_ident * out_type list
    | Otyp_manifest of out_type * out_type
    | Otyp_object of (string * out_type) list * bool option
    | Otyp_record of (string * bool * out_type) list
    | Otyp_stuff of string
    | Otyp_sum of (string * out_type list * out_type option) list
    | Otyp_tuple of out_type list
    | Otyp_var of bool * string
    | Otyp_variant of
        bool * out_variant * bool * (string list) option
    | Otyp_poly of string list * out_type
    | Otyp_module of string * string list * out_type list
    | Otyp_attribute of out_type * out_attribute

  and out_variant (*IF_CURRENT = Outcometree.out_variant *) =
    | Ovar_fields of (string * bool * out_type list) list
    | Ovar_typ of out_type

  type out_class_type (*IF_CURRENT = Outcometree.out_class_type *) =
    | Octy_constr of out_ident * out_type list
    | Octy_arrow of string * out_type * out_class_type
    | Octy_signature of out_type option * out_class_sig_item list
  and out_class_sig_item (*IF_CURRENT = Outcometree.out_class_sig_item *) =
    | Ocsg_constraint of out_type * out_type
    | Ocsg_method of string * bool * bool * out_type
    | Ocsg_value of string * bool * bool * out_type

  type out_module_type (*IF_CURRENT = Outcometree.out_module_type *) =
    | Omty_abstract
    | Omty_functor of string * out_module_type option * out_module_type
    | Omty_ident of out_ident
    | Omty_signature of out_sig_item list
    | Omty_alias of out_ident
  and out_sig_item (*IF_CURRENT = Outcometree.out_sig_item *) =
    | Osig_class of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_class_type of
        bool * string * (string * (bool * bool)) list * out_class_type *
          out_rec_status
    | Osig_typext of out_extension_constructor * out_ext_status
    | Osig_modtype of string * out_module_type
    | Osig_module of string * out_module_type * out_rec_status
    | Osig_type of out_type_decl * out_rec_status
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl (*IF_CURRENT = Outcometree.out_type_decl *) =
    { otype_name: string;
      otype_params: (string * (bool * bool)) list;
      otype_type: out_type;
      otype_private: Asttypes.private_flag;
      otype_immediate: bool;
      otype_unboxed: bool;
      otype_cstrs: (out_type * out_type) list }
  and out_extension_constructor (*IF_CURRENT = Outcometree.out_extension_constructor *) =
    { oext_name: string;
      oext_type_name: string;
      oext_type_params: string list;
      oext_args: out_type list;
      oext_ret_type: out_type option;
      oext_private: Asttypes.private_flag }
  and out_type_extension (*IF_CURRENT = Outcometree.out_type_extension *) =
    { otyext_name: string;
      otyext_params: string list;
      otyext_constructors: (string * out_type list * out_type option) list;
      otyext_private: Asttypes.private_flag }
  and out_val_decl (*IF_CURRENT = Outcometree.out_val_decl *) =
    { oval_name: string;
      oval_type: out_type;
      oval_prims: string list;
      oval_attributes: out_attribute list }
  and out_rec_status (*IF_CURRENT = Outcometree.out_rec_status *) =
    | Orec_not
    | Orec_first
    | Orec_next
  and out_ext_status (*IF_CURRENT = Outcometree.out_ext_status *) =
    | Oext_first
    | Oext_next
    | Oext_exception

  type out_phrase (*IF_CURRENT = Outcometree.out_phrase *) =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)

end

module Config = struct
  let ast_impl_magic_number = "Caml1999M020"
  let ast_intf_magic_number = "Caml1999N018"
end

let map_signature mapper = mapper.Ast_mapper.signature mapper
let map_structure mapper = mapper.Ast_mapper.structure mapper

let failing_mapper =
  let fail _ _ =
    invalid_arg "failing_mapper: this mapper function should never get called"
  in
  {
    Ast_mapper.
    structure               = fail;
    structure_item          = fail;
    module_expr             = fail;
    signature               = fail;
    signature_item          = fail;
    module_type             = fail;
    with_constraint         = fail;
    class_declaration       = fail;
    class_expr              = fail;
    class_field             = fail;
    class_structure         = fail;
    class_type              = fail;
    class_type_field        = fail;
    class_signature         = fail;
    class_type_declaration  = fail;
    class_description       = fail;
    type_declaration        = fail;
    type_kind               = fail;
    typ                     = fail;
    type_extension          = fail;
    extension_constructor   = fail;
    value_description       = fail;
    pat                     = fail;
    expr                    = fail;
    module_declaration      = fail;
    module_type_declaration = fail;
    module_binding          = fail;
    open_description        = fail;
    include_description     = fail;
    include_declaration     = fail;
    value_binding           = fail;
    constructor_declaration = fail;
    label_declaration       = fail;
    cases                   = fail;
    case                    = fail;
    location                = fail;
    extension               = fail;
    attribute               = fail;
    attributes              = fail;
    payload                 = fail;
  }

let make_top_mapper ~signature ~structure =
  {failing_mapper with Ast_mapper.
                    signature = (fun _ x -> signature x);
                    structure = (fun _ x -> structure x) }

end
module Migrate_parsetree_402_403_migrate
= struct
#1 "migrate_parsetree_402_403_migrate.ml"
# 1 "src/migrate_parsetree_402_403_migrate.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module From = Ast_402
module To = Ast_403

let extract_predef_option label typ =
  let open From in
  let open Longident in
  match label, typ.Parsetree.ptyp_desc with
  | To.Asttypes.Optional _,
    From.Parsetree.Ptyp_constr (
      {Location.txt = Ldot (Lident "*predef*", "option"); _}, [d]) ->
      d
  | _ -> typ

let rec copy_expression :
  From.Parsetree.expression ->
    To.Parsetree.expression
  =
  fun
    { From.Parsetree.pexp_desc = pexp_desc;
      From.Parsetree.pexp_loc = pexp_loc;
      From.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      To.Parsetree.pexp_desc =
        (copy_expression_desc pexp_desc);
      To.Parsetree.pexp_loc =
        (copy_location pexp_loc);
      To.Parsetree.pexp_attributes =
        (copy_attributes pexp_attributes)
    }

and copy_expression_desc :
  From.Parsetree.expression_desc ->
    To.Parsetree.expression_desc
  =
  function
  | From.Parsetree.Pexp_ident x0 ->
      To.Parsetree.Pexp_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_constant x0 ->
      To.Parsetree.Pexp_constant
        (copy_constant x0)
  | From.Parsetree.Pexp_let (x0,x1,x2) ->
      To.Parsetree.Pexp_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_function x0 ->
      To.Parsetree.Pexp_function
        (List.map copy_case x0)
  | From.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pexp_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_expression x3))
  | From.Parsetree.Pexp_apply (x0,x1) ->
      To.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pexp_match (x0,x1) ->
      To.Parsetree.Pexp_match
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_try (x0,x1) ->
      To.Parsetree.Pexp_try
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_tuple x0 ->
      To.Parsetree.Pexp_tuple
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_construct (x0,x1) ->
      To.Parsetree.Pexp_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_variant (x0,x1) ->
      To.Parsetree.Pexp_variant
        ((copy_label x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_record (x0,x1) ->
      To.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_expression x1))) x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_field (x0,x1) ->
      To.Parsetree.Pexp_field
        ((copy_expression x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pexp_setfield (x0,x1,x2) ->
      To.Parsetree.Pexp_setfield
        ((copy_expression x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_array x0 ->
      To.Parsetree.Pexp_array
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      To.Parsetree.Pexp_ifthenelse
        ((copy_expression x0),
          (copy_expression x1),
          (copy_option copy_expression x2))
  | From.Parsetree.Pexp_sequence (x0,x1) ->
      To.Parsetree.Pexp_sequence
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_while (x0,x1) ->
      To.Parsetree.Pexp_while
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      To.Parsetree.Pexp_for
        ((copy_pattern x0),
          (copy_expression x1),
          (copy_expression x2),
          (copy_direction_flag x3),
          (copy_expression x4))
  | From.Parsetree.Pexp_constraint (x0,x1) ->
      To.Parsetree.Pexp_constraint
        ((copy_expression x0),
          (copy_core_type x1))
  | From.Parsetree.Pexp_coerce (x0,x1,x2) ->
      To.Parsetree.Pexp_coerce
        ((copy_expression x0),
          (copy_option copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Pexp_send (x0,x1) ->
      To.Parsetree.Pexp_send
        ((copy_expression x0), x1)
  | From.Parsetree.Pexp_new x0 ->
      To.Parsetree.Pexp_new
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_setinstvar (x0,x1) ->
      To.Parsetree.Pexp_setinstvar
        ((copy_loc (fun x  -> x) x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_override x0 ->
      To.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_loc (fun x  -> x) x0),
                (copy_expression x1))) x0)
  | From.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      To.Parsetree.Pexp_letmodule
        ((copy_loc (fun x  -> x) x0),
          (copy_module_expr x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_assert x0 ->
      To.Parsetree.Pexp_assert
        (copy_expression x0)
  | From.Parsetree.Pexp_lazy x0 ->
      To.Parsetree.Pexp_lazy
        (copy_expression x0)
  | From.Parsetree.Pexp_poly (x0,x1) ->
      To.Parsetree.Pexp_poly
        ((copy_expression x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pexp_object x0 ->
      To.Parsetree.Pexp_object
        (copy_class_structure x0)
  | From.Parsetree.Pexp_newtype (x0,x1) ->
      To.Parsetree.Pexp_newtype
        (x0, (copy_expression x1))
  | From.Parsetree.Pexp_pack x0 ->
      To.Parsetree.Pexp_pack
        (copy_module_expr x0)
  | From.Parsetree.Pexp_open (x0,x1,x2) ->
      To.Parsetree.Pexp_open
        ((copy_override_flag x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_extension x0 ->
      To.Parsetree.Pexp_extension
        (copy_extension x0)

and copy_direction_flag :
  From.Asttypes.direction_flag ->
    To.Asttypes.direction_flag
  =
  function
  | From.Asttypes.Upto  -> To.Asttypes.Upto
  | From.Asttypes.Downto  -> To.Asttypes.Downto

and copy_case :
  From.Parsetree.case -> To.Parsetree.case =
  fun
    { From.Parsetree.pc_lhs = pc_lhs;
      From.Parsetree.pc_guard = pc_guard;
      From.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      To.Parsetree.pc_lhs =
        (copy_pattern pc_lhs);
      To.Parsetree.pc_guard =
        (copy_option copy_expression pc_guard);
      To.Parsetree.pc_rhs =
        (copy_expression pc_rhs)
    }

and copy_value_binding :
  From.Parsetree.value_binding ->
    To.Parsetree.value_binding
  =
  fun
    { From.Parsetree.pvb_pat = pvb_pat;
      From.Parsetree.pvb_expr = pvb_expr;
      From.Parsetree.pvb_attributes = pvb_attributes;
      From.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      To.Parsetree.pvb_pat =
        (copy_pattern pvb_pat);
      To.Parsetree.pvb_expr =
        (copy_expression pvb_expr);
      To.Parsetree.pvb_attributes =
        (copy_attributes pvb_attributes);
      To.Parsetree.pvb_loc =
        (copy_location pvb_loc)
    }

and copy_pattern :
  From.Parsetree.pattern -> To.Parsetree.pattern =
  fun
    { From.Parsetree.ppat_desc = ppat_desc;
      From.Parsetree.ppat_loc = ppat_loc;
      From.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      To.Parsetree.ppat_desc =
        (copy_pattern_desc ppat_desc);
      To.Parsetree.ppat_loc =
        (copy_location ppat_loc);
      To.Parsetree.ppat_attributes =
        (copy_attributes ppat_attributes)
    }

and copy_pattern_desc :
  From.Parsetree.pattern_desc ->
    To.Parsetree.pattern_desc
  =
  function
  | From.Parsetree.Ppat_any  ->
      To.Parsetree.Ppat_any
  | From.Parsetree.Ppat_var x0 ->
      To.Parsetree.Ppat_var
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_alias (x0,x1) ->
      To.Parsetree.Ppat_alias
        ((copy_pattern x0),
          (copy_loc (fun x  -> x) x1))
  | From.Parsetree.Ppat_constant x0 ->
      To.Parsetree.Ppat_constant
        (copy_constant x0)
  | From.Parsetree.Ppat_interval (x0,x1) ->
      To.Parsetree.Ppat_interval
        ((copy_constant x0),
          (copy_constant x1))
  | From.Parsetree.Ppat_tuple x0 ->
      To.Parsetree.Ppat_tuple
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_construct (x0,x1) ->
      To.Parsetree.Ppat_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_variant (x0,x1) ->
      To.Parsetree.Ppat_variant
        ((copy_label x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_record (x0,x1) ->
      To.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ppat_array x0 ->
      To.Parsetree.Ppat_array
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_or (x0,x1) ->
      To.Parsetree.Ppat_or
        ((copy_pattern x0),
          (copy_pattern x1))
  | From.Parsetree.Ppat_constraint (x0,x1) ->
      To.Parsetree.Ppat_constraint
        ((copy_pattern x0),
          (copy_core_type x1))
  | From.Parsetree.Ppat_type x0 ->
      To.Parsetree.Ppat_type
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Ppat_lazy x0 ->
      To.Parsetree.Ppat_lazy
        (copy_pattern x0)
  | From.Parsetree.Ppat_unpack x0 ->
      To.Parsetree.Ppat_unpack
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_exception x0 ->
      To.Parsetree.Ppat_exception
        (copy_pattern x0)
  | From.Parsetree.Ppat_extension x0 ->
      To.Parsetree.Ppat_extension
        (copy_extension x0)

and copy_core_type :
  From.Parsetree.core_type ->
    To.Parsetree.core_type
  =
  fun
    { From.Parsetree.ptyp_desc = ptyp_desc;
      From.Parsetree.ptyp_loc = ptyp_loc;
      From.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      To.Parsetree.ptyp_desc =
        (copy_core_type_desc ptyp_desc);
      To.Parsetree.ptyp_loc =
        (copy_location ptyp_loc);
      To.Parsetree.ptyp_attributes =
        (copy_attributes ptyp_attributes)
    }

and copy_core_type_desc :
  From.Parsetree.core_type_desc ->
    To.Parsetree.core_type_desc
  =
  function
  | From.Parsetree.Ptyp_any  ->
      To.Parsetree.Ptyp_any
  | From.Parsetree.Ptyp_var x0 ->
      To.Parsetree.Ptyp_var x0
  | From.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      let label = copy_arg_label x0 in
      To.Parsetree.Ptyp_arrow
        (label,
         copy_core_type (extract_predef_option label x1),
         copy_core_type x2)
  | From.Parsetree.Ptyp_tuple x0 ->
      To.Parsetree.Ptyp_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Ptyp_constr (x0,x1) ->
      To.Parsetree.Ptyp_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_object (x0,x1) ->
      To.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (x0, (copy_attributes x1),
                 (copy_core_type x2))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ptyp_class (x0,x1) ->
      To.Parsetree.Ptyp_class
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_alias (x0,x1) ->
      To.Parsetree.Ptyp_alias
        ((copy_core_type x0), x1)
  | From.Parsetree.Ptyp_variant (x0,x1,x2) ->
      To.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0),
          (copy_closed_flag x1),
          (copy_option
             (fun x  -> List.map copy_label x) x2))
  | From.Parsetree.Ptyp_poly (x0,x1) ->
      To.Parsetree.Ptyp_poly
        ((List.map (fun x  -> x) x0),
          (copy_core_type x1))
  | From.Parsetree.Ptyp_package x0 ->
      To.Parsetree.Ptyp_package
        (copy_package_type x0)
  | From.Parsetree.Ptyp_extension x0 ->
      To.Parsetree.Ptyp_extension
        (copy_extension x0)

and copy_package_type :
  From.Parsetree.package_type ->
    To.Parsetree.package_type
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc copy_longident x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_loc
                copy_longident x0),
              (copy_core_type x1))) x1))

and copy_row_field :
  From.Parsetree.row_field ->
    To.Parsetree.row_field
  =
  function
  | From.Parsetree.Rtag (x0,x1,x2,x3) ->
      To.Parsetree.Rtag
        ((copy_label x0),
          (copy_attributes x1), (copy_bool x2),
          (List.map copy_core_type x3))
  | From.Parsetree.Rinherit x0 ->
      To.Parsetree.Rinherit
        (copy_core_type x0)

and copy_attributes :
  From.Parsetree.attributes ->
    To.Parsetree.attributes
  = fun x  -> List.map copy_attribute x

and copy_attribute :
  From.Parsetree.attribute ->
    To.Parsetree.attribute
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_payload :
  From.Parsetree.payload -> To.Parsetree.payload =
  function
  | From.Parsetree.PStr x0 ->
      To.Parsetree.PStr
        (copy_structure x0)
  | From.Parsetree.PTyp x0 ->
      To.Parsetree.PTyp
        (copy_core_type x0)
  | From.Parsetree.PPat (x0,x1) ->
      To.Parsetree.PPat
        ((copy_pattern x0),
          (copy_option copy_expression x1))

and copy_structure :
  From.Parsetree.structure ->
    To.Parsetree.structure
  = fun x  -> List.map copy_structure_item x

and copy_structure_item :
  From.Parsetree.structure_item ->
    To.Parsetree.structure_item
  =
  fun
    { From.Parsetree.pstr_desc = pstr_desc;
      From.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      To.Parsetree.pstr_desc =
        (copy_structure_item_desc pstr_desc);
      To.Parsetree.pstr_loc =
        (copy_location pstr_loc)
    }

and copy_structure_item_desc :
  From.Parsetree.structure_item_desc ->
    To.Parsetree.structure_item_desc
  =
  function
  | From.Parsetree.Pstr_eval (x0,x1) ->
      To.Parsetree.Pstr_eval
        ((copy_expression x0),
          (copy_attributes x1))
  | From.Parsetree.Pstr_value (x0,x1) ->
      To.Parsetree.Pstr_value
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1))
  | From.Parsetree.Pstr_primitive x0 ->
      To.Parsetree.Pstr_primitive
        (copy_value_description x0)
  | From.Parsetree.Pstr_type x0 ->
      let recflag, types = type_declarations x0 in
      To.Parsetree.Pstr_type (recflag, types)
  | From.Parsetree.Pstr_typext x0 ->
      To.Parsetree.Pstr_typext
        (copy_type_extension x0)
  | From.Parsetree.Pstr_exception x0 ->
      To.Parsetree.Pstr_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Pstr_module x0 ->
      To.Parsetree.Pstr_module
        (copy_module_binding x0)
  | From.Parsetree.Pstr_recmodule x0 ->
      To.Parsetree.Pstr_recmodule
        (List.map copy_module_binding x0)
  | From.Parsetree.Pstr_modtype x0 ->
      To.Parsetree.Pstr_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Pstr_open x0 ->
      To.Parsetree.Pstr_open
        (copy_open_description x0)
  | From.Parsetree.Pstr_class x0 ->
      To.Parsetree.Pstr_class
        (List.map copy_class_declaration x0)
  | From.Parsetree.Pstr_class_type x0 ->
      To.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Pstr_include x0 ->
      To.Parsetree.Pstr_include
        (copy_include_declaration x0)
  | From.Parsetree.Pstr_attribute x0 ->
      To.Parsetree.Pstr_attribute
        (copy_attribute x0)
  | From.Parsetree.Pstr_extension (x0,x1) ->
      To.Parsetree.Pstr_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_include_declaration :
  From.Parsetree.include_declaration ->
    To.Parsetree.include_declaration
  =
  fun x  ->
    copy_include_infos
      copy_module_expr x

and copy_class_declaration :
  From.Parsetree.class_declaration ->
    To.Parsetree.class_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_expr x

and copy_class_expr :
  From.Parsetree.class_expr ->
    To.Parsetree.class_expr
  =
  fun
    { From.Parsetree.pcl_desc = pcl_desc;
      From.Parsetree.pcl_loc = pcl_loc;
      From.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      To.Parsetree.pcl_desc =
        (copy_class_expr_desc pcl_desc);
      To.Parsetree.pcl_loc =
        (copy_location pcl_loc);
      To.Parsetree.pcl_attributes =
        (copy_attributes pcl_attributes)
    }

and copy_class_expr_desc :
  From.Parsetree.class_expr_desc ->
    To.Parsetree.class_expr_desc
  =
  function
  | From.Parsetree.Pcl_constr (x0,x1) ->
      To.Parsetree.Pcl_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcl_structure x0 ->
      To.Parsetree.Pcl_structure
        (copy_class_structure x0)
  | From.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pcl_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_class_expr x3))
  | From.Parsetree.Pcl_apply (x0,x1) ->
      To.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pcl_let (x0,x1,x2) ->
      To.Parsetree.Pcl_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | From.Parsetree.Pcl_constraint (x0,x1) ->
      To.Parsetree.Pcl_constraint
        ((copy_class_expr x0),
          (copy_class_type x1))
  | From.Parsetree.Pcl_extension x0 ->
      To.Parsetree.Pcl_extension
        (copy_extension x0)

and copy_class_structure :
  From.Parsetree.class_structure ->
    To.Parsetree.class_structure
  =
  fun
    { From.Parsetree.pcstr_self = pcstr_self;
      From.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      To.Parsetree.pcstr_self =
        (copy_pattern pcstr_self);
      To.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }

and copy_class_field :
  From.Parsetree.class_field ->
    To.Parsetree.class_field
  =
  fun
    { From.Parsetree.pcf_desc = pcf_desc;
      From.Parsetree.pcf_loc = pcf_loc;
      From.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      To.Parsetree.pcf_desc =
        (copy_class_field_desc pcf_desc);
      To.Parsetree.pcf_loc =
        (copy_location pcf_loc);
      To.Parsetree.pcf_attributes =
        (copy_attributes pcf_attributes)
    }

and copy_class_field_desc :
  From.Parsetree.class_field_desc ->
    To.Parsetree.class_field_desc
  =
  function
  | From.Parsetree.Pcf_inherit (x0,x1,x2) ->
      To.Parsetree.Pcf_inherit
        ((copy_override_flag x0),
          (copy_class_expr x1),
          (copy_option (fun x  -> x) x2))
  | From.Parsetree.Pcf_val x0 ->
      To.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_method x0 ->
      To.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_constraint x0 ->
      To.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pcf_initializer x0 ->
      To.Parsetree.Pcf_initializer
        (copy_expression x0)
  | From.Parsetree.Pcf_attribute x0 ->
      To.Parsetree.Pcf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pcf_extension x0 ->
      To.Parsetree.Pcf_extension
        (copy_extension x0)

and copy_class_field_kind :
  From.Parsetree.class_field_kind ->
    To.Parsetree.class_field_kind
  =
  function
  | From.Parsetree.Cfk_virtual x0 ->
      To.Parsetree.Cfk_virtual
        (copy_core_type x0)
  | From.Parsetree.Cfk_concrete (x0,x1) ->
      To.Parsetree.Cfk_concrete
        ((copy_override_flag x0),
          (copy_expression x1))

and copy_module_binding :
  From.Parsetree.module_binding ->
    To.Parsetree.module_binding
  =
  fun
    { From.Parsetree.pmb_name = pmb_name;
      From.Parsetree.pmb_expr = pmb_expr;
      From.Parsetree.pmb_attributes = pmb_attributes;
      From.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      To.Parsetree.pmb_name =
        (copy_loc (fun x  -> x) pmb_name);
      To.Parsetree.pmb_expr =
        (copy_module_expr pmb_expr);
      To.Parsetree.pmb_attributes =
        (copy_attributes pmb_attributes);
      To.Parsetree.pmb_loc =
        (copy_location pmb_loc)
    }

and copy_module_expr :
  From.Parsetree.module_expr ->
    To.Parsetree.module_expr
  =
  fun
    { From.Parsetree.pmod_desc = pmod_desc;
      From.Parsetree.pmod_loc = pmod_loc;
      From.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      To.Parsetree.pmod_desc =
        (copy_module_expr_desc pmod_desc);
      To.Parsetree.pmod_loc =
        (copy_location pmod_loc);
      To.Parsetree.pmod_attributes =
        (copy_attributes pmod_attributes)
    }

and copy_module_expr_desc :
  From.Parsetree.module_expr_desc ->
    To.Parsetree.module_expr_desc
  =
  function
  | From.Parsetree.Pmod_ident x0 ->
      To.Parsetree.Pmod_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmod_structure x0 ->
      To.Parsetree.Pmod_structure
        (copy_structure x0)
  | From.Parsetree.Pmod_functor (x0,x1,x2) ->
      To.Parsetree.Pmod_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_expr x2))
  | From.Parsetree.Pmod_apply (x0,x1) ->
      To.Parsetree.Pmod_apply
        ((copy_module_expr x0),
          (copy_module_expr x1))
  | From.Parsetree.Pmod_constraint (x0,x1) ->
      To.Parsetree.Pmod_constraint
        ((copy_module_expr x0),
          (copy_module_type x1))
  | From.Parsetree.Pmod_unpack x0 ->
      To.Parsetree.Pmod_unpack
        (copy_expression x0)
  | From.Parsetree.Pmod_extension x0 ->
      To.Parsetree.Pmod_extension
        (copy_extension x0)

and copy_module_type :
  From.Parsetree.module_type ->
    To.Parsetree.module_type
  =
  fun
    { From.Parsetree.pmty_desc = pmty_desc;
      From.Parsetree.pmty_loc = pmty_loc;
      From.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      To.Parsetree.pmty_desc =
        (copy_module_type_desc pmty_desc);
      To.Parsetree.pmty_loc =
        (copy_location pmty_loc);
      To.Parsetree.pmty_attributes =
        (copy_attributes pmty_attributes)
    }

and copy_module_type_desc :
  From.Parsetree.module_type_desc ->
    To.Parsetree.module_type_desc
  =
  function
  | From.Parsetree.Pmty_ident x0 ->
      To.Parsetree.Pmty_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmty_signature x0 ->
      To.Parsetree.Pmty_signature
        (copy_signature x0)
  | From.Parsetree.Pmty_functor (x0,x1,x2) ->
      To.Parsetree.Pmty_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_type x2))
  | From.Parsetree.Pmty_with (x0,x1) ->
      To.Parsetree.Pmty_with
        ((copy_module_type x0),
          (List.map copy_with_constraint x1))
  | From.Parsetree.Pmty_typeof x0 ->
      To.Parsetree.Pmty_typeof
        (copy_module_expr x0)
  | From.Parsetree.Pmty_extension x0 ->
      To.Parsetree.Pmty_extension
        (copy_extension x0)
  | From.Parsetree.Pmty_alias x0 ->
      To.Parsetree.Pmty_alias
        (copy_loc copy_longident
           x0)

and copy_with_constraint :
  From.Parsetree.with_constraint ->
    To.Parsetree.with_constraint
  =
  function
  | From.Parsetree.Pwith_type (x0,x1) ->
      To.Parsetree.Pwith_type
        ((copy_loc
            copy_longident x0),
          (copy_type_declaration x1))
  | From.Parsetree.Pwith_module (x0,x1) ->
      To.Parsetree.Pwith_module
        ((copy_loc
            copy_longident x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pwith_typesubst x0 ->
      To.Parsetree.Pwith_typesubst
        (copy_type_declaration x0)
  | From.Parsetree.Pwith_modsubst (x0,x1) ->
      To.Parsetree.Pwith_modsubst
        ((copy_loc (fun x  -> x) x0),
          (copy_loc
             copy_longident x1))

and copy_signature :
  From.Parsetree.signature ->
    To.Parsetree.signature
  = fun x  -> List.map copy_signature_item x

and copy_signature_item :
  From.Parsetree.signature_item ->
    To.Parsetree.signature_item
  =
  fun
    { From.Parsetree.psig_desc = psig_desc;
      From.Parsetree.psig_loc = psig_loc }
     ->
    {
      To.Parsetree.psig_desc =
        (copy_signature_item_desc psig_desc);
      To.Parsetree.psig_loc =
        (copy_location psig_loc)
    }

and copy_signature_item_desc :
  From.Parsetree.signature_item_desc ->
    To.Parsetree.signature_item_desc
  =
  function
  | From.Parsetree.Psig_value x0 ->
      To.Parsetree.Psig_value
        (copy_value_description x0)
  | From.Parsetree.Psig_type x0 ->
      let recflag, types = type_declarations x0 in
      To.Parsetree.Psig_type (recflag, types)
  | From.Parsetree.Psig_typext x0 ->
      To.Parsetree.Psig_typext
        (copy_type_extension x0)
  | From.Parsetree.Psig_exception x0 ->
      To.Parsetree.Psig_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Psig_module x0 ->
      To.Parsetree.Psig_module
        (copy_module_declaration x0)
  | From.Parsetree.Psig_recmodule x0 ->
      To.Parsetree.Psig_recmodule
        (List.map copy_module_declaration x0)
  | From.Parsetree.Psig_modtype x0 ->
      To.Parsetree.Psig_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Psig_open x0 ->
      To.Parsetree.Psig_open
        (copy_open_description x0)
  | From.Parsetree.Psig_include x0 ->
      To.Parsetree.Psig_include
        (copy_include_description x0)
  | From.Parsetree.Psig_class x0 ->
      To.Parsetree.Psig_class
        (List.map copy_class_description x0)
  | From.Parsetree.Psig_class_type x0 ->
      To.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Psig_attribute x0 ->
      To.Parsetree.Psig_attribute
        (copy_attribute x0)
  | From.Parsetree.Psig_extension (x0,x1) ->
      To.Parsetree.Psig_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_class_type_declaration :
  From.Parsetree.class_type_declaration ->
    To.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_description :
  From.Parsetree.class_description ->
    To.Parsetree.class_description
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_type :
  From.Parsetree.class_type ->
    To.Parsetree.class_type
  =
  fun
    { From.Parsetree.pcty_desc = pcty_desc;
      From.Parsetree.pcty_loc = pcty_loc;
      From.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      To.Parsetree.pcty_desc =
        (copy_class_type_desc pcty_desc);
      To.Parsetree.pcty_loc =
        (copy_location pcty_loc);
      To.Parsetree.pcty_attributes =
        (copy_attributes pcty_attributes)
    }

and copy_class_type_desc :
  From.Parsetree.class_type_desc ->
    To.Parsetree.class_type_desc
  =
  function
  | From.Parsetree.Pcty_constr (x0,x1) ->
      To.Parsetree.Pcty_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcty_signature x0 ->
      To.Parsetree.Pcty_signature
        (copy_class_signature x0)
  | From.Parsetree.Pcty_arrow (x0,x1,x2) ->
      let label = copy_arg_label x0 in
      To.Parsetree.Pcty_arrow
        (label,
         copy_core_type (extract_predef_option label x1),
         copy_class_type x2)
  | From.Parsetree.Pcty_extension x0 ->
      To.Parsetree.Pcty_extension
        (copy_extension x0)

and copy_class_signature :
  From.Parsetree.class_signature ->
    To.Parsetree.class_signature
  =
  fun
    { From.Parsetree.pcsig_self = pcsig_self;
      From.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      To.Parsetree.pcsig_self =
        (copy_core_type pcsig_self);
      To.Parsetree.pcsig_fields =
        (List.map copy_class_type_field
           pcsig_fields)
    }

and copy_class_type_field :
  From.Parsetree.class_type_field ->
    To.Parsetree.class_type_field
  =
  fun
    { From.Parsetree.pctf_desc = pctf_desc;
      From.Parsetree.pctf_loc = pctf_loc;
      From.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      To.Parsetree.pctf_desc =
        (copy_class_type_field_desc pctf_desc);
      To.Parsetree.pctf_loc =
        (copy_location pctf_loc);
      To.Parsetree.pctf_attributes =
        (copy_attributes pctf_attributes)
    }

and copy_class_type_field_desc :
  From.Parsetree.class_type_field_desc ->
    To.Parsetree.class_type_field_desc
  =
  function
  | From.Parsetree.Pctf_inherit x0 ->
      To.Parsetree.Pctf_inherit
        (copy_class_type x0)
  | From.Parsetree.Pctf_val x0 ->
      To.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_mutable_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_method x0 ->
      To.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_private_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_constraint x0 ->
      To.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pctf_attribute x0 ->
      To.Parsetree.Pctf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pctf_extension x0 ->
      To.Parsetree.Pctf_extension
        (copy_extension x0)

and copy_extension :
  From.Parsetree.extension ->
    To.Parsetree.extension
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.class_infos ->
        'g0 To.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pci_virt = pci_virt;
        From.Parsetree.pci_params = pci_params;
        From.Parsetree.pci_name = pci_name;
        From.Parsetree.pci_expr = pci_expr;
        From.Parsetree.pci_loc = pci_loc;
        From.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        To.Parsetree.pci_virt =
          (copy_virtual_flag pci_virt);
        To.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_core_type x0),
                  (copy_variance x1))) pci_params);
        To.Parsetree.pci_name =
          (copy_loc (fun x  -> x) pci_name);
        To.Parsetree.pci_expr = (f0 pci_expr);
        To.Parsetree.pci_loc =
          (copy_location pci_loc);
        To.Parsetree.pci_attributes =
          (copy_attributes pci_attributes)
      }

and copy_virtual_flag :
  From.Asttypes.virtual_flag ->
    To.Asttypes.virtual_flag
  =
  function
  | From.Asttypes.Virtual  -> To.Asttypes.Virtual
  | From.Asttypes.Concrete  -> To.Asttypes.Concrete

and copy_include_description :
  From.Parsetree.include_description ->
    To.Parsetree.include_description
  =
  fun x  ->
    copy_include_infos
      copy_module_type x

and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.include_infos ->
        'g0 To.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pincl_mod = pincl_mod;
        From.Parsetree.pincl_loc = pincl_loc;
        From.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        To.Parsetree.pincl_mod = (f0 pincl_mod);
        To.Parsetree.pincl_loc =
          (copy_location pincl_loc);
        To.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }

and copy_open_description :
  From.Parsetree.open_description ->
    To.Parsetree.open_description
  =
  fun
    { From.Parsetree.popen_lid = popen_lid;
      From.Parsetree.popen_override = popen_override;
      From.Parsetree.popen_loc = popen_loc;
      From.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      To.Parsetree.popen_lid =
        (copy_loc copy_longident
           popen_lid);
      To.Parsetree.popen_override =
        (copy_override_flag popen_override);
      To.Parsetree.popen_loc =
        (copy_location popen_loc);
      To.Parsetree.popen_attributes =
        (copy_attributes popen_attributes)
    }

and copy_override_flag :
  From.Asttypes.override_flag ->
    To.Asttypes.override_flag
  =
  function
  | From.Asttypes.Override  -> To.Asttypes.Override
  | From.Asttypes.Fresh  -> To.Asttypes.Fresh

and copy_module_type_declaration :
  From.Parsetree.module_type_declaration ->
    To.Parsetree.module_type_declaration
  =
  fun
    { From.Parsetree.pmtd_name = pmtd_name;
      From.Parsetree.pmtd_type = pmtd_type;
      From.Parsetree.pmtd_attributes = pmtd_attributes;
      From.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      To.Parsetree.pmtd_name =
        (copy_loc (fun x  -> x) pmtd_name);
      To.Parsetree.pmtd_type =
        (copy_option copy_module_type pmtd_type);
      To.Parsetree.pmtd_attributes =
        (copy_attributes pmtd_attributes);
      To.Parsetree.pmtd_loc =
        (copy_location pmtd_loc)
    }

and copy_module_declaration :
  From.Parsetree.module_declaration ->
    To.Parsetree.module_declaration
  =
  fun
    { From.Parsetree.pmd_name = pmd_name;
      From.Parsetree.pmd_type = pmd_type;
      From.Parsetree.pmd_attributes = pmd_attributes;
      From.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      To.Parsetree.pmd_name =
        (copy_loc (fun x  -> x) pmd_name);
      To.Parsetree.pmd_type =
        (copy_module_type pmd_type);
      To.Parsetree.pmd_attributes =
        (copy_attributes pmd_attributes);
      To.Parsetree.pmd_loc =
        (copy_location pmd_loc)
    }

and copy_type_extension :
  From.Parsetree.type_extension ->
    To.Parsetree.type_extension
  =
  fun
    { From.Parsetree.ptyext_path = ptyext_path;
      From.Parsetree.ptyext_params = ptyext_params;
      From.Parsetree.ptyext_constructors = ptyext_constructors;
      From.Parsetree.ptyext_private = ptyext_private;
      From.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      To.Parsetree.ptyext_path =
        (copy_loc copy_longident
           ptyext_path);
      To.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptyext_params);
      To.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor
           ptyext_constructors);
      To.Parsetree.ptyext_private =
        (copy_private_flag ptyext_private);
      To.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }

and copy_extension_constructor :
  From.Parsetree.extension_constructor ->
    To.Parsetree.extension_constructor
  =
  fun
    { From.Parsetree.pext_name = pext_name;
      From.Parsetree.pext_kind = pext_kind;
      From.Parsetree.pext_loc = pext_loc;
      From.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      To.Parsetree.pext_name =
        (copy_loc (fun x  -> x) pext_name);
      To.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      To.Parsetree.pext_loc =
        (copy_location pext_loc);
      To.Parsetree.pext_attributes =
        (copy_attributes pext_attributes)
    }

and copy_extension_constructor_kind :
  From.Parsetree.extension_constructor_kind ->
    To.Parsetree.extension_constructor_kind
  =
  function
  | From.Parsetree.Pext_decl (x0,x1) ->
      To.Parsetree.Pext_decl
        (To.Parsetree.Pcstr_tuple (List.map copy_core_type x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pext_rebind x0 ->
      To.Parsetree.Pext_rebind
        (copy_loc copy_longident
           x0)

and copy_type_declaration :
  From.Parsetree.type_declaration ->
    To.Parsetree.type_declaration
  =
  fun
    { From.Parsetree.ptype_name = ptype_name;
      From.Parsetree.ptype_params = ptype_params;
      From.Parsetree.ptype_cstrs = ptype_cstrs;
      From.Parsetree.ptype_kind = ptype_kind;
      From.Parsetree.ptype_private = ptype_private;
      From.Parsetree.ptype_manifest = ptype_manifest;
      From.Parsetree.ptype_attributes = ptype_attributes;
      From.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      To.Parsetree.ptype_name =
        (copy_loc (fun x  -> x) ptype_name);
      To.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptype_params);
      To.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_core_type x0),
                (copy_core_type x1),
                (copy_location x2))) ptype_cstrs);
      To.Parsetree.ptype_kind =
        (copy_type_kind ptype_kind);
      To.Parsetree.ptype_private =
        (copy_private_flag ptype_private);
      To.Parsetree.ptype_manifest =
        (copy_option copy_core_type ptype_manifest);
      To.Parsetree.ptype_attributes =
        (copy_attributes ptype_attributes);
      To.Parsetree.ptype_loc =
        (copy_location ptype_loc)
    }

and copy_private_flag :
  From.Asttypes.private_flag ->
    To.Asttypes.private_flag
  =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_type_kind :
  From.Parsetree.type_kind ->
    To.Parsetree.type_kind
  =
  function
  | From.Parsetree.Ptype_abstract  ->
      To.Parsetree.Ptype_abstract
  | From.Parsetree.Ptype_variant x0 ->
      To.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | From.Parsetree.Ptype_record x0 ->
      To.Parsetree.Ptype_record
        (List.map copy_label_declaration x0)
  | From.Parsetree.Ptype_open  ->
      To.Parsetree.Ptype_open

and copy_label_declaration :
  From.Parsetree.label_declaration ->
    To.Parsetree.label_declaration
  =
  fun
    { From.Parsetree.pld_name = pld_name;
      From.Parsetree.pld_mutable = pld_mutable;
      From.Parsetree.pld_type = pld_type;
      From.Parsetree.pld_loc = pld_loc;
      From.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      To.Parsetree.pld_name =
        (copy_loc (fun x  -> x) pld_name);
      To.Parsetree.pld_mutable =
        (copy_mutable_flag pld_mutable);
      To.Parsetree.pld_type =
        (copy_core_type pld_type);
      To.Parsetree.pld_loc =
        (copy_location pld_loc);
      To.Parsetree.pld_attributes =
        (copy_attributes pld_attributes)
    }

and copy_mutable_flag :
  From.Asttypes.mutable_flag ->
    To.Asttypes.mutable_flag
  =
  function
  | From.Asttypes.Immutable  ->
      To.Asttypes.Immutable
  | From.Asttypes.Mutable  -> To.Asttypes.Mutable

and copy_constructor_declaration :
  From.Parsetree.constructor_declaration ->
    To.Parsetree.constructor_declaration
  =
  fun
    { From.Parsetree.pcd_name = pcd_name;
      From.Parsetree.pcd_args = pcd_args;
      From.Parsetree.pcd_res = pcd_res;
      From.Parsetree.pcd_loc = pcd_loc;
      From.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      To.Parsetree.pcd_name =
        (copy_loc (fun x  -> x) pcd_name);
      To.Parsetree.pcd_args =
        To.Parsetree.Pcstr_tuple (List.map copy_core_type pcd_args);
      To.Parsetree.pcd_res =
        (copy_option copy_core_type pcd_res);
      To.Parsetree.pcd_loc =
        (copy_location pcd_loc);
      To.Parsetree.pcd_attributes =
        (copy_attributes pcd_attributes)
    }

and copy_variance :
  From.Asttypes.variance -> To.Asttypes.variance =
  function
  | From.Asttypes.Covariant  ->
      To.Asttypes.Covariant
  | From.Asttypes.Contravariant  ->
      To.Asttypes.Contravariant
  | From.Asttypes.Invariant  ->
      To.Asttypes.Invariant

and copy_value_description :
  From.Parsetree.value_description ->
    To.Parsetree.value_description
  =
  fun
    { From.Parsetree.pval_name = pval_name;
      From.Parsetree.pval_type = pval_type;
      From.Parsetree.pval_prim = pval_prim;
      From.Parsetree.pval_attributes = pval_attributes;
      From.Parsetree.pval_loc = pval_loc }
     ->
    {
      To.Parsetree.pval_name =
        (copy_loc (fun x  -> x) pval_name);
      To.Parsetree.pval_type =
        (copy_core_type pval_type);
      To.Parsetree.pval_prim =
        (List.map (fun x  -> x) pval_prim);
      To.Parsetree.pval_attributes =
        (copy_attributes pval_attributes);
      To.Parsetree.pval_loc =
        (copy_location pval_loc)
    }

and copy_closed_flag :
  From.Asttypes.closed_flag ->
    To.Asttypes.closed_flag
  =
  function
  | From.Asttypes.Closed  -> To.Asttypes.Closed
  | From.Asttypes.Open  -> To.Asttypes.Open

and copy_label :
  From.Asttypes.label -> To.Asttypes.label =
  fun x ->
    x

and copy_arg_label :
  From.Asttypes.label -> To.Asttypes.arg_label =
  fun x ->
    if x <> "" then
      if x.[0] = '?' then To.Asttypes.Optional (String.sub x 1 (String.length x - 1))
      else To.Asttypes.Labelled x
    else
      To.Asttypes.Nolabel



and copy_rec_flag :
  From.Asttypes.rec_flag -> To.Asttypes.rec_flag =
  function
  | From.Asttypes.Nonrecursive  ->
      To.Asttypes.Nonrecursive
  | From.Asttypes.Recursive  ->
      To.Asttypes.Recursive

and copy_constant :
  From.Asttypes.constant -> To.Parsetree.constant =
  function
  | From.Asttypes.Const_int x0 ->
      To.Parsetree.Pconst_integer (string_of_int x0, None)
  | From.Asttypes.Const_char x0 ->
      To.Parsetree.Pconst_char x0
  | From.Asttypes.Const_string (x0,x1) ->
      To.Parsetree.Pconst_string
        (x0, (copy_option (fun x  -> x) x1))
  | From.Asttypes.Const_float x0 ->
      To.Parsetree.Pconst_float (x0, None)
  | From.Asttypes.Const_int32 x0 ->
      To.Parsetree.Pconst_integer (Int32.to_string x0, Some 'l')
  | From.Asttypes.Const_int64 x0 ->
      To.Parsetree.Pconst_integer (Int64.to_string x0, Some 'L')
  | From.Asttypes.Const_nativeint x0 ->
      To.Parsetree.Pconst_integer (Nativeint.to_string x0, Some 'n')

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_longident :
  From.Longident.t -> To.Longident.t =
  function
  | From.Longident.Lident x0 ->
      To.Longident.Lident x0
  | From.Longident.Ldot (x0,x1) ->
      To.Longident.Ldot ((copy_longident x0), x1)
  | From.Longident.Lapply (x0,x1) ->
      To.Longident.Lapply ((copy_longident x0), (copy_longident x1))

and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Asttypes.loc -> 'g0 To.Asttypes.loc
  =
  fun f0  ->
    fun
      { From.Asttypes.txt = txt;
        From.Asttypes.loc = loc }
       ->
      {
        To.Asttypes.txt = (f0 txt);
        To.Asttypes.loc = (copy_location loc)
      }

and copy_location :
  From.Location.t -> To.Location.t =
  fun
    { From.Location.loc_start = loc_start;
      From.Location.loc_end = loc_end;
      From.Location.loc_ghost = loc_ghost }
     ->
    {
      To.Location.loc_start = (copy_Lexing_position loc_start);
      To.Location.loc_end = (copy_Lexing_position loc_end);
      To.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }

and type_declarations types =
  let is_nonrec (attr,_) = attr.To.Location.txt = "nonrec" in
  match List.map copy_type_declaration types with
  | (x :: xs)
    when List.exists is_nonrec x.To.Parsetree.ptype_attributes ->
      let ptype_attributes =
        List.filter (fun x -> not (is_nonrec x)) x.To.Parsetree.ptype_attributes
      in
      (To.Asttypes.Nonrecursive,
       {x with To.Parsetree.ptype_attributes} :: xs)
  | types -> (To.Asttypes.Recursive, types)

let rec copy_out_phrase :
  From.Outcometree.out_phrase -> To.Outcometree.out_phrase =
  function
  | From.Outcometree.Ophr_eval (x0,x1) ->
      To.Outcometree.Ophr_eval
        ((copy_out_value x0),
          (copy_out_type x1))
  | From.Outcometree.Ophr_signature x0 ->
      To.Outcometree.Ophr_signature
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_sig_item x0),
                (copy_option copy_out_value x1))) x0)
  | From.Outcometree.Ophr_exception x0 ->
      To.Outcometree.Ophr_exception
        (let (x0,x1) = x0  in
         ((copy_exn x0), (copy_out_value x1)))

and copy_exn : exn -> exn = fun x  -> x

and copy_out_sig_item :
  From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item =
  function
  | From.Outcometree.Osig_class (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_class_type (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class_type
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_typext (x0,x1) ->
      To.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0),
          (copy_out_ext_status x1))
  | From.Outcometree.Osig_modtype (x0,x1) ->
      To.Outcometree.Osig_modtype
        (x0, (copy_out_module_type x1))
  | From.Outcometree.Osig_module (x0,x1,x2) ->
      To.Outcometree.Osig_module
        (x0, (copy_out_module_type x1),
          (copy_out_rec_status x2))
  | From.Outcometree.Osig_type (x0,x1) ->
      To.Outcometree.Osig_type
        ((copy_out_type_decl x0),
          (copy_out_rec_status x1))
  | From.Outcometree.Osig_value (x0,x1,x2) ->
      To.Outcometree.Osig_value { To.Outcometree.
                                  oval_name = x0;
                                  oval_type = copy_out_type x1;
                                  oval_prims = List.map (fun x -> x) x2;
                                  oval_attributes = [] }

and copy_out_type_decl :
  From.Outcometree.out_type_decl -> To.Outcometree.out_type_decl =
  fun
    { From.Outcometree.otype_name = otype_name;
      From.Outcometree.otype_params = otype_params;
      From.Outcometree.otype_type = otype_type;
      From.Outcometree.otype_private = otype_private;
      From.Outcometree.otype_cstrs = otype_cstrs }
     ->
    {
      To.Outcometree.otype_name = otype_name;
      To.Outcometree.otype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
           otype_params);
      To.Outcometree.otype_type =
        (copy_out_type otype_type);
      To.Outcometree.otype_private =
        (copy_private_flag otype_private);
      To.Outcometree.otype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_type x0),
               (copy_out_type x1))) otype_cstrs);
      To.Outcometree.otype_immediate = false;
    }

and copy_out_module_type :
  From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  =
  function
  | From.Outcometree.Omty_abstract  -> To.Outcometree.Omty_abstract
  | From.Outcometree.Omty_functor (x0,x1,x2) ->
      To.Outcometree.Omty_functor
        (x0, (copy_option copy_out_module_type x1),
          (copy_out_module_type x2))
  | From.Outcometree.Omty_ident x0 ->
      To.Outcometree.Omty_ident (copy_out_ident x0)
  | From.Outcometree.Omty_signature x0 ->
      To.Outcometree.Omty_signature
        (List.map copy_out_sig_item x0)
  | From.Outcometree.Omty_alias x0 ->
      To.Outcometree.Omty_alias (copy_out_ident x0)

and copy_out_ext_status :
  From.Outcometree.out_ext_status -> To.Outcometree.out_ext_status =
  function
  | From.Outcometree.Oext_first  -> To.Outcometree.Oext_first
  | From.Outcometree.Oext_next  -> To.Outcometree.Oext_next
  | From.Outcometree.Oext_exception  -> To.Outcometree.Oext_exception

and copy_out_extension_constructor :
  From.Outcometree.out_extension_constructor ->
    To.Outcometree.out_extension_constructor
  =
  fun
    { From.Outcometree.oext_name = oext_name;
      From.Outcometree.oext_type_name = oext_type_name;
      From.Outcometree.oext_type_params = oext_type_params;
      From.Outcometree.oext_args = oext_args;
      From.Outcometree.oext_ret_type = oext_ret_type;
      From.Outcometree.oext_private = oext_private }
     ->
    {
      To.Outcometree.oext_name = oext_name;
      To.Outcometree.oext_type_name = oext_type_name;
      To.Outcometree.oext_type_params =
        (List.map (fun x  -> x) oext_type_params);
      To.Outcometree.oext_args =
        (List.map copy_out_type oext_args);
      To.Outcometree.oext_ret_type =
        (copy_option copy_out_type oext_ret_type);
      To.Outcometree.oext_private =
        (copy_private_flag oext_private)
    }

and copy_out_rec_status :
  From.Outcometree.out_rec_status -> To.Outcometree.out_rec_status =
  function
  | From.Outcometree.Orec_not  -> To.Outcometree.Orec_not
  | From.Outcometree.Orec_first  -> To.Outcometree.Orec_first
  | From.Outcometree.Orec_next  -> To.Outcometree.Orec_next

and copy_out_class_type :
  From.Outcometree.out_class_type -> To.Outcometree.out_class_type =
  function
  | From.Outcometree.Octy_constr (x0,x1) ->
      To.Outcometree.Octy_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Octy_arrow (x0,x1,x2) ->
      To.Outcometree.Octy_arrow
        (x0, (copy_out_type x1),
          (copy_out_class_type x2))
  | From.Outcometree.Octy_signature (x0,x1) ->
      To.Outcometree.Octy_signature
        ((copy_option copy_out_type x0),
          (List.map copy_out_class_sig_item x1))

and copy_out_class_sig_item :
  From.Outcometree.out_class_sig_item ->
    To.Outcometree.out_class_sig_item
  =
  function
  | From.Outcometree.Ocsg_constraint (x0,x1) ->
      To.Outcometree.Ocsg_constraint
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Ocsg_method (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_method
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))
  | From.Outcometree.Ocsg_value (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_value
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))

and copy_out_type :
  From.Outcometree.out_type -> To.Outcometree.out_type =
  function
  | From.Outcometree.Otyp_abstract  -> To.Outcometree.Otyp_abstract
  | From.Outcometree.Otyp_open  -> To.Outcometree.Otyp_open
  | From.Outcometree.Otyp_alias (x0,x1) ->
      To.Outcometree.Otyp_alias
        ((copy_out_type x0), x1)
  | From.Outcometree.Otyp_arrow (x0,x1,x2) ->
      To.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1),
          (copy_out_type x2))
  | From.Outcometree.Otyp_class (x0,x1,x2) ->
      To.Outcometree.Otyp_class
        ((copy_bool x0), (copy_out_ident x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_constr (x0,x1) ->
      To.Outcometree.Otyp_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Otyp_manifest (x0,x1) ->
      To.Outcometree.Otyp_manifest
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Otyp_object (x0,x1) ->
      To.Outcometree.Otyp_object
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               (x0, (copy_out_type x1))) x0),
          (copy_option copy_bool x1))
  | From.Outcometree.Otyp_record x0 ->
      To.Outcometree.Otyp_record
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1), (copy_out_type x2)))
           x0)
  | From.Outcometree.Otyp_stuff x0 -> To.Outcometree.Otyp_stuff x0
  | From.Outcometree.Otyp_sum x0 ->
      To.Outcometree.Otyp_sum
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2))) x0)
  | From.Outcometree.Otyp_tuple x0 ->
      To.Outcometree.Otyp_tuple
        (List.map copy_out_type x0)
  | From.Outcometree.Otyp_var (x0,x1) ->
      To.Outcometree.Otyp_var ((copy_bool x0), x1)
  | From.Outcometree.Otyp_variant (x0,x1,x2,x3) ->
      To.Outcometree.Otyp_variant
        ((copy_bool x0), (copy_out_variant x1),
          (copy_bool x2),
          (copy_option (fun x  -> List.map (fun x  -> x) x) x3))
  | From.Outcometree.Otyp_poly (x0,x1) ->
      To.Outcometree.Otyp_poly
        ((List.map (fun x  -> x) x0), (copy_out_type x1))
  | From.Outcometree.Otyp_module (x0,x1,x2) ->
      To.Outcometree.Otyp_module
        (x0, (List.map (fun x  -> x) x1),
          (List.map copy_out_type x2))

and copy_out_variant :
  From.Outcometree.out_variant -> To.Outcometree.out_variant =
  function
  | From.Outcometree.Ovar_fields x0 ->
      To.Outcometree.Ovar_fields
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1),
                (List.map copy_out_type x2))) x0)
  | From.Outcometree.Ovar_name (x0,x1) ->
      To.Outcometree.Ovar_name
        ((copy_out_ident x0),
          (List.map copy_out_type x1))

and copy_out_value :
  From.Outcometree.out_value -> To.Outcometree.out_value =
  function
  | From.Outcometree.Oval_array x0 ->
      To.Outcometree.Oval_array
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_char x0 -> To.Outcometree.Oval_char x0
  | From.Outcometree.Oval_constr (x0,x1) ->
      To.Outcometree.Oval_constr
        ((copy_out_ident x0),
          (List.map copy_out_value x1))
  | From.Outcometree.Oval_ellipsis  -> To.Outcometree.Oval_ellipsis
  | From.Outcometree.Oval_float x0 ->
      To.Outcometree.Oval_float (copy_float x0)
  | From.Outcometree.Oval_int x0 -> To.Outcometree.Oval_int x0
  | From.Outcometree.Oval_int32 x0 -> To.Outcometree.Oval_int32 x0
  | From.Outcometree.Oval_int64 x0 -> To.Outcometree.Oval_int64 x0
  | From.Outcometree.Oval_nativeint x0 ->
      To.Outcometree.Oval_nativeint x0
  | From.Outcometree.Oval_list x0 ->
      To.Outcometree.Oval_list
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_printer x0 ->
      To.Outcometree.Oval_printer x0
  | From.Outcometree.Oval_record x0 ->
      To.Outcometree.Oval_record
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_ident x0),
                (copy_out_value x1))) x0)
  | From.Outcometree.Oval_string x0 -> To.Outcometree.Oval_string x0
  | From.Outcometree.Oval_stuff x0 -> To.Outcometree.Oval_stuff x0
  | From.Outcometree.Oval_tuple x0 ->
      To.Outcometree.Oval_tuple
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_variant (x0,x1) ->
      To.Outcometree.Oval_variant
        (x0, (copy_option copy_out_value x1))

and copy_float : float -> float = fun x  -> x

and copy_out_ident :
  From.Outcometree.out_ident -> To.Outcometree.out_ident =
  function
  | From.Outcometree.Oide_apply (x0,x1) ->
      To.Outcometree.Oide_apply
        ((copy_out_ident x0),
          (copy_out_ident x1))
  | From.Outcometree.Oide_dot (x0,x1) ->
      To.Outcometree.Oide_dot
        ((copy_out_ident x0), x1)
  | From.Outcometree.Oide_ident x0 -> To.Outcometree.Oide_ident x0

let rec copy_toplevel_phrase :
  From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase =
  function
  | From.Parsetree.Ptop_def x0 ->
      To.Parsetree.Ptop_def (copy_structure x0)
  | From.Parsetree.Ptop_dir (x0,x1) ->
      To.Parsetree.Ptop_dir
        (x0, (copy_directive_argument x1))

and copy_directive_argument :
  From.Parsetree.directive_argument -> To.Parsetree.directive_argument =
  function
  | From.Parsetree.Pdir_none  -> To.Parsetree.Pdir_none
  | From.Parsetree.Pdir_string x0 -> To.Parsetree.Pdir_string x0
  | From.Parsetree.Pdir_int x0 -> To.Parsetree.Pdir_int (string_of_int x0, None)
  | From.Parsetree.Pdir_ident x0 -> To.Parsetree.Pdir_ident (copy_longident x0)
  | From.Parsetree.Pdir_bool x0 -> To.Parsetree.Pdir_bool (copy_bool x0)

let copy_out_type_extension :
  From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension =
  fun
    { From.Outcometree.otyext_name = otyext_name;
      From.Outcometree.otyext_params = otyext_params;
      From.Outcometree.otyext_constructors = otyext_constructors;
      From.Outcometree.otyext_private = otyext_private }
     ->
    {
      To.Outcometree.otyext_name = otyext_name;
      To.Outcometree.otyext_params =
        (List.map (fun x  -> x) otyext_params);
      To.Outcometree.otyext_constructors =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2)))
           otyext_constructors);
      To.Outcometree.otyext_private =
        (copy_private_flag otyext_private)
    }

let copy_cases x = List.map copy_case x
let copy_pat = copy_pattern
let copy_expr = copy_expression
let copy_typ = copy_core_type

end
module Migrate_parsetree_def : sig 
#1 "migrate_parsetree_def.mli"
# 1 "src/migrate_parsetree_def.mli"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Features which are not available in all versions of the frontend *)
type missing_feature =
    Pexp_letexception
  | Ppat_open
  | Pexp_unreachable
  | PSig
  | Pcstr_record
  | Pconst_integer
  | Pconst_float

(** Exception thrown by migration functions when a feature is not supported. *)
exception Migration_error of missing_feature * Location.t

(** [missing_feature_description x] is a text describing the feature [x]. *)
val missing_feature_description : missing_feature -> string

(** [missing_feature_minimal_version x] is the OCaml version where x was
    introduced. *)
val missing_feature_minimal_version : missing_feature -> string

(** Turn a missing feature into a reasonable error message. *)
val migration_error_message : missing_feature -> string

end = struct
#1 "migrate_parsetree_def.ml"
# 1 "src/migrate_parsetree_def.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Errors that can happen when converting constructions that doesn't exist in
    older version of the AST. *)
type missing_feature =
  | Pexp_letexception
    (** 4.04 -> 4.03: local exception, let exception _ in ... *)
  | Ppat_open
    (** 4.04 -> 4.03: module open in pattern match x with M.(_) -> ... *)
  | Pexp_unreachable
    (** 4.04 -> 4.03: unreachable pattern -> . *)
  | PSig
    (** 4.03 -> 4.02: signature in attribute, [@: val x : int] *)
  | Pcstr_record
    (** 4.03 -> 4.02: inline record *)
  | Pconst_integer
    (** 4.03 -> 4.02: integer literal with invalid suffix, 1234d *)
  | Pconst_float
    (** 4.03 -> 4.02: float literal with invalid suffix, 1234.0g *)

exception Migration_error of missing_feature * Location.t

(** [missing_feature_description x] is a text describing the feature [x]. *)
let missing_feature_description = function
  | Pexp_letexception -> "local exceptions"
  | Ppat_open         -> "module open in patterns"
  | Pexp_unreachable  -> "unreachable patterns"
  | PSig              -> "signatures in attribute"
  | Pcstr_record      -> "inline records"
  | Pconst_integer    -> "custom integer literals"
  | Pconst_float      -> "custom float literals"

(** [missing_feature_minimal_version x] is the OCaml version where x was
    introduced. *)
let missing_feature_minimal_version = function
  | Pexp_letexception -> "OCaml 4.04"
  | Ppat_open         -> "OCaml 4.04"
  | Pexp_unreachable  -> "OCaml 4.03"
  | PSig              -> "OCaml 4.03"
  | Pcstr_record      -> "OCaml 4.03"
  | Pconst_integer    -> "OCaml 4.03"
  | Pconst_float      -> "OCaml 4.03"

(** Turn a missing feature into a reasonable error message. *)
let migration_error_message x =
  let feature = missing_feature_description x in
  let version = missing_feature_minimal_version x in
  feature ^ " are not supported before OCaml " ^ version

let () =
  let location_prefix l =
    if l = Location.none then "" else
      let {Location.loc_start; loc_end; _} = l in
      let bol = loc_start.Lexing.pos_bol in
      Printf.sprintf "File %S, line %d, characters %d-%d: "
        loc_start.Lexing.pos_fname
        loc_start.Lexing.pos_lnum
        (loc_start.Lexing.pos_cnum - bol)
        (loc_end.Lexing.pos_cnum - bol)
  in
  Printexc.register_printer (function
      | Migration_error (err, loc) ->
          Some (location_prefix loc ^ migration_error_message err)
      | _ -> None
    )

end
module Migrate_parsetree_403_402_migrate
= struct
#1 "migrate_parsetree_403_402_migrate.ml"
# 1 "src/migrate_parsetree_403_402_migrate.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Def = Migrate_parsetree_def
module From = Ast_403
module To = Ast_402

let inject_predef_option label d =
  let open To in
  let open Parsetree in
  match label with
  | From.Asttypes.Optional _ ->
    let loc = {d.ptyp_loc with Location.loc_ghost = true} in
    let txt = Longident.Ldot (Longident.Lident "*predef*", "option") in
    let ident = {Location. txt; loc} in
    { ptyp_desc = Ptyp_constr(ident,[d]); ptyp_loc = loc; ptyp_attributes = []}
  | _ -> d

let from_loc {From.Location. txt = _; loc} = loc

let migration_error location feature =
  raise (Def.Migration_error (feature, location))

let rec copy_expression :
  From.Parsetree.expression ->
    To.Parsetree.expression
  =
  fun
    { From.Parsetree.pexp_desc = pexp_desc;
      From.Parsetree.pexp_loc = pexp_loc;
      From.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      To.Parsetree.pexp_desc =
        (copy_expression_desc pexp_loc pexp_desc);
      To.Parsetree.pexp_loc =
        (copy_location pexp_loc);
      To.Parsetree.pexp_attributes =
        (copy_attributes pexp_attributes)
    }

and copy_expression_desc loc :
  From.Parsetree.expression_desc ->
    To.Parsetree.expression_desc
  =
  function
  | From.Parsetree.Pexp_ident x0 ->
      To.Parsetree.Pexp_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_constant x0 ->
      To.Parsetree.Pexp_constant
        (copy_constant loc x0)
  | From.Parsetree.Pexp_let (x0,x1,x2) ->
      To.Parsetree.Pexp_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_function x0 ->
      To.Parsetree.Pexp_function
        (List.map copy_case x0)
  | From.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pexp_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_expression x3))
  | From.Parsetree.Pexp_apply (x0,x1) ->
      To.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pexp_match (x0,x1) ->
      To.Parsetree.Pexp_match
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_try (x0,x1) ->
      To.Parsetree.Pexp_try
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_tuple x0 ->
      To.Parsetree.Pexp_tuple
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_construct (x0,x1) ->
      To.Parsetree.Pexp_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_variant (x0,x1) ->
      To.Parsetree.Pexp_variant
        ((copy_label x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_record (x0,x1) ->
      To.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_expression x1))) x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_field (x0,x1) ->
      To.Parsetree.Pexp_field
        ((copy_expression x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pexp_setfield (x0,x1,x2) ->
      To.Parsetree.Pexp_setfield
        ((copy_expression x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_array x0 ->
      To.Parsetree.Pexp_array
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      To.Parsetree.Pexp_ifthenelse
        ((copy_expression x0),
          (copy_expression x1),
          (copy_option copy_expression x2))
  | From.Parsetree.Pexp_sequence (x0,x1) ->
      To.Parsetree.Pexp_sequence
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_while (x0,x1) ->
      To.Parsetree.Pexp_while
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      To.Parsetree.Pexp_for
        ((copy_pattern x0),
          (copy_expression x1),
          (copy_expression x2),
          (copy_direction_flag x3),
          (copy_expression x4))
  | From.Parsetree.Pexp_constraint (x0,x1) ->
      To.Parsetree.Pexp_constraint
        ((copy_expression x0),
          (copy_core_type x1))
  | From.Parsetree.Pexp_coerce (x0,x1,x2) ->
      To.Parsetree.Pexp_coerce
        ((copy_expression x0),
          (copy_option copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Pexp_send (x0,x1) ->
      To.Parsetree.Pexp_send
        ((copy_expression x0), x1)
  | From.Parsetree.Pexp_new x0 ->
      To.Parsetree.Pexp_new
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_setinstvar (x0,x1) ->
      To.Parsetree.Pexp_setinstvar
        ((copy_loc (fun x  -> x) x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_override x0 ->
      To.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_loc (fun x  -> x) x0),
                (copy_expression x1))) x0)
  | From.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      To.Parsetree.Pexp_letmodule
        ((copy_loc (fun x  -> x) x0),
          (copy_module_expr x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_assert x0 ->
      To.Parsetree.Pexp_assert
        (copy_expression x0)
  | From.Parsetree.Pexp_lazy x0 ->
      To.Parsetree.Pexp_lazy
        (copy_expression x0)
  | From.Parsetree.Pexp_poly (x0,x1) ->
      To.Parsetree.Pexp_poly
        ((copy_expression x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pexp_object x0 ->
      To.Parsetree.Pexp_object
        (copy_class_structure x0)
  | From.Parsetree.Pexp_newtype (x0,x1) ->
      To.Parsetree.Pexp_newtype
        (x0, (copy_expression x1))
  | From.Parsetree.Pexp_pack x0 ->
      To.Parsetree.Pexp_pack
        (copy_module_expr x0)
  | From.Parsetree.Pexp_open (x0,x1,x2) ->
      To.Parsetree.Pexp_open
        ((copy_override_flag x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_extension x0 ->
      To.Parsetree.Pexp_extension
        (copy_extension x0)
  | From.Parsetree.Pexp_unreachable  ->
      migration_error loc Def.Pexp_unreachable

and copy_direction_flag :
  From.Asttypes.direction_flag ->
    To.Asttypes.direction_flag
  =
  function
  | From.Asttypes.Upto  -> To.Asttypes.Upto
  | From.Asttypes.Downto  -> To.Asttypes.Downto

and copy_case :
  From.Parsetree.case -> To.Parsetree.case =
  fun
    { From.Parsetree.pc_lhs = pc_lhs;
      From.Parsetree.pc_guard = pc_guard;
      From.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      To.Parsetree.pc_lhs =
        (copy_pattern pc_lhs);
      To.Parsetree.pc_guard =
        (copy_option copy_expression pc_guard);
      To.Parsetree.pc_rhs =
        (copy_expression pc_rhs)
    }

and copy_value_binding :
  From.Parsetree.value_binding ->
    To.Parsetree.value_binding
  =
  fun
    { From.Parsetree.pvb_pat = pvb_pat;
      From.Parsetree.pvb_expr = pvb_expr;
      From.Parsetree.pvb_attributes = pvb_attributes;
      From.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      To.Parsetree.pvb_pat =
        (copy_pattern pvb_pat);
      To.Parsetree.pvb_expr =
        (copy_expression pvb_expr);
      To.Parsetree.pvb_attributes =
        (copy_attributes pvb_attributes);
      To.Parsetree.pvb_loc =
        (copy_location pvb_loc)
    }

and copy_pattern :
  From.Parsetree.pattern -> To.Parsetree.pattern =
  fun
    { From.Parsetree.ppat_desc = ppat_desc;
      From.Parsetree.ppat_loc = ppat_loc;
      From.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      To.Parsetree.ppat_desc =
        (copy_pattern_desc ppat_loc ppat_desc);
      To.Parsetree.ppat_loc =
        (copy_location ppat_loc);
      To.Parsetree.ppat_attributes =
        (copy_attributes ppat_attributes)
    }

and copy_pattern_desc loc :
  From.Parsetree.pattern_desc ->
    To.Parsetree.pattern_desc
  =
  function
  | From.Parsetree.Ppat_any  ->
      To.Parsetree.Ppat_any
  | From.Parsetree.Ppat_var x0 ->
      To.Parsetree.Ppat_var
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_alias (x0,x1) ->
      To.Parsetree.Ppat_alias
        ((copy_pattern x0),
          (copy_loc (fun x  -> x) x1))
  | From.Parsetree.Ppat_constant x0 ->
      To.Parsetree.Ppat_constant
        (copy_constant loc x0)
  | From.Parsetree.Ppat_interval (x0,x1) ->
      To.Parsetree.Ppat_interval
        ((copy_constant loc x0),
          (copy_constant loc x1))
  | From.Parsetree.Ppat_tuple x0 ->
      To.Parsetree.Ppat_tuple
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_construct (x0,x1) ->
      To.Parsetree.Ppat_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_variant (x0,x1) ->
      To.Parsetree.Ppat_variant
        ((copy_label x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_record (x0,x1) ->
      To.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ppat_array x0 ->
      To.Parsetree.Ppat_array
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_or (x0,x1) ->
      To.Parsetree.Ppat_or
        ((copy_pattern x0),
          (copy_pattern x1))
  | From.Parsetree.Ppat_constraint (x0,x1) ->
      To.Parsetree.Ppat_constraint
        ((copy_pattern x0),
          (copy_core_type x1))
  | From.Parsetree.Ppat_type x0 ->
      To.Parsetree.Ppat_type
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Ppat_lazy x0 ->
      To.Parsetree.Ppat_lazy
        (copy_pattern x0)
  | From.Parsetree.Ppat_unpack x0 ->
      To.Parsetree.Ppat_unpack
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_exception x0 ->
      To.Parsetree.Ppat_exception
        (copy_pattern x0)
  | From.Parsetree.Ppat_extension x0 ->
      To.Parsetree.Ppat_extension
        (copy_extension x0)

and copy_core_type :
  From.Parsetree.core_type ->
    To.Parsetree.core_type
  =
  fun
    { From.Parsetree.ptyp_desc = ptyp_desc;
      From.Parsetree.ptyp_loc = ptyp_loc;
      From.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      To.Parsetree.ptyp_desc =
        (copy_core_type_desc ptyp_desc);
      To.Parsetree.ptyp_loc =
        (copy_location ptyp_loc);
      To.Parsetree.ptyp_attributes =
        (copy_attributes ptyp_attributes)
    }

and copy_core_type_desc :
  From.Parsetree.core_type_desc ->
    To.Parsetree.core_type_desc
  =
  function
  | From.Parsetree.Ptyp_any  ->
      To.Parsetree.Ptyp_any
  | From.Parsetree.Ptyp_var x0 ->
      To.Parsetree.Ptyp_var x0
  | From.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      To.Parsetree.Ptyp_arrow
        ((copy_arg_label x0),
          inject_predef_option x0 (copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Ptyp_tuple x0 ->
      To.Parsetree.Ptyp_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Ptyp_constr (x0,x1) ->
      To.Parsetree.Ptyp_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_object (x0,x1) ->
      To.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (x0, (copy_attributes x1),
                 (copy_core_type x2))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ptyp_class (x0,x1) ->
      To.Parsetree.Ptyp_class
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_alias (x0,x1) ->
      To.Parsetree.Ptyp_alias
        ((copy_core_type x0), x1)
  | From.Parsetree.Ptyp_variant (x0,x1,x2) ->
      To.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0),
          (copy_closed_flag x1),
          (copy_option
             (fun x  -> List.map copy_label x) x2))
  | From.Parsetree.Ptyp_poly (x0,x1) ->
      To.Parsetree.Ptyp_poly
        ((List.map (fun x  -> x) x0),
          (copy_core_type x1))
  | From.Parsetree.Ptyp_package x0 ->
      To.Parsetree.Ptyp_package
        (copy_package_type x0)
  | From.Parsetree.Ptyp_extension x0 ->
      To.Parsetree.Ptyp_extension
        (copy_extension x0)

and copy_package_type :
  From.Parsetree.package_type ->
    To.Parsetree.package_type
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc copy_longident x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_loc
                copy_longident x0),
              (copy_core_type x1))) x1))

and copy_row_field :
  From.Parsetree.row_field ->
    To.Parsetree.row_field
  =
  function
  | From.Parsetree.Rtag (x0,x1,x2,x3) ->
      To.Parsetree.Rtag
        ((copy_label x0),
          (copy_attributes x1), (copy_bool x2),
          (List.map copy_core_type x3))
  | From.Parsetree.Rinherit x0 ->
      To.Parsetree.Rinherit
        (copy_core_type x0)

and copy_attributes :
  From.Parsetree.attributes ->
    To.Parsetree.attributes
  = fun x  -> List.map copy_attribute x

and copy_attribute :
  From.Parsetree.attribute ->
    To.Parsetree.attribute
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload (from_loc x0) x1))

and copy_payload loc :
  From.Parsetree.payload -> To.Parsetree.payload =
  function
  | From.Parsetree.PStr x0 ->
      To.Parsetree.PStr
        (copy_structure x0)
  | From.Parsetree.PSig _x0 ->
      migration_error loc Def.PSig
  | From.Parsetree.PTyp x0 ->
      To.Parsetree.PTyp
        (copy_core_type x0)
  | From.Parsetree.PPat (x0,x1) ->
      To.Parsetree.PPat
        ((copy_pattern x0),
          (copy_option copy_expression x1))

and copy_structure :
  From.Parsetree.structure ->
    To.Parsetree.structure
  = fun x  -> List.map copy_structure_item x

and copy_structure_item :
  From.Parsetree.structure_item ->
    To.Parsetree.structure_item
  =
  fun
    { From.Parsetree.pstr_desc = pstr_desc;
      From.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      To.Parsetree.pstr_desc =
        (copy_structure_item_desc pstr_desc);
      To.Parsetree.pstr_loc =
        (copy_location pstr_loc)
    }

and copy_structure_item_desc :
  From.Parsetree.structure_item_desc ->
    To.Parsetree.structure_item_desc
  =
  function
  | From.Parsetree.Pstr_eval (x0,x1) ->
      To.Parsetree.Pstr_eval
        ((copy_expression x0),
          (copy_attributes x1))
  | From.Parsetree.Pstr_value (x0,x1) ->
      To.Parsetree.Pstr_value
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1))
  | From.Parsetree.Pstr_primitive x0 ->
      To.Parsetree.Pstr_primitive
        (copy_value_description x0)
  | From.Parsetree.Pstr_type (x0,x1) ->
      To.Parsetree.Pstr_type (type_declarations x0 x1)
  | From.Parsetree.Pstr_typext x0 ->
      To.Parsetree.Pstr_typext
        (copy_type_extension x0)
  | From.Parsetree.Pstr_exception x0 ->
      To.Parsetree.Pstr_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Pstr_module x0 ->
      To.Parsetree.Pstr_module
        (copy_module_binding x0)
  | From.Parsetree.Pstr_recmodule x0 ->
      To.Parsetree.Pstr_recmodule
        (List.map copy_module_binding x0)
  | From.Parsetree.Pstr_modtype x0 ->
      To.Parsetree.Pstr_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Pstr_open x0 ->
      To.Parsetree.Pstr_open
        (copy_open_description x0)
  | From.Parsetree.Pstr_class x0 ->
      To.Parsetree.Pstr_class
        (List.map copy_class_declaration x0)
  | From.Parsetree.Pstr_class_type x0 ->
      To.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Pstr_include x0 ->
      To.Parsetree.Pstr_include
        (copy_include_declaration x0)
  | From.Parsetree.Pstr_attribute x0 ->
      To.Parsetree.Pstr_attribute
        (copy_attribute x0)
  | From.Parsetree.Pstr_extension (x0,x1) ->
      To.Parsetree.Pstr_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_include_declaration :
  From.Parsetree.include_declaration ->
    To.Parsetree.include_declaration
  =
  fun x  ->
    copy_include_infos
      copy_module_expr x

and copy_class_declaration :
  From.Parsetree.class_declaration ->
    To.Parsetree.class_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_expr x

and copy_class_expr :
  From.Parsetree.class_expr ->
    To.Parsetree.class_expr
  =
  fun
    { From.Parsetree.pcl_desc = pcl_desc;
      From.Parsetree.pcl_loc = pcl_loc;
      From.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      To.Parsetree.pcl_desc =
        (copy_class_expr_desc pcl_desc);
      To.Parsetree.pcl_loc =
        (copy_location pcl_loc);
      To.Parsetree.pcl_attributes =
        (copy_attributes pcl_attributes)
    }

and copy_class_expr_desc :
  From.Parsetree.class_expr_desc ->
    To.Parsetree.class_expr_desc
  =
  function
  | From.Parsetree.Pcl_constr (x0,x1) ->
      To.Parsetree.Pcl_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcl_structure x0 ->
      To.Parsetree.Pcl_structure
        (copy_class_structure x0)
  | From.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pcl_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_class_expr x3))
  | From.Parsetree.Pcl_apply (x0,x1) ->
      To.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pcl_let (x0,x1,x2) ->
      To.Parsetree.Pcl_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | From.Parsetree.Pcl_constraint (x0,x1) ->
      To.Parsetree.Pcl_constraint
        ((copy_class_expr x0),
          (copy_class_type x1))
  | From.Parsetree.Pcl_extension x0 ->
      To.Parsetree.Pcl_extension
        (copy_extension x0)

and copy_class_structure :
  From.Parsetree.class_structure ->
    To.Parsetree.class_structure
  =
  fun
    { From.Parsetree.pcstr_self = pcstr_self;
      From.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      To.Parsetree.pcstr_self =
        (copy_pattern pcstr_self);
      To.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }

and copy_class_field :
  From.Parsetree.class_field ->
    To.Parsetree.class_field
  =
  fun
    { From.Parsetree.pcf_desc = pcf_desc;
      From.Parsetree.pcf_loc = pcf_loc;
      From.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      To.Parsetree.pcf_desc =
        (copy_class_field_desc pcf_desc);
      To.Parsetree.pcf_loc =
        (copy_location pcf_loc);
      To.Parsetree.pcf_attributes =
        (copy_attributes pcf_attributes)
    }

and copy_class_field_desc :
  From.Parsetree.class_field_desc ->
    To.Parsetree.class_field_desc
  =
  function
  | From.Parsetree.Pcf_inherit (x0,x1,x2) ->
      To.Parsetree.Pcf_inherit
        ((copy_override_flag x0),
          (copy_class_expr x1),
          (copy_option (fun x  -> x) x2))
  | From.Parsetree.Pcf_val x0 ->
      To.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_method x0 ->
      To.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_constraint x0 ->
      To.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pcf_initializer x0 ->
      To.Parsetree.Pcf_initializer
        (copy_expression x0)
  | From.Parsetree.Pcf_attribute x0 ->
      To.Parsetree.Pcf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pcf_extension x0 ->
      To.Parsetree.Pcf_extension
        (copy_extension x0)

and copy_class_field_kind :
  From.Parsetree.class_field_kind ->
    To.Parsetree.class_field_kind
  =
  function
  | From.Parsetree.Cfk_virtual x0 ->
      To.Parsetree.Cfk_virtual
        (copy_core_type x0)
  | From.Parsetree.Cfk_concrete (x0,x1) ->
      To.Parsetree.Cfk_concrete
        ((copy_override_flag x0),
          (copy_expression x1))

and copy_module_binding :
  From.Parsetree.module_binding ->
    To.Parsetree.module_binding
  =
  fun
    { From.Parsetree.pmb_name = pmb_name;
      From.Parsetree.pmb_expr = pmb_expr;
      From.Parsetree.pmb_attributes = pmb_attributes;
      From.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      To.Parsetree.pmb_name =
        (copy_loc (fun x  -> x) pmb_name);
      To.Parsetree.pmb_expr =
        (copy_module_expr pmb_expr);
      To.Parsetree.pmb_attributes =
        (copy_attributes pmb_attributes);
      To.Parsetree.pmb_loc =
        (copy_location pmb_loc)
    }

and copy_module_expr :
  From.Parsetree.module_expr ->
    To.Parsetree.module_expr
  =
  fun
    { From.Parsetree.pmod_desc = pmod_desc;
      From.Parsetree.pmod_loc = pmod_loc;
      From.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      To.Parsetree.pmod_desc =
        (copy_module_expr_desc pmod_desc);
      To.Parsetree.pmod_loc =
        (copy_location pmod_loc);
      To.Parsetree.pmod_attributes =
        (copy_attributes pmod_attributes)
    }

and copy_module_expr_desc :
  From.Parsetree.module_expr_desc ->
    To.Parsetree.module_expr_desc
  =
  function
  | From.Parsetree.Pmod_ident x0 ->
      To.Parsetree.Pmod_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmod_structure x0 ->
      To.Parsetree.Pmod_structure
        (copy_structure x0)
  | From.Parsetree.Pmod_functor (x0,x1,x2) ->
      To.Parsetree.Pmod_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_expr x2))
  | From.Parsetree.Pmod_apply (x0,x1) ->
      To.Parsetree.Pmod_apply
        ((copy_module_expr x0),
          (copy_module_expr x1))
  | From.Parsetree.Pmod_constraint (x0,x1) ->
      To.Parsetree.Pmod_constraint
        ((copy_module_expr x0),
          (copy_module_type x1))
  | From.Parsetree.Pmod_unpack x0 ->
      To.Parsetree.Pmod_unpack
        (copy_expression x0)
  | From.Parsetree.Pmod_extension x0 ->
      To.Parsetree.Pmod_extension
        (copy_extension x0)

and copy_module_type :
  From.Parsetree.module_type ->
    To.Parsetree.module_type
  =
  fun
    { From.Parsetree.pmty_desc = pmty_desc;
      From.Parsetree.pmty_loc = pmty_loc;
      From.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      To.Parsetree.pmty_desc =
        (copy_module_type_desc pmty_desc);
      To.Parsetree.pmty_loc =
        (copy_location pmty_loc);
      To.Parsetree.pmty_attributes =
        (copy_attributes pmty_attributes)
    }

and copy_module_type_desc :
  From.Parsetree.module_type_desc ->
    To.Parsetree.module_type_desc
  =
  function
  | From.Parsetree.Pmty_ident x0 ->
      To.Parsetree.Pmty_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmty_signature x0 ->
      To.Parsetree.Pmty_signature
        (copy_signature x0)
  | From.Parsetree.Pmty_functor (x0,x1,x2) ->
      To.Parsetree.Pmty_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_type x2))
  | From.Parsetree.Pmty_with (x0,x1) ->
      To.Parsetree.Pmty_with
        ((copy_module_type x0),
          (List.map copy_with_constraint x1))
  | From.Parsetree.Pmty_typeof x0 ->
      To.Parsetree.Pmty_typeof
        (copy_module_expr x0)
  | From.Parsetree.Pmty_extension x0 ->
      To.Parsetree.Pmty_extension
        (copy_extension x0)
  | From.Parsetree.Pmty_alias x0 ->
      To.Parsetree.Pmty_alias
        (copy_loc copy_longident
           x0)

and copy_with_constraint :
  From.Parsetree.with_constraint ->
    To.Parsetree.with_constraint
  =
  function
  | From.Parsetree.Pwith_type (x0,x1) ->
      To.Parsetree.Pwith_type
        ((copy_loc
            copy_longident x0),
          (copy_type_declaration x1))
  | From.Parsetree.Pwith_module (x0,x1) ->
      To.Parsetree.Pwith_module
        ((copy_loc
            copy_longident x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pwith_typesubst x0 ->
      To.Parsetree.Pwith_typesubst
        (copy_type_declaration x0)
  | From.Parsetree.Pwith_modsubst (x0,x1) ->
      To.Parsetree.Pwith_modsubst
        ((copy_loc (fun x  -> x) x0),
          (copy_loc
             copy_longident x1))

and copy_signature :
  From.Parsetree.signature ->
    To.Parsetree.signature
  = fun x  -> List.map copy_signature_item x

and copy_signature_item :
  From.Parsetree.signature_item ->
    To.Parsetree.signature_item
  =
  fun
    { From.Parsetree.psig_desc = psig_desc;
      From.Parsetree.psig_loc = psig_loc }
     ->
    {
      To.Parsetree.psig_desc =
        (copy_signature_item_desc psig_desc);
      To.Parsetree.psig_loc =
        (copy_location psig_loc)
    }

and copy_signature_item_desc :
  From.Parsetree.signature_item_desc ->
    To.Parsetree.signature_item_desc
  =
  function
  | From.Parsetree.Psig_value x0 ->
      To.Parsetree.Psig_value
        (copy_value_description x0)
  | From.Parsetree.Psig_type (x0,x1) ->
      To.Parsetree.Psig_type (type_declarations x0 x1)
  | From.Parsetree.Psig_typext x0 ->
      To.Parsetree.Psig_typext
        (copy_type_extension x0)
  | From.Parsetree.Psig_exception x0 ->
      To.Parsetree.Psig_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Psig_module x0 ->
      To.Parsetree.Psig_module
        (copy_module_declaration x0)
  | From.Parsetree.Psig_recmodule x0 ->
      To.Parsetree.Psig_recmodule
        (List.map copy_module_declaration x0)
  | From.Parsetree.Psig_modtype x0 ->
      To.Parsetree.Psig_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Psig_open x0 ->
      To.Parsetree.Psig_open
        (copy_open_description x0)
  | From.Parsetree.Psig_include x0 ->
      To.Parsetree.Psig_include
        (copy_include_description x0)
  | From.Parsetree.Psig_class x0 ->
      To.Parsetree.Psig_class
        (List.map copy_class_description x0)
  | From.Parsetree.Psig_class_type x0 ->
      To.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Psig_attribute x0 ->
      To.Parsetree.Psig_attribute
        (copy_attribute x0)
  | From.Parsetree.Psig_extension (x0,x1) ->
      To.Parsetree.Psig_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_class_type_declaration :
  From.Parsetree.class_type_declaration ->
    To.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_description :
  From.Parsetree.class_description ->
    To.Parsetree.class_description
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_type :
  From.Parsetree.class_type ->
    To.Parsetree.class_type
  =
  fun
    { From.Parsetree.pcty_desc = pcty_desc;
      From.Parsetree.pcty_loc = pcty_loc;
      From.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      To.Parsetree.pcty_desc =
        (copy_class_type_desc pcty_desc);
      To.Parsetree.pcty_loc =
        (copy_location pcty_loc);
      To.Parsetree.pcty_attributes =
        (copy_attributes pcty_attributes)
    }

and copy_class_type_desc :
  From.Parsetree.class_type_desc ->
    To.Parsetree.class_type_desc
  =
  function
  | From.Parsetree.Pcty_constr (x0,x1) ->
      To.Parsetree.Pcty_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcty_signature x0 ->
      To.Parsetree.Pcty_signature
        (copy_class_signature x0)
  | From.Parsetree.Pcty_arrow (x0,x1,x2) ->
      To.Parsetree.Pcty_arrow
        ((copy_arg_label x0),
          inject_predef_option x0 (copy_core_type x1),
          (copy_class_type x2))
  | From.Parsetree.Pcty_extension x0 ->
      To.Parsetree.Pcty_extension
        (copy_extension x0)

and copy_class_signature :
  From.Parsetree.class_signature ->
    To.Parsetree.class_signature
  =
  fun
    { From.Parsetree.pcsig_self = pcsig_self;
      From.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      To.Parsetree.pcsig_self =
        (copy_core_type pcsig_self);
      To.Parsetree.pcsig_fields =
        (List.map copy_class_type_field
           pcsig_fields)
    }

and copy_class_type_field :
  From.Parsetree.class_type_field ->
    To.Parsetree.class_type_field
  =
  fun
    { From.Parsetree.pctf_desc = pctf_desc;
      From.Parsetree.pctf_loc = pctf_loc;
      From.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      To.Parsetree.pctf_desc =
        (copy_class_type_field_desc pctf_desc);
      To.Parsetree.pctf_loc =
        (copy_location pctf_loc);
      To.Parsetree.pctf_attributes =
        (copy_attributes pctf_attributes)
    }

and copy_class_type_field_desc :
  From.Parsetree.class_type_field_desc ->
    To.Parsetree.class_type_field_desc
  =
  function
  | From.Parsetree.Pctf_inherit x0 ->
      To.Parsetree.Pctf_inherit
        (copy_class_type x0)
  | From.Parsetree.Pctf_val x0 ->
      To.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_mutable_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_method x0 ->
      To.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_private_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_constraint x0 ->
      To.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pctf_attribute x0 ->
      To.Parsetree.Pctf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pctf_extension x0 ->
      To.Parsetree.Pctf_extension
        (copy_extension x0)

and copy_extension :
  From.Parsetree.extension ->
    To.Parsetree.extension
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload (from_loc x0) x1))

and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.class_infos ->
        'g0 To.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pci_virt = pci_virt;
        From.Parsetree.pci_params = pci_params;
        From.Parsetree.pci_name = pci_name;
        From.Parsetree.pci_expr = pci_expr;
        From.Parsetree.pci_loc = pci_loc;
        From.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        To.Parsetree.pci_virt =
          (copy_virtual_flag pci_virt);
        To.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_core_type x0),
                  (copy_variance x1))) pci_params);
        To.Parsetree.pci_name =
          (copy_loc (fun x  -> x) pci_name);
        To.Parsetree.pci_expr = (f0 pci_expr);
        To.Parsetree.pci_loc =
          (copy_location pci_loc);
        To.Parsetree.pci_attributes =
          (copy_attributes pci_attributes)
      }

and copy_virtual_flag :
  From.Asttypes.virtual_flag ->
    To.Asttypes.virtual_flag
  =
  function
  | From.Asttypes.Virtual  -> To.Asttypes.Virtual
  | From.Asttypes.Concrete  -> To.Asttypes.Concrete

and copy_include_description :
  From.Parsetree.include_description ->
    To.Parsetree.include_description
  =
  fun x  ->
    copy_include_infos
      copy_module_type x

and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.include_infos ->
        'g0 To.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pincl_mod = pincl_mod;
        From.Parsetree.pincl_loc = pincl_loc;
        From.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        To.Parsetree.pincl_mod = (f0 pincl_mod);
        To.Parsetree.pincl_loc =
          (copy_location pincl_loc);
        To.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }

and copy_open_description :
  From.Parsetree.open_description ->
    To.Parsetree.open_description
  =
  fun
    { From.Parsetree.popen_lid = popen_lid;
      From.Parsetree.popen_override = popen_override;
      From.Parsetree.popen_loc = popen_loc;
      From.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      To.Parsetree.popen_lid =
        (copy_loc copy_longident
           popen_lid);
      To.Parsetree.popen_override =
        (copy_override_flag popen_override);
      To.Parsetree.popen_loc =
        (copy_location popen_loc);
      To.Parsetree.popen_attributes =
        (copy_attributes popen_attributes)
    }

and copy_override_flag :
  From.Asttypes.override_flag ->
    To.Asttypes.override_flag
  =
  function
  | From.Asttypes.Override  -> To.Asttypes.Override
  | From.Asttypes.Fresh  -> To.Asttypes.Fresh

and copy_module_type_declaration :
  From.Parsetree.module_type_declaration ->
    To.Parsetree.module_type_declaration
  =
  fun
    { From.Parsetree.pmtd_name = pmtd_name;
      From.Parsetree.pmtd_type = pmtd_type;
      From.Parsetree.pmtd_attributes = pmtd_attributes;
      From.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      To.Parsetree.pmtd_name =
        (copy_loc (fun x  -> x) pmtd_name);
      To.Parsetree.pmtd_type =
        (copy_option copy_module_type pmtd_type);
      To.Parsetree.pmtd_attributes =
        (copy_attributes pmtd_attributes);
      To.Parsetree.pmtd_loc =
        (copy_location pmtd_loc)
    }

and copy_module_declaration :
  From.Parsetree.module_declaration ->
    To.Parsetree.module_declaration
  =
  fun
    { From.Parsetree.pmd_name = pmd_name;
      From.Parsetree.pmd_type = pmd_type;
      From.Parsetree.pmd_attributes = pmd_attributes;
      From.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      To.Parsetree.pmd_name =
        (copy_loc (fun x  -> x) pmd_name);
      To.Parsetree.pmd_type =
        (copy_module_type pmd_type);
      To.Parsetree.pmd_attributes =
        (copy_attributes pmd_attributes);
      To.Parsetree.pmd_loc =
        (copy_location pmd_loc)
    }

and copy_type_extension :
  From.Parsetree.type_extension ->
    To.Parsetree.type_extension
  =
  fun
    { From.Parsetree.ptyext_path = ptyext_path;
      From.Parsetree.ptyext_params = ptyext_params;
      From.Parsetree.ptyext_constructors = ptyext_constructors;
      From.Parsetree.ptyext_private = ptyext_private;
      From.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      To.Parsetree.ptyext_path =
        (copy_loc copy_longident
           ptyext_path);
      To.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptyext_params);
      To.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor
           ptyext_constructors);
      To.Parsetree.ptyext_private =
        (copy_private_flag ptyext_private);
      To.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }

and copy_extension_constructor :
  From.Parsetree.extension_constructor ->
    To.Parsetree.extension_constructor
  =
  fun
    { From.Parsetree.pext_name = pext_name;
      From.Parsetree.pext_kind = pext_kind;
      From.Parsetree.pext_loc = pext_loc;
      From.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      To.Parsetree.pext_name =
        (copy_loc (fun x  -> x) pext_name);
      To.Parsetree.pext_kind =
        (copy_extension_constructor_kind (from_loc pext_name) pext_kind);
      To.Parsetree.pext_loc =
        (copy_location pext_loc);
      To.Parsetree.pext_attributes =
        (copy_attributes pext_attributes)
    }

and copy_extension_constructor_kind loc :
  From.Parsetree.extension_constructor_kind ->
    To.Parsetree.extension_constructor_kind
  =
  function
  | From.Parsetree.Pext_decl (x0,x1) ->
      To.Parsetree.Pext_decl
        ((copy_constructor_arguments loc x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pext_rebind x0 ->
      To.Parsetree.Pext_rebind
        (copy_loc copy_longident
           x0)

and copy_type_declaration :
  From.Parsetree.type_declaration ->
    To.Parsetree.type_declaration
  =
  fun
    { From.Parsetree.ptype_name = ptype_name;
      From.Parsetree.ptype_params = ptype_params;
      From.Parsetree.ptype_cstrs = ptype_cstrs;
      From.Parsetree.ptype_kind = ptype_kind;
      From.Parsetree.ptype_private = ptype_private;
      From.Parsetree.ptype_manifest = ptype_manifest;
      From.Parsetree.ptype_attributes = ptype_attributes;
      From.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      To.Parsetree.ptype_name =
        (copy_loc (fun x  -> x) ptype_name);
      To.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptype_params);
      To.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_core_type x0),
                (copy_core_type x1),
                (copy_location x2))) ptype_cstrs);
      To.Parsetree.ptype_kind =
        (copy_type_kind ptype_kind);
      To.Parsetree.ptype_private =
        (copy_private_flag ptype_private);
      To.Parsetree.ptype_manifest =
        (copy_option copy_core_type ptype_manifest);
      To.Parsetree.ptype_attributes =
        (copy_attributes ptype_attributes);
      To.Parsetree.ptype_loc =
        (copy_location ptype_loc)
    }

and copy_private_flag :
  From.Asttypes.private_flag ->
    To.Asttypes.private_flag
  =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_type_kind :
  From.Parsetree.type_kind ->
    To.Parsetree.type_kind
  =
  function
  | From.Parsetree.Ptype_abstract  ->
      To.Parsetree.Ptype_abstract
  | From.Parsetree.Ptype_variant x0 ->
      To.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | From.Parsetree.Ptype_record x0 ->
      To.Parsetree.Ptype_record
        (List.map copy_label_declaration x0)
  | From.Parsetree.Ptype_open  ->
      To.Parsetree.Ptype_open

and copy_constructor_declaration :
  From.Parsetree.constructor_declaration ->
    To.Parsetree.constructor_declaration
  =
  fun
    { From.Parsetree.pcd_name = pcd_name;
      From.Parsetree.pcd_args = pcd_args;
      From.Parsetree.pcd_res = pcd_res;
      From.Parsetree.pcd_loc = pcd_loc;
      From.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      To.Parsetree.pcd_name =
        (copy_loc (fun x  -> x) pcd_name);
      To.Parsetree.pcd_args =
        (copy_constructor_arguments (from_loc pcd_name) pcd_args);
      To.Parsetree.pcd_res =
        (copy_option copy_core_type pcd_res);
      To.Parsetree.pcd_loc =
        (copy_location pcd_loc);
      To.Parsetree.pcd_attributes =
        (copy_attributes pcd_attributes)
    }

and copy_constructor_arguments loc :
  From.Parsetree.constructor_arguments ->
    To.Parsetree.core_type list
  =
  function
  | From.Parsetree.Pcstr_tuple x0 ->
      List.map copy_core_type x0
  | From.Parsetree.Pcstr_record _x0 ->
      migration_error loc Def.Pcstr_record

and copy_label_declaration :
  From.Parsetree.label_declaration ->
    To.Parsetree.label_declaration
  =
  fun
    { From.Parsetree.pld_name = pld_name;
      From.Parsetree.pld_mutable = pld_mutable;
      From.Parsetree.pld_type = pld_type;
      From.Parsetree.pld_loc = pld_loc;
      From.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      To.Parsetree.pld_name =
        (copy_loc (fun x  -> x) pld_name);
      To.Parsetree.pld_mutable =
        (copy_mutable_flag pld_mutable);
      To.Parsetree.pld_type =
        (copy_core_type pld_type);
      To.Parsetree.pld_loc =
        (copy_location pld_loc);
      To.Parsetree.pld_attributes =
        (copy_attributes pld_attributes)
    }

and copy_mutable_flag :
  From.Asttypes.mutable_flag ->
    To.Asttypes.mutable_flag
  =
  function
  | From.Asttypes.Immutable  ->
      To.Asttypes.Immutable
  | From.Asttypes.Mutable  -> To.Asttypes.Mutable

and copy_variance :
  From.Asttypes.variance -> To.Asttypes.variance =
  function
  | From.Asttypes.Covariant  ->
      To.Asttypes.Covariant
  | From.Asttypes.Contravariant  ->
      To.Asttypes.Contravariant
  | From.Asttypes.Invariant  ->
      To.Asttypes.Invariant

and copy_value_description :
  From.Parsetree.value_description ->
    To.Parsetree.value_description
  =
  fun
    { From.Parsetree.pval_name = pval_name;
      From.Parsetree.pval_type = pval_type;
      From.Parsetree.pval_prim = pval_prim;
      From.Parsetree.pval_attributes = pval_attributes;
      From.Parsetree.pval_loc = pval_loc }
     ->
    {
      To.Parsetree.pval_name =
        (copy_loc (fun x  -> x) pval_name);
      To.Parsetree.pval_type =
        (copy_core_type pval_type);
      To.Parsetree.pval_prim =
        (List.map (fun x  -> x) pval_prim);
      To.Parsetree.pval_attributes =
        (copy_attributes pval_attributes);
      To.Parsetree.pval_loc =
        (copy_location pval_loc)
    }

and copy_arg_label :
  From.Asttypes.arg_label -> string
  =
  function
  | From.Asttypes.Nolabel  -> ""
  | From.Asttypes.Labelled x0 -> x0
  | From.Asttypes.Optional x0 -> "?" ^ x0

and copy_closed_flag :
  From.Asttypes.closed_flag ->
    To.Asttypes.closed_flag
  =
  function
  | From.Asttypes.Closed  -> To.Asttypes.Closed
  | From.Asttypes.Open  -> To.Asttypes.Open

and copy_label :
  From.Asttypes.label -> To.Asttypes.label =
  fun x  -> x

and copy_rec_flag :
  From.Asttypes.rec_flag -> To.Asttypes.rec_flag =
  function
  | From.Asttypes.Nonrecursive  ->
      To.Asttypes.Nonrecursive
  | From.Asttypes.Recursive  ->
      To.Asttypes.Recursive

and copy_constant loc :
  From.Parsetree.constant -> To.Asttypes.constant
  =
  function
  | From.Parsetree.Pconst_integer (x0,x1) ->
     begin match x1 with
     | None -> To.Asttypes.Const_int (int_of_string x0)
     | Some 'l' ->
         To.Asttypes.Const_int32 (Int32.of_string x0)
     | Some 'L' ->
         To.Asttypes.Const_int64 (Int64.of_string x0)
     | Some 'n' ->
         To.Asttypes.Const_nativeint (Nativeint.of_string x0)
     | Some _ -> migration_error loc Def.Pconst_integer
     end
  | From.Parsetree.Pconst_char x0 ->
      To.Asttypes.Const_char x0
  | From.Parsetree.Pconst_string (x0,x1) ->
      To.Asttypes.Const_string (x0,x1)
  | From.Parsetree.Pconst_float (x0,x1) ->
      begin match x1 with
      | None -> To.Asttypes.Const_float x0
      | Some _ -> migration_error loc Def.Pconst_float
      end

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_longident : From.Longident.t -> To.Longident.t = function
  | From.Longident.Lident x0 ->
      To.Longident.Lident x0
  | From.Longident.Ldot (x0,x1) ->
      To.Longident.Ldot
        ((copy_longident x0), x1)
  | From.Longident.Lapply (x0,x1) ->
      To.Longident.Lapply
        ((copy_longident x0), (copy_longident x1))

and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Asttypes.loc -> 'g0 To.Asttypes.loc
  =
  fun f0  ->
    fun
      { From.Asttypes.txt = txt;
        From.Asttypes.loc = loc }
       ->
      {
        To.Asttypes.txt = (f0 txt);
        To.Asttypes.loc = copy_location loc
      }

and copy_location :
  From.Location.t -> To.Location.t =
  fun
    { From.Location.loc_start = loc_start;
      From.Location.loc_end = loc_end;
      From.Location.loc_ghost = loc_ghost }
     ->
    {
      To.Location.loc_start = (copy_Lexing_position loc_start);
      To.Location.loc_end = (copy_Lexing_position loc_end);
      To.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }

and type_declarations recflag types =
  match
    (recflag, List.map copy_type_declaration types)
  with
  | From.Asttypes.Recursive, types -> types
  | From.Asttypes.Nonrecursive, [] -> []
  | From.Asttypes.Nonrecursive, (x :: xs) ->
      let pos = {Lexing. pos_fname = "_none_"; pos_lnum = 1;
                 pos_bol = 0; pos_cnum = -1} in
      let loc = {To.Location. loc_start = pos; loc_end = pos;
                 loc_ghost = true} in
      let ptype_attributes =
        ({To.Asttypes.txt = "nonrec"; loc}, To.Parsetree.PStr []) ::
        x.To.Parsetree.ptype_attributes
      in
      {x with To.Parsetree.ptype_attributes} :: xs

let rec copy_out_phrase :
  From.Outcometree.out_phrase -> To.Outcometree.out_phrase =
  function
  | From.Outcometree.Ophr_eval (x0,x1) ->
      To.Outcometree.Ophr_eval
        ((copy_out_value x0),
          (copy_out_type x1))
  | From.Outcometree.Ophr_signature x0 ->
      To.Outcometree.Ophr_signature
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_sig_item x0),
                (copy_option copy_out_value x1))) x0)
  | From.Outcometree.Ophr_exception x0 ->
      To.Outcometree.Ophr_exception
        (let (x0,x1) = x0  in
         ((copy_exn x0), (copy_out_value x1)))

and copy_exn : exn -> exn = fun x  -> x

and copy_out_sig_item :
  From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item =
  function
  | From.Outcometree.Osig_class (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_class_type (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class_type
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_typext (x0,x1) ->
      To.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0),
          (copy_out_ext_status x1))
  | From.Outcometree.Osig_modtype (x0,x1) ->
      To.Outcometree.Osig_modtype
        (x0, (copy_out_module_type x1))
  | From.Outcometree.Osig_module (x0,x1,x2) ->
      To.Outcometree.Osig_module
        (x0, (copy_out_module_type x1),
          (copy_out_rec_status x2))
  | From.Outcometree.Osig_type (x0,x1) ->
      To.Outcometree.Osig_type
        ((copy_out_type_decl x0),
          (copy_out_rec_status x1))
  | From.Outcometree.Osig_value x0 -> copy_out_val_decl x0
  | From.Outcometree.Osig_ellipsis ->
      To.Outcometree.Osig_value ("...", To.Outcometree.Otyp_abstract, [])

and copy_out_val_decl :
  From.Outcometree.out_val_decl -> To.Outcometree.out_sig_item =
  fun
    { From.Outcometree.oval_name = oval_name;
      From.Outcometree.oval_type = oval_type;
      From.Outcometree.oval_prims = oval_prims;
      From.Outcometree.oval_attributes = _ }
     ->
       To.Outcometree.Osig_value (
         oval_name,
         copy_out_type oval_type,
         List.map (fun x  -> x) oval_prims
       )

and copy_out_type_decl :
  From.Outcometree.out_type_decl -> To.Outcometree.out_type_decl =
  fun
    { From.Outcometree.otype_name = otype_name;
      From.Outcometree.otype_params = otype_params;
      From.Outcometree.otype_type = otype_type;
      From.Outcometree.otype_private = otype_private;
      From.Outcometree.otype_immediate = _;
      From.Outcometree.otype_cstrs = otype_cstrs }
     ->
    {
      To.Outcometree.otype_name = otype_name;
      To.Outcometree.otype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
           otype_params);
      To.Outcometree.otype_type =
        (copy_out_type otype_type);
      To.Outcometree.otype_private =
        (copy_From_Asttypes_private_flag otype_private);
      (*To.Outcometree.otype_immediate = (copy_bool otype_immediate);*)
      To.Outcometree.otype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_type x0),
                (copy_out_type x1))) otype_cstrs)
    }

and copy_out_module_type :
  From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  =
  function
  | From.Outcometree.Omty_abstract  -> To.Outcometree.Omty_abstract
  | From.Outcometree.Omty_functor (x0,x1,x2) ->
      To.Outcometree.Omty_functor
        (x0, (copy_option copy_out_module_type x1),
          (copy_out_module_type x2))
  | From.Outcometree.Omty_ident x0 ->
      To.Outcometree.Omty_ident (copy_out_ident x0)
  | From.Outcometree.Omty_signature x0 ->
      To.Outcometree.Omty_signature
        (List.map copy_out_sig_item x0)
  | From.Outcometree.Omty_alias x0 ->
      To.Outcometree.Omty_alias (copy_out_ident x0)

and copy_out_ext_status :
  From.Outcometree.out_ext_status -> To.Outcometree.out_ext_status =
  function
  | From.Outcometree.Oext_first  -> To.Outcometree.Oext_first
  | From.Outcometree.Oext_next  -> To.Outcometree.Oext_next
  | From.Outcometree.Oext_exception  -> To.Outcometree.Oext_exception

and copy_out_extension_constructor :
  From.Outcometree.out_extension_constructor ->
    To.Outcometree.out_extension_constructor
  =
  fun
    { From.Outcometree.oext_name = oext_name;
      From.Outcometree.oext_type_name = oext_type_name;
      From.Outcometree.oext_type_params = oext_type_params;
      From.Outcometree.oext_args = oext_args;
      From.Outcometree.oext_ret_type = oext_ret_type;
      From.Outcometree.oext_private = oext_private }
     ->
    {
      To.Outcometree.oext_name = oext_name;
      To.Outcometree.oext_type_name = oext_type_name;
      To.Outcometree.oext_type_params =
        (List.map (fun x  -> x) oext_type_params);
      To.Outcometree.oext_args =
        (List.map copy_out_type oext_args);
      To.Outcometree.oext_ret_type =
        (copy_option copy_out_type oext_ret_type);
      To.Outcometree.oext_private =
        (copy_From_Asttypes_private_flag oext_private)
    }

and copy_From_Asttypes_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_out_rec_status :
  From.Outcometree.out_rec_status -> To.Outcometree.out_rec_status =
  function
  | From.Outcometree.Orec_not  -> To.Outcometree.Orec_not
  | From.Outcometree.Orec_first  -> To.Outcometree.Orec_first
  | From.Outcometree.Orec_next  -> To.Outcometree.Orec_next

and copy_out_class_type :
  From.Outcometree.out_class_type -> To.Outcometree.out_class_type =
  function
  | From.Outcometree.Octy_constr (x0,x1) ->
      To.Outcometree.Octy_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Octy_arrow (x0,x1,x2) ->
      To.Outcometree.Octy_arrow
        (x0, (copy_out_type x1),
          (copy_out_class_type x2))
  | From.Outcometree.Octy_signature (x0,x1) ->
      To.Outcometree.Octy_signature
        ((copy_option copy_out_type x0),
          (List.map copy_out_class_sig_item x1))

and copy_out_class_sig_item :
  From.Outcometree.out_class_sig_item ->
    To.Outcometree.out_class_sig_item
  =
  function
  | From.Outcometree.Ocsg_constraint (x0,x1) ->
      To.Outcometree.Ocsg_constraint
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Ocsg_method (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_method
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))
  | From.Outcometree.Ocsg_value (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_value
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))

and copy_out_type :
  From.Outcometree.out_type -> To.Outcometree.out_type =
  function
  | From.Outcometree.Otyp_abstract  -> To.Outcometree.Otyp_abstract
  | From.Outcometree.Otyp_open  -> To.Outcometree.Otyp_open
  | From.Outcometree.Otyp_alias (x0,x1) ->
      To.Outcometree.Otyp_alias
        ((copy_out_type x0), x1)
  | From.Outcometree.Otyp_arrow (x0,x1,x2) ->
      To.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1),
          (copy_out_type x2))
  | From.Outcometree.Otyp_class (x0,x1,x2) ->
      To.Outcometree.Otyp_class
        ((copy_bool x0), (copy_out_ident x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_constr (x0,x1) ->
      To.Outcometree.Otyp_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Otyp_manifest (x0,x1) ->
      To.Outcometree.Otyp_manifest
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Otyp_object (x0,x1) ->
      To.Outcometree.Otyp_object
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               (x0, (copy_out_type x1))) x0),
          (copy_option copy_bool x1))
  | From.Outcometree.Otyp_record x0 ->
      To.Outcometree.Otyp_record
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1), (copy_out_type x2)))
           x0)
  | From.Outcometree.Otyp_stuff x0 -> To.Outcometree.Otyp_stuff x0
  | From.Outcometree.Otyp_sum x0 ->
      To.Outcometree.Otyp_sum
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2))) x0)
  | From.Outcometree.Otyp_tuple x0 ->
      To.Outcometree.Otyp_tuple
        (List.map copy_out_type x0)
  | From.Outcometree.Otyp_var (x0,x1) ->
      To.Outcometree.Otyp_var ((copy_bool x0), x1)
  | From.Outcometree.Otyp_variant (x0,x1,x2,x3) ->
      To.Outcometree.Otyp_variant
        ((copy_bool x0), (copy_out_variant x1),
          (copy_bool x2),
          (copy_option (fun x  -> List.map (fun x  -> x) x) x3))
  | From.Outcometree.Otyp_poly (x0,x1) ->
      To.Outcometree.Otyp_poly
        ((List.map (fun x  -> x) x0), (copy_out_type x1))
  | From.Outcometree.Otyp_module (x0,x1,x2) ->
      To.Outcometree.Otyp_module
        (x0, (List.map (fun x  -> x) x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_attribute (_x0,_x1) ->
      To.Outcometree.Otyp_abstract
      (*To.Outcometree.Otyp_attribute
        ((copy_out_type x0),
          (copy_out_attribute x1))*)

(*and copy_out_attribute :
  From.Outcometree.out_attribute -> To.Outcometree.out_attribute =
  fun { From.Outcometree.oattr_name = oattr_name }  ->
    { To.Outcometree.oattr_name = oattr_name }*)

and copy_out_variant :
  From.Outcometree.out_variant -> To.Outcometree.out_variant =
  function
  | From.Outcometree.Ovar_fields x0 ->
      To.Outcometree.Ovar_fields
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1),
                (List.map copy_out_type x2))) x0)
  | From.Outcometree.Ovar_name (x0,x1) ->
      To.Outcometree.Ovar_name
        ((copy_out_ident x0),
          (List.map copy_out_type x1))

and copy_out_value :
  From.Outcometree.out_value -> To.Outcometree.out_value =
  function
  | From.Outcometree.Oval_array x0 ->
      To.Outcometree.Oval_array
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_char x0 -> To.Outcometree.Oval_char x0
  | From.Outcometree.Oval_constr (x0,x1) ->
      To.Outcometree.Oval_constr
        ((copy_out_ident x0),
          (List.map copy_out_value x1))
  | From.Outcometree.Oval_ellipsis  -> To.Outcometree.Oval_ellipsis
  | From.Outcometree.Oval_float x0 ->
      To.Outcometree.Oval_float (copy_float x0)
  | From.Outcometree.Oval_int x0 -> To.Outcometree.Oval_int x0
  | From.Outcometree.Oval_int32 x0 -> To.Outcometree.Oval_int32 x0
  | From.Outcometree.Oval_int64 x0 -> To.Outcometree.Oval_int64 x0
  | From.Outcometree.Oval_nativeint x0 ->
      To.Outcometree.Oval_nativeint x0
  | From.Outcometree.Oval_list x0 ->
      To.Outcometree.Oval_list
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_printer x0 ->
      To.Outcometree.Oval_printer x0
  | From.Outcometree.Oval_record x0 ->
      To.Outcometree.Oval_record
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_ident x0),
                (copy_out_value x1))) x0)
  | From.Outcometree.Oval_string x0 -> To.Outcometree.Oval_string x0
  | From.Outcometree.Oval_stuff x0 -> To.Outcometree.Oval_stuff x0
  | From.Outcometree.Oval_tuple x0 ->
      To.Outcometree.Oval_tuple
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_variant (x0,x1) ->
      To.Outcometree.Oval_variant
        (x0, (copy_option copy_out_value x1))

and copy_float : float -> float = fun x  -> x

and copy_out_ident :
  From.Outcometree.out_ident -> To.Outcometree.out_ident =
  function
  | From.Outcometree.Oide_apply (x0,x1) ->
      To.Outcometree.Oide_apply
        ((copy_out_ident x0),
          (copy_out_ident x1))
  | From.Outcometree.Oide_dot (x0,x1) ->
      To.Outcometree.Oide_dot
        ((copy_out_ident x0), x1)
  | From.Outcometree.Oide_ident x0 -> To.Outcometree.Oide_ident x0

let rec copy_toplevel_phrase :
  From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase =
  function
  | From.Parsetree.Ptop_def x0 ->
      To.Parsetree.Ptop_def (copy_structure x0)
  | From.Parsetree.Ptop_dir (x0,x1) ->
      To.Parsetree.Ptop_dir
        (x0, (copy_directive_argument x1))

and copy_directive_argument :
  From.Parsetree.directive_argument ->
    To.Parsetree.directive_argument
  =
  function
  | From.Parsetree.Pdir_none  -> To.Parsetree.Pdir_none
  | From.Parsetree.Pdir_string x0 -> To.Parsetree.Pdir_string x0
  | From.Parsetree.Pdir_int (x0,_x1) ->
      To.Parsetree.Pdir_int (int_of_string x0)
  | From.Parsetree.Pdir_ident x0 ->
      To.Parsetree.Pdir_ident (copy_longident x0)
  | From.Parsetree.Pdir_bool x0 ->
      To.Parsetree.Pdir_bool (copy_bool x0)

let copy_out_type_extension :
  From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension =
  fun
    { From.Outcometree.otyext_name = otyext_name;
      From.Outcometree.otyext_params = otyext_params;
      From.Outcometree.otyext_constructors = otyext_constructors;
      From.Outcometree.otyext_private = otyext_private }
     ->
    {
      To.Outcometree.otyext_name = otyext_name;
      To.Outcometree.otyext_params =
        (List.map (fun x  -> x) otyext_params);
      To.Outcometree.otyext_constructors =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2)))
           otyext_constructors);
      To.Outcometree.otyext_private =
        (copy_private_flag otyext_private)
    }

let copy_cases x = List.map copy_case x
let copy_pat = copy_pattern
let copy_expr = copy_expression
let copy_typ = copy_core_type

end
module Migrate_parsetree_402_403
= struct
#1 "migrate_parsetree_402_403.ml"
# 1 "src/migrate_parsetree_402_403.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Migrate_parsetree_402_403_migrate

(*$ open Printf
    let fields = [
      "attribute"; "attributes"; "case"; "cases"; "class_declaration";
      "class_description"; "class_expr"; "class_field"; "class_signature";
      "class_structure"; "class_type"; "class_type_declaration";
      "class_type_field"; "constructor_declaration"; "expr"; "extension";
      "extension_constructor"; "include_declaration"; "include_description";
      "label_declaration"; "location"; "module_binding"; "module_declaration";
      "module_expr"; "module_type"; "module_type_declaration";
      "open_description"; "pat"; "signature"; "signature_item"; "structure";
      "structure_item"; "typ"; "type_declaration"; "type_extension";
      "type_kind"; "value_binding"; "value_description";
      "with_constraint"
    ]
  let foreach_field f =
    printf "\n";
    List.iter f fields
*)(*$*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$ foreach_field (printf "%s;\n")*)
     attribute;
     attributes;
     case;
     cases;
     class_declaration;
     class_description;
     class_expr;
     class_field;
     class_signature;
     class_structure;
     class_type;
     class_type_declaration;
     class_type_field;
     constructor_declaration;
     expr;
     extension;
     extension_constructor;
     include_declaration;
     include_description;
     label_declaration;
     location;
     module_binding;
     module_declaration;
     module_expr;
     module_type;
     module_type_declaration;
     open_description;
     pat;
     signature;
     signature_item;
     structure;
     structure_item;
     typ;
     type_declaration;
     type_extension;
     type_kind;
     value_binding;
     value_description;
     with_constraint;
     (*$*)
     payload
   } as mapper) ->
  let module R = Migrate_parsetree_403_402_migrate in
  {
    To.Ast_mapper.
    (*$ foreach_field (fun s ->
          printf
          "%s = (fun _ x -> copy_%s (%s mapper (R.copy_%s x)));\n" s s s s)
    *)
    attribute = (fun _ x -> copy_attribute (attribute mapper (R.copy_attribute x)));
    attributes = (fun _ x -> copy_attributes (attributes mapper (R.copy_attributes x)));
    case = (fun _ x -> copy_case (case mapper (R.copy_case x)));
    cases = (fun _ x -> copy_cases (cases mapper (R.copy_cases x)));
    class_declaration = (fun _ x -> copy_class_declaration (class_declaration mapper (R.copy_class_declaration x)));
    class_description = (fun _ x -> copy_class_description (class_description mapper (R.copy_class_description x)));
    class_expr = (fun _ x -> copy_class_expr (class_expr mapper (R.copy_class_expr x)));
    class_field = (fun _ x -> copy_class_field (class_field mapper (R.copy_class_field x)));
    class_signature = (fun _ x -> copy_class_signature (class_signature mapper (R.copy_class_signature x)));
    class_structure = (fun _ x -> copy_class_structure (class_structure mapper (R.copy_class_structure x)));
    class_type = (fun _ x -> copy_class_type (class_type mapper (R.copy_class_type x)));
    class_type_declaration = (fun _ x -> copy_class_type_declaration (class_type_declaration mapper (R.copy_class_type_declaration x)));
    class_type_field = (fun _ x -> copy_class_type_field (class_type_field mapper (R.copy_class_type_field x)));
    constructor_declaration = (fun _ x -> copy_constructor_declaration (constructor_declaration mapper (R.copy_constructor_declaration x)));
    expr = (fun _ x -> copy_expr (expr mapper (R.copy_expr x)));
    extension = (fun _ x -> copy_extension (extension mapper (R.copy_extension x)));
    extension_constructor = (fun _ x -> copy_extension_constructor (extension_constructor mapper (R.copy_extension_constructor x)));
    include_declaration = (fun _ x -> copy_include_declaration (include_declaration mapper (R.copy_include_declaration x)));
    include_description = (fun _ x -> copy_include_description (include_description mapper (R.copy_include_description x)));
    label_declaration = (fun _ x -> copy_label_declaration (label_declaration mapper (R.copy_label_declaration x)));
    location = (fun _ x -> copy_location (location mapper (R.copy_location x)));
    module_binding = (fun _ x -> copy_module_binding (module_binding mapper (R.copy_module_binding x)));
    module_declaration = (fun _ x -> copy_module_declaration (module_declaration mapper (R.copy_module_declaration x)));
    module_expr = (fun _ x -> copy_module_expr (module_expr mapper (R.copy_module_expr x)));
    module_type = (fun _ x -> copy_module_type (module_type mapper (R.copy_module_type x)));
    module_type_declaration = (fun _ x -> copy_module_type_declaration (module_type_declaration mapper (R.copy_module_type_declaration x)));
    open_description = (fun _ x -> copy_open_description (open_description mapper (R.copy_open_description x)));
    pat = (fun _ x -> copy_pat (pat mapper (R.copy_pat x)));
    signature = (fun _ x -> copy_signature (signature mapper (R.copy_signature x)));
    signature_item = (fun _ x -> copy_signature_item (signature_item mapper (R.copy_signature_item x)));
    structure = (fun _ x -> copy_structure (structure mapper (R.copy_structure x)));
    structure_item = (fun _ x -> copy_structure_item (structure_item mapper (R.copy_structure_item x)));
    typ = (fun _ x -> copy_typ (typ mapper (R.copy_typ x)));
    type_declaration = (fun _ x -> copy_type_declaration (type_declaration mapper (R.copy_type_declaration x)));
    type_extension = (fun _ x -> copy_type_extension (type_extension mapper (R.copy_type_extension x)));
    type_kind = (fun _ x -> copy_type_kind (type_kind mapper (R.copy_type_kind x)));
    value_binding = (fun _ x -> copy_value_binding (value_binding mapper (R.copy_value_binding x)));
    value_description = (fun _ x -> copy_value_description (value_description mapper (R.copy_value_description x)));
    with_constraint = (fun _ x -> copy_with_constraint (with_constraint mapper (R.copy_with_constraint x)));
    (*$*)
    payload = (fun _ x -> copy_payload (payload mapper (R.copy_payload Location.none x)))
  }

end
module Migrate_parsetree_403_402
= struct
#1 "migrate_parsetree_403_402.ml"
# 1 "src/migrate_parsetree_403_402.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Migrate_parsetree_403_402_migrate

(*$ open Printf
    let fields = [
      "attribute"; "attributes"; "case"; "cases"; "class_declaration";
      "class_description"; "class_expr"; "class_field"; "class_signature";
      "class_structure"; "class_type"; "class_type_declaration";
      "class_type_field"; "constructor_declaration"; "expr"; "extension";
      "extension_constructor"; "include_declaration"; "include_description";
      "label_declaration"; "location"; "module_binding"; "module_declaration";
      "module_expr"; "module_type"; "module_type_declaration";
      "open_description"; "pat"; "signature"; "signature_item"; "structure";
      "structure_item"; "typ"; "type_declaration"; "type_extension";
      "type_kind"; "value_binding"; "value_description";
      "with_constraint"
    ]
  let foreach_field f =
    printf "\n";
    List.iter f fields
*)(*$*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$ foreach_field (printf "%s;\n")*)
     attribute;
     attributes;
     case;
     cases;
     class_declaration;
     class_description;
     class_expr;
     class_field;
     class_signature;
     class_structure;
     class_type;
     class_type_declaration;
     class_type_field;
     constructor_declaration;
     expr;
     extension;
     extension_constructor;
     include_declaration;
     include_description;
     label_declaration;
     location;
     module_binding;
     module_declaration;
     module_expr;
     module_type;
     module_type_declaration;
     open_description;
     pat;
     signature;
     signature_item;
     structure;
     structure_item;
     typ;
     type_declaration;
     type_extension;
     type_kind;
     value_binding;
     value_description;
     with_constraint;
     (*$*)
     payload
   } as mapper) ->
  let module R = Migrate_parsetree_402_403_migrate in
  {
    To.Ast_mapper.
    (*$ foreach_field (fun s ->
          printf
          "%s = (fun _ x -> copy_%s (%s mapper (R.copy_%s x)));\n" s s s s)
    *)
    attribute = (fun _ x -> copy_attribute (attribute mapper (R.copy_attribute x)));
    attributes = (fun _ x -> copy_attributes (attributes mapper (R.copy_attributes x)));
    case = (fun _ x -> copy_case (case mapper (R.copy_case x)));
    cases = (fun _ x -> copy_cases (cases mapper (R.copy_cases x)));
    class_declaration = (fun _ x -> copy_class_declaration (class_declaration mapper (R.copy_class_declaration x)));
    class_description = (fun _ x -> copy_class_description (class_description mapper (R.copy_class_description x)));
    class_expr = (fun _ x -> copy_class_expr (class_expr mapper (R.copy_class_expr x)));
    class_field = (fun _ x -> copy_class_field (class_field mapper (R.copy_class_field x)));
    class_signature = (fun _ x -> copy_class_signature (class_signature mapper (R.copy_class_signature x)));
    class_structure = (fun _ x -> copy_class_structure (class_structure mapper (R.copy_class_structure x)));
    class_type = (fun _ x -> copy_class_type (class_type mapper (R.copy_class_type x)));
    class_type_declaration = (fun _ x -> copy_class_type_declaration (class_type_declaration mapper (R.copy_class_type_declaration x)));
    class_type_field = (fun _ x -> copy_class_type_field (class_type_field mapper (R.copy_class_type_field x)));
    constructor_declaration = (fun _ x -> copy_constructor_declaration (constructor_declaration mapper (R.copy_constructor_declaration x)));
    expr = (fun _ x -> copy_expr (expr mapper (R.copy_expr x)));
    extension = (fun _ x -> copy_extension (extension mapper (R.copy_extension x)));
    extension_constructor = (fun _ x -> copy_extension_constructor (extension_constructor mapper (R.copy_extension_constructor x)));
    include_declaration = (fun _ x -> copy_include_declaration (include_declaration mapper (R.copy_include_declaration x)));
    include_description = (fun _ x -> copy_include_description (include_description mapper (R.copy_include_description x)));
    label_declaration = (fun _ x -> copy_label_declaration (label_declaration mapper (R.copy_label_declaration x)));
    location = (fun _ x -> copy_location (location mapper (R.copy_location x)));
    module_binding = (fun _ x -> copy_module_binding (module_binding mapper (R.copy_module_binding x)));
    module_declaration = (fun _ x -> copy_module_declaration (module_declaration mapper (R.copy_module_declaration x)));
    module_expr = (fun _ x -> copy_module_expr (module_expr mapper (R.copy_module_expr x)));
    module_type = (fun _ x -> copy_module_type (module_type mapper (R.copy_module_type x)));
    module_type_declaration = (fun _ x -> copy_module_type_declaration (module_type_declaration mapper (R.copy_module_type_declaration x)));
    open_description = (fun _ x -> copy_open_description (open_description mapper (R.copy_open_description x)));
    pat = (fun _ x -> copy_pat (pat mapper (R.copy_pat x)));
    signature = (fun _ x -> copy_signature (signature mapper (R.copy_signature x)));
    signature_item = (fun _ x -> copy_signature_item (signature_item mapper (R.copy_signature_item x)));
    structure = (fun _ x -> copy_structure (structure mapper (R.copy_structure x)));
    structure_item = (fun _ x -> copy_structure_item (structure_item mapper (R.copy_structure_item x)));
    typ = (fun _ x -> copy_typ (typ mapper (R.copy_typ x)));
    type_declaration = (fun _ x -> copy_type_declaration (type_declaration mapper (R.copy_type_declaration x)));
    type_extension = (fun _ x -> copy_type_extension (type_extension mapper (R.copy_type_extension x)));
    type_kind = (fun _ x -> copy_type_kind (type_kind mapper (R.copy_type_kind x)));
    value_binding = (fun _ x -> copy_value_binding (value_binding mapper (R.copy_value_binding x)));
    value_description = (fun _ x -> copy_value_description (value_description mapper (R.copy_value_description x)));
    with_constraint = (fun _ x -> copy_with_constraint (with_constraint mapper (R.copy_with_constraint x)));
    (*$*)
    payload = (fun _ x -> copy_payload Location.none (payload mapper (R.copy_payload x)))
  }

end
module Migrate_parsetree_403_404_migrate
= struct
#1 "migrate_parsetree_403_404_migrate.ml"
# 1 "src/migrate_parsetree_403_404_migrate.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module From = Ast_403
module To = Ast_404

let rec copy_expression :
  From.Parsetree.expression ->
    To.Parsetree.expression
  =
  fun
    { From.Parsetree.pexp_desc = pexp_desc;
      From.Parsetree.pexp_loc = pexp_loc;
      From.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      To.Parsetree.pexp_desc =
        (copy_expression_desc pexp_desc);
      To.Parsetree.pexp_loc =
        (copy_location pexp_loc);
      To.Parsetree.pexp_attributes =
        (copy_attributes pexp_attributes)
    }

and copy_expression_desc :
  From.Parsetree.expression_desc ->
    To.Parsetree.expression_desc
  =
  function
  | From.Parsetree.Pexp_ident x0 ->
      To.Parsetree.Pexp_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_constant x0 ->
      To.Parsetree.Pexp_constant
        (copy_constant x0)
  | From.Parsetree.Pexp_let (x0,x1,x2) ->
      To.Parsetree.Pexp_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_function x0 ->
      To.Parsetree.Pexp_function
        (List.map copy_case x0)
  | From.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pexp_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_expression x3))
  | From.Parsetree.Pexp_apply (x0,x1) ->
      To.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pexp_match (x0,x1) ->
      To.Parsetree.Pexp_match
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_try (x0,x1) ->
      To.Parsetree.Pexp_try
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_tuple x0 ->
      To.Parsetree.Pexp_tuple
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_construct (x0,x1) ->
      To.Parsetree.Pexp_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_variant (x0,x1) ->
      To.Parsetree.Pexp_variant
        ((copy_label x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_record (x0,x1) ->
      To.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_expression x1))) x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_field (x0,x1) ->
      To.Parsetree.Pexp_field
        ((copy_expression x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pexp_setfield (x0,x1,x2) ->
      To.Parsetree.Pexp_setfield
        ((copy_expression x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_array x0 ->
      To.Parsetree.Pexp_array
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      To.Parsetree.Pexp_ifthenelse
        ((copy_expression x0),
          (copy_expression x1),
          (copy_option copy_expression x2))
  | From.Parsetree.Pexp_sequence (x0,x1) ->
      To.Parsetree.Pexp_sequence
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_while (x0,x1) ->
      To.Parsetree.Pexp_while
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      To.Parsetree.Pexp_for
        ((copy_pattern x0),
          (copy_expression x1),
          (copy_expression x2),
          (copy_direction_flag x3),
          (copy_expression x4))
  | From.Parsetree.Pexp_constraint (x0,x1) ->
      To.Parsetree.Pexp_constraint
        ((copy_expression x0),
          (copy_core_type x1))
  | From.Parsetree.Pexp_coerce (x0,x1,x2) ->
      To.Parsetree.Pexp_coerce
        ((copy_expression x0),
          (copy_option copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Pexp_send (x0,x1) ->
      To.Parsetree.Pexp_send
        ((copy_expression x0), x1)
  | From.Parsetree.Pexp_new x0 ->
      To.Parsetree.Pexp_new
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_setinstvar (x0,x1) ->
      To.Parsetree.Pexp_setinstvar
        ((copy_loc (fun x  -> x) x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_override x0 ->
      To.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_loc (fun x  -> x) x0),
                (copy_expression x1))) x0)
  | From.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      To.Parsetree.Pexp_letmodule
        ((copy_loc (fun x  -> x) x0),
          (copy_module_expr x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_assert x0 ->
      To.Parsetree.Pexp_assert
        (copy_expression x0)
  | From.Parsetree.Pexp_lazy x0 ->
      To.Parsetree.Pexp_lazy
        (copy_expression x0)
  | From.Parsetree.Pexp_poly (x0,x1) ->
      To.Parsetree.Pexp_poly
        ((copy_expression x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pexp_object x0 ->
      To.Parsetree.Pexp_object
        (copy_class_structure x0)
  | From.Parsetree.Pexp_newtype (x0,x1) ->
      To.Parsetree.Pexp_newtype
        (x0, (copy_expression x1))
  | From.Parsetree.Pexp_pack x0 ->
      To.Parsetree.Pexp_pack
        (copy_module_expr x0)
  | From.Parsetree.Pexp_open (x0,x1,x2) ->
      To.Parsetree.Pexp_open
        ((copy_override_flag x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_extension x0 ->
      To.Parsetree.Pexp_extension
        (copy_extension x0)
  | From.Parsetree.Pexp_unreachable  ->
      To.Parsetree.Pexp_unreachable

and copy_direction_flag :
  From.Asttypes.direction_flag ->
    To.Asttypes.direction_flag
  =
  function
  | From.Asttypes.Upto  -> To.Asttypes.Upto
  | From.Asttypes.Downto  -> To.Asttypes.Downto

and copy_case :
  From.Parsetree.case -> To.Parsetree.case =
  fun
    { From.Parsetree.pc_lhs = pc_lhs;
      From.Parsetree.pc_guard = pc_guard;
      From.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      To.Parsetree.pc_lhs =
        (copy_pattern pc_lhs);
      To.Parsetree.pc_guard =
        (copy_option copy_expression pc_guard);
      To.Parsetree.pc_rhs =
        (copy_expression pc_rhs)
    }

and copy_value_binding :
  From.Parsetree.value_binding ->
    To.Parsetree.value_binding
  =
  fun
    { From.Parsetree.pvb_pat = pvb_pat;
      From.Parsetree.pvb_expr = pvb_expr;
      From.Parsetree.pvb_attributes = pvb_attributes;
      From.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      To.Parsetree.pvb_pat =
        (copy_pattern pvb_pat);
      To.Parsetree.pvb_expr =
        (copy_expression pvb_expr);
      To.Parsetree.pvb_attributes =
        (copy_attributes pvb_attributes);
      To.Parsetree.pvb_loc =
        (copy_location pvb_loc)
    }

and copy_pattern :
  From.Parsetree.pattern -> To.Parsetree.pattern =
  fun
    { From.Parsetree.ppat_desc = ppat_desc;
      From.Parsetree.ppat_loc = ppat_loc;
      From.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      To.Parsetree.ppat_desc =
        (copy_pattern_desc ppat_desc);
      To.Parsetree.ppat_loc =
        (copy_location ppat_loc);
      To.Parsetree.ppat_attributes =
        (copy_attributes ppat_attributes)
    }

and copy_pattern_desc :
  From.Parsetree.pattern_desc ->
    To.Parsetree.pattern_desc
  =
  function
  | From.Parsetree.Ppat_any  ->
      To.Parsetree.Ppat_any
  | From.Parsetree.Ppat_var x0 ->
      To.Parsetree.Ppat_var
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_alias (x0,x1) ->
      To.Parsetree.Ppat_alias
        ((copy_pattern x0),
          (copy_loc (fun x  -> x) x1))
  | From.Parsetree.Ppat_constant x0 ->
      To.Parsetree.Ppat_constant
        (copy_constant x0)
  | From.Parsetree.Ppat_interval (x0,x1) ->
      To.Parsetree.Ppat_interval
        ((copy_constant x0),
          (copy_constant x1))
  | From.Parsetree.Ppat_tuple x0 ->
      To.Parsetree.Ppat_tuple
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_construct (x0,x1) ->
      To.Parsetree.Ppat_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_variant (x0,x1) ->
      To.Parsetree.Ppat_variant
        ((copy_label x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_record (x0,x1) ->
      To.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ppat_array x0 ->
      To.Parsetree.Ppat_array
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_or (x0,x1) ->
      To.Parsetree.Ppat_or
        ((copy_pattern x0),
          (copy_pattern x1))
  | From.Parsetree.Ppat_constraint (x0,x1) ->
      To.Parsetree.Ppat_constraint
        ((copy_pattern x0),
          (copy_core_type x1))
  | From.Parsetree.Ppat_type x0 ->
      To.Parsetree.Ppat_type
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Ppat_lazy x0 ->
      To.Parsetree.Ppat_lazy
        (copy_pattern x0)
  | From.Parsetree.Ppat_unpack x0 ->
      To.Parsetree.Ppat_unpack
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_exception x0 ->
      To.Parsetree.Ppat_exception
        (copy_pattern x0)
  | From.Parsetree.Ppat_extension x0 ->
      To.Parsetree.Ppat_extension
        (copy_extension x0)

and copy_core_type :
  From.Parsetree.core_type ->
    To.Parsetree.core_type
  =
  fun
    { From.Parsetree.ptyp_desc = ptyp_desc;
      From.Parsetree.ptyp_loc = ptyp_loc;
      From.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      To.Parsetree.ptyp_desc =
        (copy_core_type_desc ptyp_desc);
      To.Parsetree.ptyp_loc =
        (copy_location ptyp_loc);
      To.Parsetree.ptyp_attributes =
        (copy_attributes ptyp_attributes)
    }

and copy_core_type_desc :
  From.Parsetree.core_type_desc ->
    To.Parsetree.core_type_desc
  =
  function
  | From.Parsetree.Ptyp_any  ->
      To.Parsetree.Ptyp_any
  | From.Parsetree.Ptyp_var x0 ->
      To.Parsetree.Ptyp_var x0
  | From.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      To.Parsetree.Ptyp_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Ptyp_tuple x0 ->
      To.Parsetree.Ptyp_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Ptyp_constr (x0,x1) ->
      To.Parsetree.Ptyp_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_object (x0,x1) ->
      To.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (x0, (copy_attributes x1),
                 (copy_core_type x2))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ptyp_class (x0,x1) ->
      To.Parsetree.Ptyp_class
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_alias (x0,x1) ->
      To.Parsetree.Ptyp_alias
        ((copy_core_type x0), x1)
  | From.Parsetree.Ptyp_variant (x0,x1,x2) ->
      To.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0),
          (copy_closed_flag x1),
          (copy_option
             (fun x  -> List.map copy_label x) x2))
  | From.Parsetree.Ptyp_poly (x0,x1) ->
      To.Parsetree.Ptyp_poly
        ((List.map (fun x  -> x) x0),
          (copy_core_type x1))
  | From.Parsetree.Ptyp_package x0 ->
      To.Parsetree.Ptyp_package
        (copy_package_type x0)
  | From.Parsetree.Ptyp_extension x0 ->
      To.Parsetree.Ptyp_extension
        (copy_extension x0)

and copy_package_type :
  From.Parsetree.package_type ->
    To.Parsetree.package_type
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc copy_longident x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_loc
                copy_longident x0),
              (copy_core_type x1))) x1))

and copy_row_field :
  From.Parsetree.row_field ->
    To.Parsetree.row_field
  =
  function
  | From.Parsetree.Rtag (x0,x1,x2,x3) ->
      To.Parsetree.Rtag
        ((copy_label x0),
          (copy_attributes x1), (copy_bool x2),
          (List.map copy_core_type x3))
  | From.Parsetree.Rinherit x0 ->
      To.Parsetree.Rinherit
        (copy_core_type x0)

and copy_attributes :
  From.Parsetree.attributes ->
    To.Parsetree.attributes
  = fun x  -> List.map copy_attribute x

and copy_attribute :
  From.Parsetree.attribute ->
    To.Parsetree.attribute
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_payload :
  From.Parsetree.payload -> To.Parsetree.payload =
  function
  | From.Parsetree.PStr x0 ->
      To.Parsetree.PStr
        (copy_structure x0)
  | From.Parsetree.PSig x0 ->
      To.Parsetree.PSig
        (copy_signature x0)
  | From.Parsetree.PTyp x0 ->
      To.Parsetree.PTyp
        (copy_core_type x0)
  | From.Parsetree.PPat (x0,x1) ->
      To.Parsetree.PPat
        ((copy_pattern x0),
          (copy_option copy_expression x1))

and copy_structure :
  From.Parsetree.structure ->
    To.Parsetree.structure
  = fun x  -> List.map copy_structure_item x

and copy_structure_item :
  From.Parsetree.structure_item ->
    To.Parsetree.structure_item
  =
  fun
    { From.Parsetree.pstr_desc = pstr_desc;
      From.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      To.Parsetree.pstr_desc =
        (copy_structure_item_desc pstr_desc);
      To.Parsetree.pstr_loc =
        (copy_location pstr_loc)
    }

and copy_structure_item_desc :
  From.Parsetree.structure_item_desc ->
    To.Parsetree.structure_item_desc
  =
  function
  | From.Parsetree.Pstr_eval (x0,x1) ->
      To.Parsetree.Pstr_eval
        ((copy_expression x0),
          (copy_attributes x1))
  | From.Parsetree.Pstr_value (x0,x1) ->
      To.Parsetree.Pstr_value
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1))
  | From.Parsetree.Pstr_primitive x0 ->
      To.Parsetree.Pstr_primitive
        (copy_value_description x0)
  | From.Parsetree.Pstr_type (x0,x1) ->
      To.Parsetree.Pstr_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Pstr_typext x0 ->
      To.Parsetree.Pstr_typext
        (copy_type_extension x0)
  | From.Parsetree.Pstr_exception x0 ->
      To.Parsetree.Pstr_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Pstr_module x0 ->
      To.Parsetree.Pstr_module
        (copy_module_binding x0)
  | From.Parsetree.Pstr_recmodule x0 ->
      To.Parsetree.Pstr_recmodule
        (List.map copy_module_binding x0)
  | From.Parsetree.Pstr_modtype x0 ->
      To.Parsetree.Pstr_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Pstr_open x0 ->
      To.Parsetree.Pstr_open
        (copy_open_description x0)
  | From.Parsetree.Pstr_class x0 ->
      To.Parsetree.Pstr_class
        (List.map copy_class_declaration x0)
  | From.Parsetree.Pstr_class_type x0 ->
      To.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Pstr_include x0 ->
      To.Parsetree.Pstr_include
        (copy_include_declaration x0)
  | From.Parsetree.Pstr_attribute x0 ->
      To.Parsetree.Pstr_attribute
        (copy_attribute x0)
  | From.Parsetree.Pstr_extension (x0,x1) ->
      To.Parsetree.Pstr_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_include_declaration :
  From.Parsetree.include_declaration ->
    To.Parsetree.include_declaration
  =
  fun x  ->
    copy_include_infos
      copy_module_expr x

and copy_class_declaration :
  From.Parsetree.class_declaration ->
    To.Parsetree.class_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_expr x

and copy_class_expr :
  From.Parsetree.class_expr ->
    To.Parsetree.class_expr
  =
  fun
    { From.Parsetree.pcl_desc = pcl_desc;
      From.Parsetree.pcl_loc = pcl_loc;
      From.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      To.Parsetree.pcl_desc =
        (copy_class_expr_desc pcl_desc);
      To.Parsetree.pcl_loc =
        (copy_location pcl_loc);
      To.Parsetree.pcl_attributes =
        (copy_attributes pcl_attributes)
    }

and copy_class_expr_desc :
  From.Parsetree.class_expr_desc ->
    To.Parsetree.class_expr_desc
  =
  function
  | From.Parsetree.Pcl_constr (x0,x1) ->
      To.Parsetree.Pcl_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcl_structure x0 ->
      To.Parsetree.Pcl_structure
        (copy_class_structure x0)
  | From.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pcl_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_class_expr x3))
  | From.Parsetree.Pcl_apply (x0,x1) ->
      To.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pcl_let (x0,x1,x2) ->
      To.Parsetree.Pcl_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | From.Parsetree.Pcl_constraint (x0,x1) ->
      To.Parsetree.Pcl_constraint
        ((copy_class_expr x0),
          (copy_class_type x1))
  | From.Parsetree.Pcl_extension x0 ->
      To.Parsetree.Pcl_extension
        (copy_extension x0)

and copy_class_structure :
  From.Parsetree.class_structure ->
    To.Parsetree.class_structure
  =
  fun
    { From.Parsetree.pcstr_self = pcstr_self;
      From.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      To.Parsetree.pcstr_self =
        (copy_pattern pcstr_self);
      To.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }

and copy_class_field :
  From.Parsetree.class_field ->
    To.Parsetree.class_field
  =
  fun
    { From.Parsetree.pcf_desc = pcf_desc;
      From.Parsetree.pcf_loc = pcf_loc;
      From.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      To.Parsetree.pcf_desc =
        (copy_class_field_desc pcf_desc);
      To.Parsetree.pcf_loc =
        (copy_location pcf_loc);
      To.Parsetree.pcf_attributes =
        (copy_attributes pcf_attributes)
    }

and copy_class_field_desc :
  From.Parsetree.class_field_desc ->
    To.Parsetree.class_field_desc
  =
  function
  | From.Parsetree.Pcf_inherit (x0,x1,x2) ->
      To.Parsetree.Pcf_inherit
        ((copy_override_flag x0),
          (copy_class_expr x1),
          (copy_option (fun x  -> x) x2))
  | From.Parsetree.Pcf_val x0 ->
      To.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_method x0 ->
      To.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_constraint x0 ->
      To.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pcf_initializer x0 ->
      To.Parsetree.Pcf_initializer
        (copy_expression x0)
  | From.Parsetree.Pcf_attribute x0 ->
      To.Parsetree.Pcf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pcf_extension x0 ->
      To.Parsetree.Pcf_extension
        (copy_extension x0)

and copy_class_field_kind :
  From.Parsetree.class_field_kind ->
    To.Parsetree.class_field_kind
  =
  function
  | From.Parsetree.Cfk_virtual x0 ->
      To.Parsetree.Cfk_virtual
        (copy_core_type x0)
  | From.Parsetree.Cfk_concrete (x0,x1) ->
      To.Parsetree.Cfk_concrete
        ((copy_override_flag x0),
          (copy_expression x1))

and copy_module_binding :
  From.Parsetree.module_binding ->
    To.Parsetree.module_binding
  =
  fun
    { From.Parsetree.pmb_name = pmb_name;
      From.Parsetree.pmb_expr = pmb_expr;
      From.Parsetree.pmb_attributes = pmb_attributes;
      From.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      To.Parsetree.pmb_name =
        (copy_loc (fun x  -> x) pmb_name);
      To.Parsetree.pmb_expr =
        (copy_module_expr pmb_expr);
      To.Parsetree.pmb_attributes =
        (copy_attributes pmb_attributes);
      To.Parsetree.pmb_loc =
        (copy_location pmb_loc)
    }

and copy_module_expr :
  From.Parsetree.module_expr ->
    To.Parsetree.module_expr
  =
  fun
    { From.Parsetree.pmod_desc = pmod_desc;
      From.Parsetree.pmod_loc = pmod_loc;
      From.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      To.Parsetree.pmod_desc =
        (copy_module_expr_desc pmod_desc);
      To.Parsetree.pmod_loc =
        (copy_location pmod_loc);
      To.Parsetree.pmod_attributes =
        (copy_attributes pmod_attributes)
    }

and copy_module_expr_desc :
  From.Parsetree.module_expr_desc ->
    To.Parsetree.module_expr_desc
  =
  function
  | From.Parsetree.Pmod_ident x0 ->
      To.Parsetree.Pmod_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmod_structure x0 ->
      To.Parsetree.Pmod_structure
        (copy_structure x0)
  | From.Parsetree.Pmod_functor (x0,x1,x2) ->
      To.Parsetree.Pmod_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_expr x2))
  | From.Parsetree.Pmod_apply (x0,x1) ->
      To.Parsetree.Pmod_apply
        ((copy_module_expr x0),
          (copy_module_expr x1))
  | From.Parsetree.Pmod_constraint (x0,x1) ->
      To.Parsetree.Pmod_constraint
        ((copy_module_expr x0),
          (copy_module_type x1))
  | From.Parsetree.Pmod_unpack x0 ->
      To.Parsetree.Pmod_unpack
        (copy_expression x0)
  | From.Parsetree.Pmod_extension x0 ->
      To.Parsetree.Pmod_extension
        (copy_extension x0)

and copy_module_type :
  From.Parsetree.module_type ->
    To.Parsetree.module_type
  =
  fun
    { From.Parsetree.pmty_desc = pmty_desc;
      From.Parsetree.pmty_loc = pmty_loc;
      From.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      To.Parsetree.pmty_desc =
        (copy_module_type_desc pmty_desc);
      To.Parsetree.pmty_loc =
        (copy_location pmty_loc);
      To.Parsetree.pmty_attributes =
        (copy_attributes pmty_attributes)
    }

and copy_module_type_desc :
  From.Parsetree.module_type_desc ->
    To.Parsetree.module_type_desc
  =
  function
  | From.Parsetree.Pmty_ident x0 ->
      To.Parsetree.Pmty_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmty_signature x0 ->
      To.Parsetree.Pmty_signature
        (copy_signature x0)
  | From.Parsetree.Pmty_functor (x0,x1,x2) ->
      To.Parsetree.Pmty_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_type x2))
  | From.Parsetree.Pmty_with (x0,x1) ->
      To.Parsetree.Pmty_with
        ((copy_module_type x0),
          (List.map copy_with_constraint x1))
  | From.Parsetree.Pmty_typeof x0 ->
      To.Parsetree.Pmty_typeof
        (copy_module_expr x0)
  | From.Parsetree.Pmty_extension x0 ->
      To.Parsetree.Pmty_extension
        (copy_extension x0)
  | From.Parsetree.Pmty_alias x0 ->
      To.Parsetree.Pmty_alias
        (copy_loc copy_longident
           x0)

and copy_with_constraint :
  From.Parsetree.with_constraint ->
    To.Parsetree.with_constraint
  =
  function
  | From.Parsetree.Pwith_type (x0,x1) ->
      To.Parsetree.Pwith_type
        ((copy_loc
            copy_longident x0),
          (copy_type_declaration x1))
  | From.Parsetree.Pwith_module (x0,x1) ->
      To.Parsetree.Pwith_module
        ((copy_loc
            copy_longident x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pwith_typesubst x0 ->
      To.Parsetree.Pwith_typesubst
        (copy_type_declaration x0)
  | From.Parsetree.Pwith_modsubst (x0,x1) ->
      To.Parsetree.Pwith_modsubst
        ((copy_loc (fun x  -> x) x0),
          (copy_loc
             copy_longident x1))

and copy_signature :
  From.Parsetree.signature ->
    To.Parsetree.signature
  = fun x  -> List.map copy_signature_item x

and copy_signature_item :
  From.Parsetree.signature_item ->
    To.Parsetree.signature_item
  =
  fun
    { From.Parsetree.psig_desc = psig_desc;
      From.Parsetree.psig_loc = psig_loc }
     ->
    {
      To.Parsetree.psig_desc =
        (copy_signature_item_desc psig_desc);
      To.Parsetree.psig_loc =
        (copy_location psig_loc)
    }

and copy_signature_item_desc :
  From.Parsetree.signature_item_desc ->
    To.Parsetree.signature_item_desc
  =
  function
  | From.Parsetree.Psig_value x0 ->
      To.Parsetree.Psig_value
        (copy_value_description x0)
  | From.Parsetree.Psig_type (x0,x1) ->
      To.Parsetree.Psig_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Psig_typext x0 ->
      To.Parsetree.Psig_typext
        (copy_type_extension x0)
  | From.Parsetree.Psig_exception x0 ->
      To.Parsetree.Psig_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Psig_module x0 ->
      To.Parsetree.Psig_module
        (copy_module_declaration x0)
  | From.Parsetree.Psig_recmodule x0 ->
      To.Parsetree.Psig_recmodule
        (List.map copy_module_declaration x0)
  | From.Parsetree.Psig_modtype x0 ->
      To.Parsetree.Psig_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Psig_open x0 ->
      To.Parsetree.Psig_open
        (copy_open_description x0)
  | From.Parsetree.Psig_include x0 ->
      To.Parsetree.Psig_include
        (copy_include_description x0)
  | From.Parsetree.Psig_class x0 ->
      To.Parsetree.Psig_class
        (List.map copy_class_description x0)
  | From.Parsetree.Psig_class_type x0 ->
      To.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Psig_attribute x0 ->
      To.Parsetree.Psig_attribute
        (copy_attribute x0)
  | From.Parsetree.Psig_extension (x0,x1) ->
      To.Parsetree.Psig_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_class_type_declaration :
  From.Parsetree.class_type_declaration ->
    To.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_description :
  From.Parsetree.class_description ->
    To.Parsetree.class_description
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_type :
  From.Parsetree.class_type ->
    To.Parsetree.class_type
  =
  fun
    { From.Parsetree.pcty_desc = pcty_desc;
      From.Parsetree.pcty_loc = pcty_loc;
      From.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      To.Parsetree.pcty_desc =
        (copy_class_type_desc pcty_desc);
      To.Parsetree.pcty_loc =
        (copy_location pcty_loc);
      To.Parsetree.pcty_attributes =
        (copy_attributes pcty_attributes)
    }

and copy_class_type_desc :
  From.Parsetree.class_type_desc ->
    To.Parsetree.class_type_desc
  =
  function
  | From.Parsetree.Pcty_constr (x0,x1) ->
      To.Parsetree.Pcty_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcty_signature x0 ->
      To.Parsetree.Pcty_signature
        (copy_class_signature x0)
  | From.Parsetree.Pcty_arrow (x0,x1,x2) ->
      To.Parsetree.Pcty_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_class_type x2))
  | From.Parsetree.Pcty_extension x0 ->
      To.Parsetree.Pcty_extension
        (copy_extension x0)

and copy_class_signature :
  From.Parsetree.class_signature ->
    To.Parsetree.class_signature
  =
  fun
    { From.Parsetree.pcsig_self = pcsig_self;
      From.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      To.Parsetree.pcsig_self =
        (copy_core_type pcsig_self);
      To.Parsetree.pcsig_fields =
        (List.map copy_class_type_field
           pcsig_fields)
    }

and copy_class_type_field :
  From.Parsetree.class_type_field ->
    To.Parsetree.class_type_field
  =
  fun
    { From.Parsetree.pctf_desc = pctf_desc;
      From.Parsetree.pctf_loc = pctf_loc;
      From.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      To.Parsetree.pctf_desc =
        (copy_class_type_field_desc pctf_desc);
      To.Parsetree.pctf_loc =
        (copy_location pctf_loc);
      To.Parsetree.pctf_attributes =
        (copy_attributes pctf_attributes)
    }

and copy_class_type_field_desc :
  From.Parsetree.class_type_field_desc ->
    To.Parsetree.class_type_field_desc
  =
  function
  | From.Parsetree.Pctf_inherit x0 ->
      To.Parsetree.Pctf_inherit
        (copy_class_type x0)
  | From.Parsetree.Pctf_val x0 ->
      To.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_mutable_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_method x0 ->
      To.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_private_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_constraint x0 ->
      To.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pctf_attribute x0 ->
      To.Parsetree.Pctf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pctf_extension x0 ->
      To.Parsetree.Pctf_extension
        (copy_extension x0)

and copy_extension :
  From.Parsetree.extension ->
    To.Parsetree.extension
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.class_infos ->
        'g0 To.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pci_virt = pci_virt;
        From.Parsetree.pci_params = pci_params;
        From.Parsetree.pci_name = pci_name;
        From.Parsetree.pci_expr = pci_expr;
        From.Parsetree.pci_loc = pci_loc;
        From.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        To.Parsetree.pci_virt =
          (copy_virtual_flag pci_virt);
        To.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_core_type x0),
                  (copy_variance x1))) pci_params);
        To.Parsetree.pci_name =
          (copy_loc (fun x  -> x) pci_name);
        To.Parsetree.pci_expr = (f0 pci_expr);
        To.Parsetree.pci_loc =
          (copy_location pci_loc);
        To.Parsetree.pci_attributes =
          (copy_attributes pci_attributes)
      }

and copy_virtual_flag :
  From.Asttypes.virtual_flag ->
    To.Asttypes.virtual_flag
  =
  function
  | From.Asttypes.Virtual  -> To.Asttypes.Virtual
  | From.Asttypes.Concrete  -> To.Asttypes.Concrete

and copy_include_description :
  From.Parsetree.include_description ->
    To.Parsetree.include_description
  =
  fun x  ->
    copy_include_infos
      copy_module_type x

and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.include_infos ->
        'g0 To.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pincl_mod = pincl_mod;
        From.Parsetree.pincl_loc = pincl_loc;
        From.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        To.Parsetree.pincl_mod = (f0 pincl_mod);
        To.Parsetree.pincl_loc =
          (copy_location pincl_loc);
        To.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }

and copy_open_description :
  From.Parsetree.open_description ->
    To.Parsetree.open_description
  =
  fun
    { From.Parsetree.popen_lid = popen_lid;
      From.Parsetree.popen_override = popen_override;
      From.Parsetree.popen_loc = popen_loc;
      From.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      To.Parsetree.popen_lid =
        (copy_loc copy_longident
           popen_lid);
      To.Parsetree.popen_override =
        (copy_override_flag popen_override);
      To.Parsetree.popen_loc =
        (copy_location popen_loc);
      To.Parsetree.popen_attributes =
        (copy_attributes popen_attributes)
    }

and copy_override_flag :
  From.Asttypes.override_flag ->
    To.Asttypes.override_flag
  =
  function
  | From.Asttypes.Override  -> To.Asttypes.Override
  | From.Asttypes.Fresh  -> To.Asttypes.Fresh

and copy_module_type_declaration :
  From.Parsetree.module_type_declaration ->
    To.Parsetree.module_type_declaration
  =
  fun
    { From.Parsetree.pmtd_name = pmtd_name;
      From.Parsetree.pmtd_type = pmtd_type;
      From.Parsetree.pmtd_attributes = pmtd_attributes;
      From.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      To.Parsetree.pmtd_name =
        (copy_loc (fun x  -> x) pmtd_name);
      To.Parsetree.pmtd_type =
        (copy_option copy_module_type pmtd_type);
      To.Parsetree.pmtd_attributes =
        (copy_attributes pmtd_attributes);
      To.Parsetree.pmtd_loc =
        (copy_location pmtd_loc)
    }

and copy_module_declaration :
  From.Parsetree.module_declaration ->
    To.Parsetree.module_declaration
  =
  fun
    { From.Parsetree.pmd_name = pmd_name;
      From.Parsetree.pmd_type = pmd_type;
      From.Parsetree.pmd_attributes = pmd_attributes;
      From.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      To.Parsetree.pmd_name =
        (copy_loc (fun x  -> x) pmd_name);
      To.Parsetree.pmd_type =
        (copy_module_type pmd_type);
      To.Parsetree.pmd_attributes =
        (copy_attributes pmd_attributes);
      To.Parsetree.pmd_loc =
        (copy_location pmd_loc)
    }

and copy_type_extension :
  From.Parsetree.type_extension ->
    To.Parsetree.type_extension
  =
  fun
    { From.Parsetree.ptyext_path = ptyext_path;
      From.Parsetree.ptyext_params = ptyext_params;
      From.Parsetree.ptyext_constructors = ptyext_constructors;
      From.Parsetree.ptyext_private = ptyext_private;
      From.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      To.Parsetree.ptyext_path =
        (copy_loc copy_longident
           ptyext_path);
      To.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptyext_params);
      To.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor
           ptyext_constructors);
      To.Parsetree.ptyext_private =
        (copy_private_flag ptyext_private);
      To.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }

and copy_extension_constructor :
  From.Parsetree.extension_constructor ->
    To.Parsetree.extension_constructor
  =
  fun
    { From.Parsetree.pext_name = pext_name;
      From.Parsetree.pext_kind = pext_kind;
      From.Parsetree.pext_loc = pext_loc;
      From.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      To.Parsetree.pext_name =
        (copy_loc (fun x  -> x) pext_name);
      To.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      To.Parsetree.pext_loc =
        (copy_location pext_loc);
      To.Parsetree.pext_attributes =
        (copy_attributes pext_attributes)
    }

and copy_extension_constructor_kind :
  From.Parsetree.extension_constructor_kind ->
    To.Parsetree.extension_constructor_kind
  =
  function
  | From.Parsetree.Pext_decl (x0,x1) ->
      To.Parsetree.Pext_decl
        ((copy_constructor_arguments x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pext_rebind x0 ->
      To.Parsetree.Pext_rebind
        (copy_loc copy_longident
           x0)

and copy_type_declaration :
  From.Parsetree.type_declaration ->
    To.Parsetree.type_declaration
  =
  fun
    { From.Parsetree.ptype_name = ptype_name;
      From.Parsetree.ptype_params = ptype_params;
      From.Parsetree.ptype_cstrs = ptype_cstrs;
      From.Parsetree.ptype_kind = ptype_kind;
      From.Parsetree.ptype_private = ptype_private;
      From.Parsetree.ptype_manifest = ptype_manifest;
      From.Parsetree.ptype_attributes = ptype_attributes;
      From.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      To.Parsetree.ptype_name =
        (copy_loc (fun x  -> x) ptype_name);
      To.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptype_params);
      To.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_core_type x0),
                (copy_core_type x1),
                (copy_location x2))) ptype_cstrs);
      To.Parsetree.ptype_kind =
        (copy_type_kind ptype_kind);
      To.Parsetree.ptype_private =
        (copy_private_flag ptype_private);
      To.Parsetree.ptype_manifest =
        (copy_option copy_core_type ptype_manifest);
      To.Parsetree.ptype_attributes =
        (copy_attributes ptype_attributes);
      To.Parsetree.ptype_loc =
        (copy_location ptype_loc)
    }

and copy_private_flag :
  From.Asttypes.private_flag ->
    To.Asttypes.private_flag
  =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_type_kind :
  From.Parsetree.type_kind ->
    To.Parsetree.type_kind
  =
  function
  | From.Parsetree.Ptype_abstract  ->
      To.Parsetree.Ptype_abstract
  | From.Parsetree.Ptype_variant x0 ->
      To.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | From.Parsetree.Ptype_record x0 ->
      To.Parsetree.Ptype_record
        (List.map copy_label_declaration x0)
  | From.Parsetree.Ptype_open  ->
      To.Parsetree.Ptype_open

and copy_constructor_declaration :
  From.Parsetree.constructor_declaration ->
    To.Parsetree.constructor_declaration
  =
  fun
    { From.Parsetree.pcd_name = pcd_name;
      From.Parsetree.pcd_args = pcd_args;
      From.Parsetree.pcd_res = pcd_res;
      From.Parsetree.pcd_loc = pcd_loc;
      From.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      To.Parsetree.pcd_name =
        (copy_loc (fun x  -> x) pcd_name);
      To.Parsetree.pcd_args =
        (copy_constructor_arguments pcd_args);
      To.Parsetree.pcd_res =
        (copy_option copy_core_type pcd_res);
      To.Parsetree.pcd_loc =
        (copy_location pcd_loc);
      To.Parsetree.pcd_attributes =
        (copy_attributes pcd_attributes)
    }

and copy_constructor_arguments :
  From.Parsetree.constructor_arguments ->
    To.Parsetree.constructor_arguments
  =
  function
  | From.Parsetree.Pcstr_tuple x0 ->
      To.Parsetree.Pcstr_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Pcstr_record x0 ->
      To.Parsetree.Pcstr_record
        (List.map copy_label_declaration x0)

and copy_label_declaration :
  From.Parsetree.label_declaration ->
    To.Parsetree.label_declaration
  =
  fun
    { From.Parsetree.pld_name = pld_name;
      From.Parsetree.pld_mutable = pld_mutable;
      From.Parsetree.pld_type = pld_type;
      From.Parsetree.pld_loc = pld_loc;
      From.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      To.Parsetree.pld_name =
        (copy_loc (fun x  -> x) pld_name);
      To.Parsetree.pld_mutable =
        (copy_mutable_flag pld_mutable);
      To.Parsetree.pld_type =
        (copy_core_type pld_type);
      To.Parsetree.pld_loc =
        (copy_location pld_loc);
      To.Parsetree.pld_attributes =
        (copy_attributes pld_attributes)
    }

and copy_mutable_flag :
  From.Asttypes.mutable_flag ->
    To.Asttypes.mutable_flag
  =
  function
  | From.Asttypes.Immutable  ->
      To.Asttypes.Immutable
  | From.Asttypes.Mutable  -> To.Asttypes.Mutable

and copy_variance :
  From.Asttypes.variance -> To.Asttypes.variance =
  function
  | From.Asttypes.Covariant  ->
      To.Asttypes.Covariant
  | From.Asttypes.Contravariant  ->
      To.Asttypes.Contravariant
  | From.Asttypes.Invariant  ->
      To.Asttypes.Invariant

and copy_value_description :
  From.Parsetree.value_description ->
    To.Parsetree.value_description
  =
  fun
    { From.Parsetree.pval_name = pval_name;
      From.Parsetree.pval_type = pval_type;
      From.Parsetree.pval_prim = pval_prim;
      From.Parsetree.pval_attributes = pval_attributes;
      From.Parsetree.pval_loc = pval_loc }
     ->
    {
      To.Parsetree.pval_name =
        (copy_loc (fun x  -> x) pval_name);
      To.Parsetree.pval_type =
        (copy_core_type pval_type);
      To.Parsetree.pval_prim =
        (List.map (fun x  -> x) pval_prim);
      To.Parsetree.pval_attributes =
        (copy_attributes pval_attributes);
      To.Parsetree.pval_loc =
        (copy_location pval_loc)
    }

and copy_arg_label :
  From.Asttypes.arg_label -> To.Asttypes.arg_label
  =
  function
  | From.Asttypes.Nolabel  -> To.Asttypes.Nolabel
  | From.Asttypes.Labelled x0 ->
      To.Asttypes.Labelled x0
  | From.Asttypes.Optional x0 ->
      To.Asttypes.Optional x0

and copy_closed_flag :
  From.Asttypes.closed_flag ->
    To.Asttypes.closed_flag
  =
  function
  | From.Asttypes.Closed  -> To.Asttypes.Closed
  | From.Asttypes.Open  -> To.Asttypes.Open

and copy_label :
  From.Asttypes.label -> To.Asttypes.label =
  fun x  -> x

and copy_rec_flag :
  From.Asttypes.rec_flag -> To.Asttypes.rec_flag =
  function
  | From.Asttypes.Nonrecursive  ->
      To.Asttypes.Nonrecursive
  | From.Asttypes.Recursive  ->
      To.Asttypes.Recursive

and copy_constant :
  From.Parsetree.constant -> To.Parsetree.constant
  =
  function
  | From.Parsetree.Pconst_integer (x0,x1) ->
      To.Parsetree.Pconst_integer
        (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_char x0 ->
      To.Parsetree.Pconst_char x0
  | From.Parsetree.Pconst_string (x0,x1) ->
      To.Parsetree.Pconst_string
        (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_float (x0,x1) ->
      To.Parsetree.Pconst_float
        (x0, (copy_option (fun x  -> x) x1))

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_longident :
  From.Longident.t -> To.Longident.t =
  function
  | From.Longident.Lident x0 ->
      To.Longident.Lident x0
  | From.Longident.Ldot (x0,x1) ->
      To.Longident.Ldot
        ((copy_longident x0), x1)
  | From.Longident.Lapply (x0,x1) ->
      To.Longident.Lapply
        ((copy_longident x0), (copy_longident x1))

and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Asttypes.loc -> 'g0 To.Asttypes.loc
  =
  fun f0  ->
    fun
      { From.Asttypes.txt = txt;
        From.Asttypes.loc = loc }
       ->
      {
        To.Asttypes.txt = (f0 txt);
        To.Asttypes.loc = (copy_location loc)
      }

and copy_location :
  From.Location.t -> To.Location.t =
  fun
    { From.Location.loc_start = loc_start;
      From.Location.loc_end = loc_end;
      From.Location.loc_ghost = loc_ghost }
     ->
    {
      To.Location.loc_start = (copy_Lexing_position loc_start);
      To.Location.loc_end = (copy_Lexing_position loc_end);
      To.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }

let rec copy_out_phrase :
  From.Outcometree.out_phrase -> To.Outcometree.out_phrase =
  function
  | From.Outcometree.Ophr_eval (x0,x1) ->
      To.Outcometree.Ophr_eval
        ((copy_out_value x0),
          (copy_out_type x1))
  | From.Outcometree.Ophr_signature x0 ->
      To.Outcometree.Ophr_signature
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_sig_item x0),
                (copy_option copy_out_value x1))) x0)
  | From.Outcometree.Ophr_exception x0 ->
      To.Outcometree.Ophr_exception
        (let (x0,x1) = x0  in
         ((copy_exn x0), (copy_out_value x1)))

and copy_exn : exn -> exn = fun x  -> x

and copy_out_sig_item :
  From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item =
  function
  | From.Outcometree.Osig_class (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_class_type (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class_type
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_typext (x0,x1) ->
      To.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0),
          (copy_out_ext_status x1))
  | From.Outcometree.Osig_modtype (x0,x1) ->
      To.Outcometree.Osig_modtype
        (x0, (copy_out_module_type x1))
  | From.Outcometree.Osig_module (x0,x1,x2) ->
      To.Outcometree.Osig_module
        (x0, (copy_out_module_type x1),
          (copy_out_rec_status x2))
  | From.Outcometree.Osig_type (x0,x1) ->
      To.Outcometree.Osig_type
        ((copy_out_type_decl x0),
          (copy_out_rec_status x1))
  | From.Outcometree.Osig_value x0 ->
      To.Outcometree.Osig_value
        (copy_out_val_decl x0)
  | From.Outcometree.Osig_ellipsis  -> To.Outcometree.Osig_ellipsis

and copy_out_val_decl :
  From.Outcometree.out_val_decl -> To.Outcometree.out_val_decl =
  fun
    { From.Outcometree.oval_name = oval_name;
      From.Outcometree.oval_type = oval_type;
      From.Outcometree.oval_prims = oval_prims;
      From.Outcometree.oval_attributes = oval_attributes }
     ->
    {
      To.Outcometree.oval_name = oval_name;
      To.Outcometree.oval_type =
        (copy_out_type oval_type);
      To.Outcometree.oval_prims = (List.map (fun x  -> x) oval_prims);
      To.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }

and copy_out_type_decl :
  From.Outcometree.out_type_decl -> To.Outcometree.out_type_decl =
  fun
    { From.Outcometree.otype_name = otype_name;
      From.Outcometree.otype_params = otype_params;
      From.Outcometree.otype_type = otype_type;
      From.Outcometree.otype_private = otype_private;
      From.Outcometree.otype_immediate = otype_immediate;
      From.Outcometree.otype_cstrs = otype_cstrs }
     ->
    {
      To.Outcometree.otype_name = otype_name;
      To.Outcometree.otype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
           otype_params);
      To.Outcometree.otype_type =
        (copy_out_type otype_type);
      To.Outcometree.otype_private =
        (copy_From_Asttypes_private_flag otype_private);
      To.Outcometree.otype_immediate = (copy_bool otype_immediate);
      To.Outcometree.otype_unboxed = false;
      To.Outcometree.otype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_type x0),
                (copy_out_type x1))) otype_cstrs)
    }

and copy_out_module_type :
  From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  =
  function
  | From.Outcometree.Omty_abstract  -> To.Outcometree.Omty_abstract
  | From.Outcometree.Omty_functor (x0,x1,x2) ->
      To.Outcometree.Omty_functor
        (x0, (copy_option copy_out_module_type x1),
          (copy_out_module_type x2))
  | From.Outcometree.Omty_ident x0 ->
      To.Outcometree.Omty_ident (copy_out_ident x0)
  | From.Outcometree.Omty_signature x0 ->
      To.Outcometree.Omty_signature
        (List.map copy_out_sig_item x0)
  | From.Outcometree.Omty_alias x0 ->
      To.Outcometree.Omty_alias (copy_out_ident x0)

and copy_out_ext_status :
  From.Outcometree.out_ext_status -> To.Outcometree.out_ext_status =
  function
  | From.Outcometree.Oext_first  -> To.Outcometree.Oext_first
  | From.Outcometree.Oext_next  -> To.Outcometree.Oext_next
  | From.Outcometree.Oext_exception  -> To.Outcometree.Oext_exception

and copy_out_extension_constructor :
  From.Outcometree.out_extension_constructor ->
    To.Outcometree.out_extension_constructor
  =
  fun
    { From.Outcometree.oext_name = oext_name;
      From.Outcometree.oext_type_name = oext_type_name;
      From.Outcometree.oext_type_params = oext_type_params;
      From.Outcometree.oext_args = oext_args;
      From.Outcometree.oext_ret_type = oext_ret_type;
      From.Outcometree.oext_private = oext_private }
     ->
    {
      To.Outcometree.oext_name = oext_name;
      To.Outcometree.oext_type_name = oext_type_name;
      To.Outcometree.oext_type_params =
        (List.map (fun x  -> x) oext_type_params);
      To.Outcometree.oext_args =
        (List.map copy_out_type oext_args);
      To.Outcometree.oext_ret_type =
        (copy_option copy_out_type oext_ret_type);
      To.Outcometree.oext_private =
        (copy_From_Asttypes_private_flag oext_private)
    }

and copy_From_Asttypes_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_out_rec_status :
  From.Outcometree.out_rec_status -> To.Outcometree.out_rec_status =
  function
  | From.Outcometree.Orec_not  -> To.Outcometree.Orec_not
  | From.Outcometree.Orec_first  -> To.Outcometree.Orec_first
  | From.Outcometree.Orec_next  -> To.Outcometree.Orec_next

and copy_out_class_type :
  From.Outcometree.out_class_type -> To.Outcometree.out_class_type =
  function
  | From.Outcometree.Octy_constr (x0,x1) ->
      To.Outcometree.Octy_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Octy_arrow (x0,x1,x2) ->
      To.Outcometree.Octy_arrow
        (x0, (copy_out_type x1),
          (copy_out_class_type x2))
  | From.Outcometree.Octy_signature (x0,x1) ->
      To.Outcometree.Octy_signature
        ((copy_option copy_out_type x0),
          (List.map copy_out_class_sig_item x1))

and copy_out_class_sig_item :
  From.Outcometree.out_class_sig_item ->
    To.Outcometree.out_class_sig_item
  =
  function
  | From.Outcometree.Ocsg_constraint (x0,x1) ->
      To.Outcometree.Ocsg_constraint
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Ocsg_method (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_method
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))
  | From.Outcometree.Ocsg_value (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_value
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))

and copy_out_type :
  From.Outcometree.out_type -> To.Outcometree.out_type =
  function
  | From.Outcometree.Otyp_abstract  -> To.Outcometree.Otyp_abstract
  | From.Outcometree.Otyp_open  -> To.Outcometree.Otyp_open
  | From.Outcometree.Otyp_alias (x0,x1) ->
      To.Outcometree.Otyp_alias
        ((copy_out_type x0), x1)
  | From.Outcometree.Otyp_arrow (x0,x1,x2) ->
      To.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1),
          (copy_out_type x2))
  | From.Outcometree.Otyp_class (x0,x1,x2) ->
      To.Outcometree.Otyp_class
        ((copy_bool x0), (copy_out_ident x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_constr (x0,x1) ->
      To.Outcometree.Otyp_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Otyp_manifest (x0,x1) ->
      To.Outcometree.Otyp_manifest
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Otyp_object (x0,x1) ->
      To.Outcometree.Otyp_object
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               (x0, (copy_out_type x1))) x0),
          (copy_option copy_bool x1))
  | From.Outcometree.Otyp_record x0 ->
      To.Outcometree.Otyp_record
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1), (copy_out_type x2)))
           x0)
  | From.Outcometree.Otyp_stuff x0 -> To.Outcometree.Otyp_stuff x0
  | From.Outcometree.Otyp_sum x0 ->
      To.Outcometree.Otyp_sum
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2))) x0)
  | From.Outcometree.Otyp_tuple x0 ->
      To.Outcometree.Otyp_tuple
        (List.map copy_out_type x0)
  | From.Outcometree.Otyp_var (x0,x1) ->
      To.Outcometree.Otyp_var ((copy_bool x0), x1)
  | From.Outcometree.Otyp_variant (x0,x1,x2,x3) ->
      To.Outcometree.Otyp_variant
        ((copy_bool x0), (copy_out_variant x1),
          (copy_bool x2),
          (copy_option (fun x  -> List.map (fun x  -> x) x) x3))
  | From.Outcometree.Otyp_poly (x0,x1) ->
      To.Outcometree.Otyp_poly
        ((List.map (fun x  -> x) x0), (copy_out_type x1))
  | From.Outcometree.Otyp_module (x0,x1,x2) ->
      To.Outcometree.Otyp_module
        (x0, (List.map (fun x  -> x) x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_attribute (x0,x1) ->
      To.Outcometree.Otyp_attribute
        ((copy_out_type x0),
          (copy_out_attribute x1))

and copy_out_attribute :
  From.Outcometree.out_attribute -> To.Outcometree.out_attribute =
  fun { From.Outcometree.oattr_name = oattr_name }  ->
    { To.Outcometree.oattr_name = oattr_name }

and copy_out_variant :
  From.Outcometree.out_variant -> To.Outcometree.out_variant =
  function
  | From.Outcometree.Ovar_fields x0 ->
      To.Outcometree.Ovar_fields
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1),
                (List.map copy_out_type x2))) x0)
  | From.Outcometree.Ovar_name (x0,x1) ->
      To.Outcometree.Ovar_name
        ((copy_out_ident x0),
          (List.map copy_out_type x1))

and copy_out_value :
  From.Outcometree.out_value -> To.Outcometree.out_value =
  function
  | From.Outcometree.Oval_array x0 ->
      To.Outcometree.Oval_array
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_char x0 -> To.Outcometree.Oval_char x0
  | From.Outcometree.Oval_constr (x0,x1) ->
      To.Outcometree.Oval_constr
        ((copy_out_ident x0),
          (List.map copy_out_value x1))
  | From.Outcometree.Oval_ellipsis  -> To.Outcometree.Oval_ellipsis
  | From.Outcometree.Oval_float x0 ->
      To.Outcometree.Oval_float (copy_float x0)
  | From.Outcometree.Oval_int x0 -> To.Outcometree.Oval_int x0
  | From.Outcometree.Oval_int32 x0 -> To.Outcometree.Oval_int32 x0
  | From.Outcometree.Oval_int64 x0 -> To.Outcometree.Oval_int64 x0
  | From.Outcometree.Oval_nativeint x0 ->
      To.Outcometree.Oval_nativeint x0
  | From.Outcometree.Oval_list x0 ->
      To.Outcometree.Oval_list
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_printer x0 ->
      To.Outcometree.Oval_printer x0
  | From.Outcometree.Oval_record x0 ->
      To.Outcometree.Oval_record
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_ident x0),
                (copy_out_value x1))) x0)
  | From.Outcometree.Oval_string x0 -> To.Outcometree.Oval_string x0
  | From.Outcometree.Oval_stuff x0 -> To.Outcometree.Oval_stuff x0
  | From.Outcometree.Oval_tuple x0 ->
      To.Outcometree.Oval_tuple
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_variant (x0,x1) ->
      To.Outcometree.Oval_variant
        (x0, (copy_option copy_out_value x1))

and copy_float : float -> float = fun x  -> x

and copy_out_ident :
  From.Outcometree.out_ident -> To.Outcometree.out_ident =
  function
  | From.Outcometree.Oide_apply (x0,x1) ->
      To.Outcometree.Oide_apply ((copy_out_ident x0), (copy_out_ident x1))
  | From.Outcometree.Oide_dot (x0,x1) ->
      To.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | From.Outcometree.Oide_ident x0 -> To.Outcometree.Oide_ident x0

let rec copy_toplevel_phrase :
  From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase =
  function
  | From.Parsetree.Ptop_def x0 ->
      To.Parsetree.Ptop_def (copy_structure x0)
  | From.Parsetree.Ptop_dir (x0,x1) ->
      To.Parsetree.Ptop_dir (x0, copy_directive_argument x1)

and copy_directive_argument :
  From.Parsetree.directive_argument -> To.Parsetree.directive_argument =
  function
  | From.Parsetree.Pdir_none  -> To.Parsetree.Pdir_none
  | From.Parsetree.Pdir_string x0 -> To.Parsetree.Pdir_string x0
  | From.Parsetree.Pdir_int (x0,x1) ->
      To.Parsetree.Pdir_int (x0, copy_option (fun x  -> x) x1)
  | From.Parsetree.Pdir_ident x0 ->
      To.Parsetree.Pdir_ident (copy_longident x0)
  | From.Parsetree.Pdir_bool x0 ->
      To.Parsetree.Pdir_bool (copy_bool x0)

let copy_out_type_extension :
  From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension =
  fun
    { From.Outcometree.otyext_name = otyext_name;
      From.Outcometree.otyext_params = otyext_params;
      From.Outcometree.otyext_constructors = otyext_constructors;
      From.Outcometree.otyext_private = otyext_private }
     ->
    {
      To.Outcometree.otyext_name = otyext_name;
      To.Outcometree.otyext_params =
        (List.map (fun x  -> x) otyext_params);
      To.Outcometree.otyext_constructors =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2)))
           otyext_constructors);
      To.Outcometree.otyext_private =
        (copy_private_flag otyext_private)
    }

let copy_cases x = List.map copy_case x
let copy_pat = copy_pattern
let copy_expr = copy_expression
let copy_typ = copy_core_type

end
module Migrate_parsetree_404_403_migrate
= struct
#1 "migrate_parsetree_404_403_migrate.ml"
# 1 "src/migrate_parsetree_404_403_migrate.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Def = Migrate_parsetree_def
module From = Ast_404
module To = Ast_403

let from_loc {From.Location. txt = _; loc} = loc

let migration_error location feature =
  raise (Def.Migration_error (feature, location))

let rec copy_expression :
  From.Parsetree.expression ->
    To.Parsetree.expression
  =
  fun
    { From.Parsetree.pexp_desc = pexp_desc;
      From.Parsetree.pexp_loc = pexp_loc;
      From.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      To.Parsetree.pexp_desc =
        (copy_expression_desc pexp_loc pexp_desc);
      To.Parsetree.pexp_loc =
        (copy_location pexp_loc);
      To.Parsetree.pexp_attributes =
        (copy_attributes pexp_attributes)
    }

and copy_expression_desc loc :
  From.Parsetree.expression_desc ->
    To.Parsetree.expression_desc
  =
  function
  | From.Parsetree.Pexp_ident x0 ->
      To.Parsetree.Pexp_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_constant x0 ->
      To.Parsetree.Pexp_constant
        (copy_constant x0)
  | From.Parsetree.Pexp_let (x0,x1,x2) ->
      To.Parsetree.Pexp_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_function x0 ->
      To.Parsetree.Pexp_function
        (List.map copy_case x0)
  | From.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pexp_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_expression x3))
  | From.Parsetree.Pexp_apply (x0,x1) ->
      To.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pexp_match (x0,x1) ->
      To.Parsetree.Pexp_match
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_try (x0,x1) ->
      To.Parsetree.Pexp_try
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_tuple x0 ->
      To.Parsetree.Pexp_tuple
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_construct (x0,x1) ->
      To.Parsetree.Pexp_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_variant (x0,x1) ->
      To.Parsetree.Pexp_variant
        ((copy_label x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_record (x0,x1) ->
      To.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_expression x1))) x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_field (x0,x1) ->
      To.Parsetree.Pexp_field
        ((copy_expression x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pexp_setfield (x0,x1,x2) ->
      To.Parsetree.Pexp_setfield
        ((copy_expression x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_array x0 ->
      To.Parsetree.Pexp_array
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      To.Parsetree.Pexp_ifthenelse
        ((copy_expression x0),
          (copy_expression x1),
          (copy_option copy_expression x2))
  | From.Parsetree.Pexp_sequence (x0,x1) ->
      To.Parsetree.Pexp_sequence
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_while (x0,x1) ->
      To.Parsetree.Pexp_while
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      To.Parsetree.Pexp_for
        ((copy_pattern x0),
          (copy_expression x1),
          (copy_expression x2),
          (copy_direction_flag x3),
          (copy_expression x4))
  | From.Parsetree.Pexp_constraint (x0,x1) ->
      To.Parsetree.Pexp_constraint
        ((copy_expression x0),
          (copy_core_type x1))
  | From.Parsetree.Pexp_coerce (x0,x1,x2) ->
      To.Parsetree.Pexp_coerce
        ((copy_expression x0),
          (copy_option copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Pexp_send (x0,x1) ->
      To.Parsetree.Pexp_send
        ((copy_expression x0), x1)
  | From.Parsetree.Pexp_new x0 ->
      To.Parsetree.Pexp_new
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_setinstvar (x0,x1) ->
      To.Parsetree.Pexp_setinstvar
        ((copy_loc (fun x  -> x) x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_override x0 ->
      To.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_loc (fun x  -> x) x0),
                (copy_expression x1))) x0)
  | From.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      To.Parsetree.Pexp_letmodule
        ((copy_loc (fun x  -> x) x0),
          (copy_module_expr x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_letexception _ ->
      migration_error loc Def.Pexp_letexception
  | From.Parsetree.Pexp_assert x0 ->
      To.Parsetree.Pexp_assert
        (copy_expression x0)
  | From.Parsetree.Pexp_lazy x0 ->
      To.Parsetree.Pexp_lazy
        (copy_expression x0)
  | From.Parsetree.Pexp_poly (x0,x1) ->
      To.Parsetree.Pexp_poly
        ((copy_expression x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pexp_object x0 ->
      To.Parsetree.Pexp_object
        (copy_class_structure x0)
  | From.Parsetree.Pexp_newtype (x0,x1) ->
      To.Parsetree.Pexp_newtype
        (x0, (copy_expression x1))
  | From.Parsetree.Pexp_pack x0 ->
      To.Parsetree.Pexp_pack
        (copy_module_expr x0)
  | From.Parsetree.Pexp_open (x0,x1,x2) ->
      To.Parsetree.Pexp_open
        ((copy_override_flag x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_extension x0 ->
      To.Parsetree.Pexp_extension
        (copy_extension x0)
  | From.Parsetree.Pexp_unreachable  ->
      To.Parsetree.Pexp_unreachable

and copy_direction_flag :
  From.Asttypes.direction_flag ->
    To.Asttypes.direction_flag
  =
  function
  | From.Asttypes.Upto  -> To.Asttypes.Upto
  | From.Asttypes.Downto  -> To.Asttypes.Downto

and copy_case :
  From.Parsetree.case -> To.Parsetree.case =
  fun
    { From.Parsetree.pc_lhs = pc_lhs;
      From.Parsetree.pc_guard = pc_guard;
      From.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      To.Parsetree.pc_lhs =
        (copy_pattern pc_lhs);
      To.Parsetree.pc_guard =
        (copy_option copy_expression pc_guard);
      To.Parsetree.pc_rhs =
        (copy_expression pc_rhs)
    }

and copy_value_binding :
  From.Parsetree.value_binding ->
    To.Parsetree.value_binding
  =
  fun
    { From.Parsetree.pvb_pat = pvb_pat;
      From.Parsetree.pvb_expr = pvb_expr;
      From.Parsetree.pvb_attributes = pvb_attributes;
      From.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      To.Parsetree.pvb_pat =
        (copy_pattern pvb_pat);
      To.Parsetree.pvb_expr =
        (copy_expression pvb_expr);
      To.Parsetree.pvb_attributes =
        (copy_attributes pvb_attributes);
      To.Parsetree.pvb_loc =
        (copy_location pvb_loc)
    }

and copy_pattern :
  From.Parsetree.pattern -> To.Parsetree.pattern =
  fun
    { From.Parsetree.ppat_desc = ppat_desc;
      From.Parsetree.ppat_loc = ppat_loc;
      From.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      To.Parsetree.ppat_desc =
        (copy_pattern_desc ppat_loc ppat_desc);
      To.Parsetree.ppat_loc =
        (copy_location ppat_loc);
      To.Parsetree.ppat_attributes =
        (copy_attributes ppat_attributes)
    }

and copy_pattern_desc loc :
  From.Parsetree.pattern_desc ->
    To.Parsetree.pattern_desc
  =
  function
  | From.Parsetree.Ppat_any  ->
      To.Parsetree.Ppat_any
  | From.Parsetree.Ppat_var x0 ->
      To.Parsetree.Ppat_var
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_alias (x0,x1) ->
      To.Parsetree.Ppat_alias
        ((copy_pattern x0),
          (copy_loc (fun x  -> x) x1))
  | From.Parsetree.Ppat_constant x0 ->
      To.Parsetree.Ppat_constant
        (copy_constant x0)
  | From.Parsetree.Ppat_interval (x0,x1) ->
      To.Parsetree.Ppat_interval
        ((copy_constant x0),
          (copy_constant x1))
  | From.Parsetree.Ppat_tuple x0 ->
      To.Parsetree.Ppat_tuple
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_construct (x0,x1) ->
      To.Parsetree.Ppat_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_variant (x0,x1) ->
      To.Parsetree.Ppat_variant
        ((copy_label x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_record (x0,x1) ->
      To.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ppat_array x0 ->
      To.Parsetree.Ppat_array
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_or (x0,x1) ->
      To.Parsetree.Ppat_or
        ((copy_pattern x0),
          (copy_pattern x1))
  | From.Parsetree.Ppat_constraint (x0,x1) ->
      To.Parsetree.Ppat_constraint
        ((copy_pattern x0),
          (copy_core_type x1))
  | From.Parsetree.Ppat_type x0 ->
      To.Parsetree.Ppat_type
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Ppat_lazy x0 ->
      To.Parsetree.Ppat_lazy
        (copy_pattern x0)
  | From.Parsetree.Ppat_unpack x0 ->
      To.Parsetree.Ppat_unpack
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_exception x0 ->
      To.Parsetree.Ppat_exception
        (copy_pattern x0)
  | From.Parsetree.Ppat_extension x0 ->
      To.Parsetree.Ppat_extension
        (copy_extension x0)
  | From.Parsetree.Ppat_open _ ->
      migration_error loc Def.Ppat_open
and copy_core_type :
  From.Parsetree.core_type ->
    To.Parsetree.core_type
  =
  fun
    { From.Parsetree.ptyp_desc = ptyp_desc;
      From.Parsetree.ptyp_loc = ptyp_loc;
      From.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      To.Parsetree.ptyp_desc =
        (copy_core_type_desc ptyp_desc);
      To.Parsetree.ptyp_loc =
        (copy_location ptyp_loc);
      To.Parsetree.ptyp_attributes =
        (copy_attributes ptyp_attributes)
    }

and copy_core_type_desc :
  From.Parsetree.core_type_desc ->
    To.Parsetree.core_type_desc
  =
  function
  | From.Parsetree.Ptyp_any  ->
      To.Parsetree.Ptyp_any
  | From.Parsetree.Ptyp_var x0 ->
      To.Parsetree.Ptyp_var x0
  | From.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      To.Parsetree.Ptyp_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Ptyp_tuple x0 ->
      To.Parsetree.Ptyp_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Ptyp_constr (x0,x1) ->
      To.Parsetree.Ptyp_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_object (x0,x1) ->
      To.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (x0, (copy_attributes x1),
                 (copy_core_type x2))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ptyp_class (x0,x1) ->
      To.Parsetree.Ptyp_class
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_alias (x0,x1) ->
      To.Parsetree.Ptyp_alias
        ((copy_core_type x0), x1)
  | From.Parsetree.Ptyp_variant (x0,x1,x2) ->
      To.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0),
          (copy_closed_flag x1),
          (copy_option
             (fun x  -> List.map copy_label x) x2))
  | From.Parsetree.Ptyp_poly (x0,x1) ->
      To.Parsetree.Ptyp_poly
        ((List.map (fun x  -> x) x0),
          (copy_core_type x1))
  | From.Parsetree.Ptyp_package x0 ->
      To.Parsetree.Ptyp_package
        (copy_package_type x0)
  | From.Parsetree.Ptyp_extension x0 ->
      To.Parsetree.Ptyp_extension
        (copy_extension x0)

and copy_package_type :
  From.Parsetree.package_type ->
    To.Parsetree.package_type
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc copy_longident x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_loc
                copy_longident x0),
              (copy_core_type x1))) x1))

and copy_row_field :
  From.Parsetree.row_field ->
    To.Parsetree.row_field
  =
  function
  | From.Parsetree.Rtag (x0,x1,x2,x3) ->
      To.Parsetree.Rtag
        ((copy_label x0),
          (copy_attributes x1), (copy_bool x2),
          (List.map copy_core_type x3))
  | From.Parsetree.Rinherit x0 ->
      To.Parsetree.Rinherit
        (copy_core_type x0)

and copy_attributes :
  From.Parsetree.attributes ->
    To.Parsetree.attributes
  = fun x  -> List.map copy_attribute x

and copy_attribute :
  From.Parsetree.attribute ->
    To.Parsetree.attribute
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_payload :
  From.Parsetree.payload -> To.Parsetree.payload =
  function
  | From.Parsetree.PStr x0 ->
      To.Parsetree.PStr
        (copy_structure x0)
  | From.Parsetree.PSig x0 ->
      To.Parsetree.PSig
        (copy_signature x0)
  | From.Parsetree.PTyp x0 ->
      To.Parsetree.PTyp
        (copy_core_type x0)
  | From.Parsetree.PPat (x0,x1) ->
      To.Parsetree.PPat
        ((copy_pattern x0),
          (copy_option copy_expression x1))

and copy_structure :
  From.Parsetree.structure ->
    To.Parsetree.structure
  = fun x  -> List.map copy_structure_item x

and copy_structure_item :
  From.Parsetree.structure_item ->
    To.Parsetree.structure_item
  =
  fun
    { From.Parsetree.pstr_desc = pstr_desc;
      From.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      To.Parsetree.pstr_desc =
        (copy_structure_item_desc pstr_desc);
      To.Parsetree.pstr_loc =
        (copy_location pstr_loc)
    }

and copy_structure_item_desc :
  From.Parsetree.structure_item_desc ->
    To.Parsetree.structure_item_desc
  =
  function
  | From.Parsetree.Pstr_eval (x0,x1) ->
      To.Parsetree.Pstr_eval
        ((copy_expression x0),
          (copy_attributes x1))
  | From.Parsetree.Pstr_value (x0,x1) ->
      To.Parsetree.Pstr_value
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1))
  | From.Parsetree.Pstr_primitive x0 ->
      To.Parsetree.Pstr_primitive
        (copy_value_description x0)
  | From.Parsetree.Pstr_type (x0,x1) ->
      To.Parsetree.Pstr_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Pstr_typext x0 ->
      To.Parsetree.Pstr_typext
        (copy_type_extension x0)
  | From.Parsetree.Pstr_exception x0 ->
      To.Parsetree.Pstr_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Pstr_module x0 ->
      To.Parsetree.Pstr_module
        (copy_module_binding x0)
  | From.Parsetree.Pstr_recmodule x0 ->
      To.Parsetree.Pstr_recmodule
        (List.map copy_module_binding x0)
  | From.Parsetree.Pstr_modtype x0 ->
      To.Parsetree.Pstr_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Pstr_open x0 ->
      To.Parsetree.Pstr_open
        (copy_open_description x0)
  | From.Parsetree.Pstr_class x0 ->
      To.Parsetree.Pstr_class
        (List.map copy_class_declaration x0)
  | From.Parsetree.Pstr_class_type x0 ->
      To.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Pstr_include x0 ->
      To.Parsetree.Pstr_include
        (copy_include_declaration x0)
  | From.Parsetree.Pstr_attribute x0 ->
      To.Parsetree.Pstr_attribute
        (copy_attribute x0)
  | From.Parsetree.Pstr_extension (x0,x1) ->
      To.Parsetree.Pstr_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_include_declaration :
  From.Parsetree.include_declaration ->
    To.Parsetree.include_declaration
  =
  fun x  ->
    copy_include_infos
      copy_module_expr x

and copy_class_declaration :
  From.Parsetree.class_declaration ->
    To.Parsetree.class_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_expr x

and copy_class_expr :
  From.Parsetree.class_expr ->
    To.Parsetree.class_expr
  =
  fun
    { From.Parsetree.pcl_desc = pcl_desc;
      From.Parsetree.pcl_loc = pcl_loc;
      From.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      To.Parsetree.pcl_desc =
        (copy_class_expr_desc pcl_desc);
      To.Parsetree.pcl_loc =
        (copy_location pcl_loc);
      To.Parsetree.pcl_attributes =
        (copy_attributes pcl_attributes)
    }

and copy_class_expr_desc :
  From.Parsetree.class_expr_desc ->
    To.Parsetree.class_expr_desc
  =
  function
  | From.Parsetree.Pcl_constr (x0,x1) ->
      To.Parsetree.Pcl_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcl_structure x0 ->
      To.Parsetree.Pcl_structure
        (copy_class_structure x0)
  | From.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pcl_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_class_expr x3))
  | From.Parsetree.Pcl_apply (x0,x1) ->
      To.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pcl_let (x0,x1,x2) ->
      To.Parsetree.Pcl_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | From.Parsetree.Pcl_constraint (x0,x1) ->
      To.Parsetree.Pcl_constraint
        ((copy_class_expr x0),
          (copy_class_type x1))
  | From.Parsetree.Pcl_extension x0 ->
      To.Parsetree.Pcl_extension
        (copy_extension x0)

and copy_class_structure :
  From.Parsetree.class_structure ->
    To.Parsetree.class_structure
  =
  fun
    { From.Parsetree.pcstr_self = pcstr_self;
      From.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      To.Parsetree.pcstr_self =
        (copy_pattern pcstr_self);
      To.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }

and copy_class_field :
  From.Parsetree.class_field ->
    To.Parsetree.class_field
  =
  fun
    { From.Parsetree.pcf_desc = pcf_desc;
      From.Parsetree.pcf_loc = pcf_loc;
      From.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      To.Parsetree.pcf_desc =
        (copy_class_field_desc pcf_desc);
      To.Parsetree.pcf_loc =
        (copy_location pcf_loc);
      To.Parsetree.pcf_attributes =
        (copy_attributes pcf_attributes)
    }

and copy_class_field_desc :
  From.Parsetree.class_field_desc ->
    To.Parsetree.class_field_desc
  =
  function
  | From.Parsetree.Pcf_inherit (x0,x1,x2) ->
      To.Parsetree.Pcf_inherit
        ((copy_override_flag x0),
          (copy_class_expr x1),
          (copy_option (fun x  -> x) x2))
  | From.Parsetree.Pcf_val x0 ->
      To.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_method x0 ->
      To.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_constraint x0 ->
      To.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pcf_initializer x0 ->
      To.Parsetree.Pcf_initializer
        (copy_expression x0)
  | From.Parsetree.Pcf_attribute x0 ->
      To.Parsetree.Pcf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pcf_extension x0 ->
      To.Parsetree.Pcf_extension
        (copy_extension x0)

and copy_class_field_kind :
  From.Parsetree.class_field_kind ->
    To.Parsetree.class_field_kind
  =
  function
  | From.Parsetree.Cfk_virtual x0 ->
      To.Parsetree.Cfk_virtual
        (copy_core_type x0)
  | From.Parsetree.Cfk_concrete (x0,x1) ->
      To.Parsetree.Cfk_concrete
        ((copy_override_flag x0),
          (copy_expression x1))

and copy_module_binding :
  From.Parsetree.module_binding ->
    To.Parsetree.module_binding
  =
  fun
    { From.Parsetree.pmb_name = pmb_name;
      From.Parsetree.pmb_expr = pmb_expr;
      From.Parsetree.pmb_attributes = pmb_attributes;
      From.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      To.Parsetree.pmb_name =
        (copy_loc (fun x  -> x) pmb_name);
      To.Parsetree.pmb_expr =
        (copy_module_expr pmb_expr);
      To.Parsetree.pmb_attributes =
        (copy_attributes pmb_attributes);
      To.Parsetree.pmb_loc =
        (copy_location pmb_loc)
    }

and copy_module_expr :
  From.Parsetree.module_expr ->
    To.Parsetree.module_expr
  =
  fun
    { From.Parsetree.pmod_desc = pmod_desc;
      From.Parsetree.pmod_loc = pmod_loc;
      From.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      To.Parsetree.pmod_desc =
        (copy_module_expr_desc pmod_desc);
      To.Parsetree.pmod_loc =
        (copy_location pmod_loc);
      To.Parsetree.pmod_attributes =
        (copy_attributes pmod_attributes)
    }

and copy_module_expr_desc :
  From.Parsetree.module_expr_desc ->
    To.Parsetree.module_expr_desc
  =
  function
  | From.Parsetree.Pmod_ident x0 ->
      To.Parsetree.Pmod_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmod_structure x0 ->
      To.Parsetree.Pmod_structure
        (copy_structure x0)
  | From.Parsetree.Pmod_functor (x0,x1,x2) ->
      To.Parsetree.Pmod_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_expr x2))
  | From.Parsetree.Pmod_apply (x0,x1) ->
      To.Parsetree.Pmod_apply
        ((copy_module_expr x0),
          (copy_module_expr x1))
  | From.Parsetree.Pmod_constraint (x0,x1) ->
      To.Parsetree.Pmod_constraint
        ((copy_module_expr x0),
          (copy_module_type x1))
  | From.Parsetree.Pmod_unpack x0 ->
      To.Parsetree.Pmod_unpack
        (copy_expression x0)
  | From.Parsetree.Pmod_extension x0 ->
      To.Parsetree.Pmod_extension
        (copy_extension x0)

and copy_module_type :
  From.Parsetree.module_type ->
    To.Parsetree.module_type
  =
  fun
    { From.Parsetree.pmty_desc = pmty_desc;
      From.Parsetree.pmty_loc = pmty_loc;
      From.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      To.Parsetree.pmty_desc =
        (copy_module_type_desc pmty_desc);
      To.Parsetree.pmty_loc =
        (copy_location pmty_loc);
      To.Parsetree.pmty_attributes =
        (copy_attributes pmty_attributes)
    }

and copy_module_type_desc :
  From.Parsetree.module_type_desc ->
    To.Parsetree.module_type_desc
  =
  function
  | From.Parsetree.Pmty_ident x0 ->
      To.Parsetree.Pmty_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmty_signature x0 ->
      To.Parsetree.Pmty_signature
        (copy_signature x0)
  | From.Parsetree.Pmty_functor (x0,x1,x2) ->
      To.Parsetree.Pmty_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_type x2))
  | From.Parsetree.Pmty_with (x0,x1) ->
      To.Parsetree.Pmty_with
        ((copy_module_type x0),
          (List.map copy_with_constraint x1))
  | From.Parsetree.Pmty_typeof x0 ->
      To.Parsetree.Pmty_typeof
        (copy_module_expr x0)
  | From.Parsetree.Pmty_extension x0 ->
      To.Parsetree.Pmty_extension
        (copy_extension x0)
  | From.Parsetree.Pmty_alias x0 ->
      To.Parsetree.Pmty_alias
        (copy_loc copy_longident
           x0)

and copy_with_constraint :
  From.Parsetree.with_constraint ->
    To.Parsetree.with_constraint
  =
  function
  | From.Parsetree.Pwith_type (x0,x1) ->
      To.Parsetree.Pwith_type
        ((copy_loc
            copy_longident x0),
          (copy_type_declaration x1))
  | From.Parsetree.Pwith_module (x0,x1) ->
      To.Parsetree.Pwith_module
        ((copy_loc
            copy_longident x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pwith_typesubst x0 ->
      To.Parsetree.Pwith_typesubst
        (copy_type_declaration x0)
  | From.Parsetree.Pwith_modsubst (x0,x1) ->
      To.Parsetree.Pwith_modsubst
        ((copy_loc (fun x  -> x) x0),
          (copy_loc
             copy_longident x1))

and copy_signature :
  From.Parsetree.signature ->
    To.Parsetree.signature
  = fun x  -> List.map copy_signature_item x

and copy_signature_item :
  From.Parsetree.signature_item ->
    To.Parsetree.signature_item
  =
  fun
    { From.Parsetree.psig_desc = psig_desc;
      From.Parsetree.psig_loc = psig_loc }
     ->
    {
      To.Parsetree.psig_desc =
        (copy_signature_item_desc psig_desc);
      To.Parsetree.psig_loc =
        (copy_location psig_loc)
    }

and copy_signature_item_desc :
  From.Parsetree.signature_item_desc ->
    To.Parsetree.signature_item_desc
  =
  function
  | From.Parsetree.Psig_value x0 ->
      To.Parsetree.Psig_value
        (copy_value_description x0)
  | From.Parsetree.Psig_type (x0,x1) ->
      To.Parsetree.Psig_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Psig_typext x0 ->
      To.Parsetree.Psig_typext
        (copy_type_extension x0)
  | From.Parsetree.Psig_exception x0 ->
      To.Parsetree.Psig_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Psig_module x0 ->
      To.Parsetree.Psig_module
        (copy_module_declaration x0)
  | From.Parsetree.Psig_recmodule x0 ->
      To.Parsetree.Psig_recmodule
        (List.map copy_module_declaration x0)
  | From.Parsetree.Psig_modtype x0 ->
      To.Parsetree.Psig_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Psig_open x0 ->
      To.Parsetree.Psig_open
        (copy_open_description x0)
  | From.Parsetree.Psig_include x0 ->
      To.Parsetree.Psig_include
        (copy_include_description x0)
  | From.Parsetree.Psig_class x0 ->
      To.Parsetree.Psig_class
        (List.map copy_class_description x0)
  | From.Parsetree.Psig_class_type x0 ->
      To.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Psig_attribute x0 ->
      To.Parsetree.Psig_attribute
        (copy_attribute x0)
  | From.Parsetree.Psig_extension (x0,x1) ->
      To.Parsetree.Psig_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_class_type_declaration :
  From.Parsetree.class_type_declaration ->
    To.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_description :
  From.Parsetree.class_description ->
    To.Parsetree.class_description
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_type :
  From.Parsetree.class_type ->
    To.Parsetree.class_type
  =
  fun
    { From.Parsetree.pcty_desc = pcty_desc;
      From.Parsetree.pcty_loc = pcty_loc;
      From.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      To.Parsetree.pcty_desc =
        (copy_class_type_desc pcty_desc);
      To.Parsetree.pcty_loc =
        (copy_location pcty_loc);
      To.Parsetree.pcty_attributes =
        (copy_attributes pcty_attributes)
    }

and copy_class_type_desc :
  From.Parsetree.class_type_desc ->
    To.Parsetree.class_type_desc
  =
  function
  | From.Parsetree.Pcty_constr (x0,x1) ->
      To.Parsetree.Pcty_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcty_signature x0 ->
      To.Parsetree.Pcty_signature
        (copy_class_signature x0)
  | From.Parsetree.Pcty_arrow (x0,x1,x2) ->
      To.Parsetree.Pcty_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_class_type x2))
  | From.Parsetree.Pcty_extension x0 ->
      To.Parsetree.Pcty_extension
        (copy_extension x0)

and copy_class_signature :
  From.Parsetree.class_signature ->
    To.Parsetree.class_signature
  =
  fun
    { From.Parsetree.pcsig_self = pcsig_self;
      From.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      To.Parsetree.pcsig_self =
        (copy_core_type pcsig_self);
      To.Parsetree.pcsig_fields =
        (List.map copy_class_type_field
           pcsig_fields)
    }

and copy_class_type_field :
  From.Parsetree.class_type_field ->
    To.Parsetree.class_type_field
  =
  fun
    { From.Parsetree.pctf_desc = pctf_desc;
      From.Parsetree.pctf_loc = pctf_loc;
      From.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      To.Parsetree.pctf_desc =
        (copy_class_type_field_desc pctf_desc);
      To.Parsetree.pctf_loc =
        (copy_location pctf_loc);
      To.Parsetree.pctf_attributes =
        (copy_attributes pctf_attributes)
    }

and copy_class_type_field_desc :
  From.Parsetree.class_type_field_desc ->
    To.Parsetree.class_type_field_desc
  =
  function
  | From.Parsetree.Pctf_inherit x0 ->
      To.Parsetree.Pctf_inherit
        (copy_class_type x0)
  | From.Parsetree.Pctf_val x0 ->
      To.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_mutable_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_method x0 ->
      To.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_private_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_constraint x0 ->
      To.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pctf_attribute x0 ->
      To.Parsetree.Pctf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pctf_extension x0 ->
      To.Parsetree.Pctf_extension
        (copy_extension x0)

and copy_extension :
  From.Parsetree.extension ->
    To.Parsetree.extension
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.class_infos ->
        'g0 To.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pci_virt = pci_virt;
        From.Parsetree.pci_params = pci_params;
        From.Parsetree.pci_name = pci_name;
        From.Parsetree.pci_expr = pci_expr;
        From.Parsetree.pci_loc = pci_loc;
        From.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        To.Parsetree.pci_virt =
          (copy_virtual_flag pci_virt);
        To.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_core_type x0),
                  (copy_variance x1))) pci_params);
        To.Parsetree.pci_name =
          (copy_loc (fun x  -> x) pci_name);
        To.Parsetree.pci_expr = (f0 pci_expr);
        To.Parsetree.pci_loc =
          (copy_location pci_loc);
        To.Parsetree.pci_attributes =
          (copy_attributes pci_attributes)
      }

and copy_virtual_flag :
  From.Asttypes.virtual_flag ->
    To.Asttypes.virtual_flag
  =
  function
  | From.Asttypes.Virtual  -> To.Asttypes.Virtual
  | From.Asttypes.Concrete  -> To.Asttypes.Concrete

and copy_include_description :
  From.Parsetree.include_description ->
    To.Parsetree.include_description
  =
  fun x  ->
    copy_include_infos
      copy_module_type x

and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.include_infos ->
        'g0 To.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pincl_mod = pincl_mod;
        From.Parsetree.pincl_loc = pincl_loc;
        From.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        To.Parsetree.pincl_mod = (f0 pincl_mod);
        To.Parsetree.pincl_loc =
          (copy_location pincl_loc);
        To.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }

and copy_open_description :
  From.Parsetree.open_description ->
    To.Parsetree.open_description
  =
  fun
    { From.Parsetree.popen_lid = popen_lid;
      From.Parsetree.popen_override = popen_override;
      From.Parsetree.popen_loc = popen_loc;
      From.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      To.Parsetree.popen_lid =
        (copy_loc copy_longident
           popen_lid);
      To.Parsetree.popen_override =
        (copy_override_flag popen_override);
      To.Parsetree.popen_loc =
        (copy_location popen_loc);
      To.Parsetree.popen_attributes =
        (copy_attributes popen_attributes)
    }

and copy_override_flag :
  From.Asttypes.override_flag ->
    To.Asttypes.override_flag
  =
  function
  | From.Asttypes.Override  -> To.Asttypes.Override
  | From.Asttypes.Fresh  -> To.Asttypes.Fresh

and copy_module_type_declaration :
  From.Parsetree.module_type_declaration ->
    To.Parsetree.module_type_declaration
  =
  fun
    { From.Parsetree.pmtd_name = pmtd_name;
      From.Parsetree.pmtd_type = pmtd_type;
      From.Parsetree.pmtd_attributes = pmtd_attributes;
      From.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      To.Parsetree.pmtd_name =
        (copy_loc (fun x  -> x) pmtd_name);
      To.Parsetree.pmtd_type =
        (copy_option copy_module_type pmtd_type);
      To.Parsetree.pmtd_attributes =
        (copy_attributes pmtd_attributes);
      To.Parsetree.pmtd_loc =
        (copy_location pmtd_loc)
    }

and copy_module_declaration :
  From.Parsetree.module_declaration ->
    To.Parsetree.module_declaration
  =
  fun
    { From.Parsetree.pmd_name = pmd_name;
      From.Parsetree.pmd_type = pmd_type;
      From.Parsetree.pmd_attributes = pmd_attributes;
      From.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      To.Parsetree.pmd_name =
        (copy_loc (fun x  -> x) pmd_name);
      To.Parsetree.pmd_type =
        (copy_module_type pmd_type);
      To.Parsetree.pmd_attributes =
        (copy_attributes pmd_attributes);
      To.Parsetree.pmd_loc =
        (copy_location pmd_loc)
    }

and copy_type_extension :
  From.Parsetree.type_extension ->
    To.Parsetree.type_extension
  =
  fun
    { From.Parsetree.ptyext_path = ptyext_path;
      From.Parsetree.ptyext_params = ptyext_params;
      From.Parsetree.ptyext_constructors = ptyext_constructors;
      From.Parsetree.ptyext_private = ptyext_private;
      From.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      To.Parsetree.ptyext_path =
        (copy_loc copy_longident
           ptyext_path);
      To.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptyext_params);
      To.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor
           ptyext_constructors);
      To.Parsetree.ptyext_private =
        (copy_private_flag ptyext_private);
      To.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }

and copy_extension_constructor :
  From.Parsetree.extension_constructor ->
    To.Parsetree.extension_constructor
  =
  fun
    { From.Parsetree.pext_name = pext_name;
      From.Parsetree.pext_kind = pext_kind;
      From.Parsetree.pext_loc = pext_loc;
      From.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      To.Parsetree.pext_name =
        (copy_loc (fun x  -> x) pext_name);
      To.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      To.Parsetree.pext_loc =
        (copy_location pext_loc);
      To.Parsetree.pext_attributes =
        (copy_attributes pext_attributes)
    }

and copy_extension_constructor_kind :
  From.Parsetree.extension_constructor_kind ->
    To.Parsetree.extension_constructor_kind
  =
  function
  | From.Parsetree.Pext_decl (x0,x1) ->
      To.Parsetree.Pext_decl
        ((copy_constructor_arguments x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pext_rebind x0 ->
      To.Parsetree.Pext_rebind
        (copy_loc copy_longident
           x0)

and copy_type_declaration :
  From.Parsetree.type_declaration ->
    To.Parsetree.type_declaration
  =
  fun
    { From.Parsetree.ptype_name = ptype_name;
      From.Parsetree.ptype_params = ptype_params;
      From.Parsetree.ptype_cstrs = ptype_cstrs;
      From.Parsetree.ptype_kind = ptype_kind;
      From.Parsetree.ptype_private = ptype_private;
      From.Parsetree.ptype_manifest = ptype_manifest;
      From.Parsetree.ptype_attributes = ptype_attributes;
      From.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      To.Parsetree.ptype_name =
        (copy_loc (fun x  -> x) ptype_name);
      To.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptype_params);
      To.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_core_type x0),
                (copy_core_type x1),
                (copy_location x2))) ptype_cstrs);
      To.Parsetree.ptype_kind =
        (copy_type_kind ptype_kind);
      To.Parsetree.ptype_private =
        (copy_private_flag ptype_private);
      To.Parsetree.ptype_manifest =
        (copy_option copy_core_type ptype_manifest);
      To.Parsetree.ptype_attributes =
        (copy_attributes ptype_attributes);
      To.Parsetree.ptype_loc =
        (copy_location ptype_loc)
    }

and copy_private_flag :
  From.Asttypes.private_flag ->
    To.Asttypes.private_flag
  =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_type_kind :
  From.Parsetree.type_kind ->
    To.Parsetree.type_kind
  =
  function
  | From.Parsetree.Ptype_abstract  ->
      To.Parsetree.Ptype_abstract
  | From.Parsetree.Ptype_variant x0 ->
      To.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | From.Parsetree.Ptype_record x0 ->
      To.Parsetree.Ptype_record
        (List.map copy_label_declaration x0)
  | From.Parsetree.Ptype_open  ->
      To.Parsetree.Ptype_open

and copy_constructor_declaration :
  From.Parsetree.constructor_declaration ->
    To.Parsetree.constructor_declaration
  =
  fun
    { From.Parsetree.pcd_name = pcd_name;
      From.Parsetree.pcd_args = pcd_args;
      From.Parsetree.pcd_res = pcd_res;
      From.Parsetree.pcd_loc = pcd_loc;
      From.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      To.Parsetree.pcd_name =
        (copy_loc (fun x  -> x) pcd_name);
      To.Parsetree.pcd_args =
        (copy_constructor_arguments pcd_args);
      To.Parsetree.pcd_res =
        (copy_option copy_core_type pcd_res);
      To.Parsetree.pcd_loc =
        (copy_location pcd_loc);
      To.Parsetree.pcd_attributes =
        (copy_attributes pcd_attributes)
    }

and copy_constructor_arguments :
  From.Parsetree.constructor_arguments ->
    To.Parsetree.constructor_arguments
  =
  function
  | From.Parsetree.Pcstr_tuple x0 ->
      To.Parsetree.Pcstr_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Pcstr_record x0 ->
      To.Parsetree.Pcstr_record
        (List.map copy_label_declaration x0)

and copy_label_declaration :
  From.Parsetree.label_declaration ->
    To.Parsetree.label_declaration
  =
  fun
    { From.Parsetree.pld_name = pld_name;
      From.Parsetree.pld_mutable = pld_mutable;
      From.Parsetree.pld_type = pld_type;
      From.Parsetree.pld_loc = pld_loc;
      From.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      To.Parsetree.pld_name =
        (copy_loc (fun x  -> x) pld_name);
      To.Parsetree.pld_mutable =
        (copy_mutable_flag pld_mutable);
      To.Parsetree.pld_type =
        (copy_core_type pld_type);
      To.Parsetree.pld_loc =
        (copy_location pld_loc);
      To.Parsetree.pld_attributes =
        (copy_attributes pld_attributes)
    }

and copy_mutable_flag :
  From.Asttypes.mutable_flag ->
    To.Asttypes.mutable_flag
  =
  function
  | From.Asttypes.Immutable  ->
      To.Asttypes.Immutable
  | From.Asttypes.Mutable  -> To.Asttypes.Mutable

and copy_variance :
  From.Asttypes.variance -> To.Asttypes.variance =
  function
  | From.Asttypes.Covariant  ->
      To.Asttypes.Covariant
  | From.Asttypes.Contravariant  ->
      To.Asttypes.Contravariant
  | From.Asttypes.Invariant  ->
      To.Asttypes.Invariant

and copy_value_description :
  From.Parsetree.value_description ->
    To.Parsetree.value_description
  =
  fun
    { From.Parsetree.pval_name = pval_name;
      From.Parsetree.pval_type = pval_type;
      From.Parsetree.pval_prim = pval_prim;
      From.Parsetree.pval_attributes = pval_attributes;
      From.Parsetree.pval_loc = pval_loc }
     ->
    {
      To.Parsetree.pval_name =
        (copy_loc (fun x  -> x) pval_name);
      To.Parsetree.pval_type =
        (copy_core_type pval_type);
      To.Parsetree.pval_prim =
        (List.map (fun x  -> x) pval_prim);
      To.Parsetree.pval_attributes =
        (copy_attributes pval_attributes);
      To.Parsetree.pval_loc =
        (copy_location pval_loc)
    }

and copy_arg_label :
  From.Asttypes.arg_label -> To.Asttypes.arg_label
  =
  function
  | From.Asttypes.Nolabel  -> To.Asttypes.Nolabel
  | From.Asttypes.Labelled x0 ->
      To.Asttypes.Labelled x0
  | From.Asttypes.Optional x0 ->
      To.Asttypes.Optional x0

and copy_closed_flag :
  From.Asttypes.closed_flag ->
    To.Asttypes.closed_flag
  =
  function
  | From.Asttypes.Closed  -> To.Asttypes.Closed
  | From.Asttypes.Open  -> To.Asttypes.Open

and copy_label :
  From.Asttypes.label -> To.Asttypes.label =
  fun x  -> x

and copy_rec_flag :
  From.Asttypes.rec_flag -> To.Asttypes.rec_flag =
  function
  | From.Asttypes.Nonrecursive  ->
      To.Asttypes.Nonrecursive
  | From.Asttypes.Recursive  ->
      To.Asttypes.Recursive

and copy_constant :
  From.Parsetree.constant -> To.Parsetree.constant
  =
  function
  | From.Parsetree.Pconst_integer (x0,x1) ->
      To.Parsetree.Pconst_integer
        (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_char x0 ->
      To.Parsetree.Pconst_char x0
  | From.Parsetree.Pconst_string (x0,x1) ->
      To.Parsetree.Pconst_string
        (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_float (x0,x1) ->
      To.Parsetree.Pconst_float
        (x0, (copy_option (fun x  -> x) x1))

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_longident :
  From.Longident.t -> To.Longident.t =
  function
  | From.Longident.Lident x0 ->
      To.Longident.Lident x0
  | From.Longident.Ldot (x0,x1) ->
      To.Longident.Ldot
        ((copy_longident x0), x1)
  | From.Longident.Lapply (x0,x1) ->
      To.Longident.Lapply
        ((copy_longident x0), (copy_longident x1))

and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Asttypes.loc -> 'g0 To.Asttypes.loc
  =
  fun f0  ->
    fun
      { From.Asttypes.txt = txt;
        From.Asttypes.loc = loc }
       ->
      {
        To.Asttypes.txt = (f0 txt);
        To.Asttypes.loc = (copy_location loc)
      }

and copy_location :
  From.Location.t -> To.Location.t =
  fun
    { From.Location.loc_start = loc_start;
      From.Location.loc_end = loc_end;
      From.Location.loc_ghost = loc_ghost }
     ->
    {
      To.Location.loc_start = (copy_Lexing_position loc_start);
      To.Location.loc_end = (copy_Lexing_position loc_end);
      To.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }

let rec copy_out_phrase :
  From.Outcometree.out_phrase -> To.Outcometree.out_phrase =
  function
  | From.Outcometree.Ophr_eval (x0,x1) ->
      To.Outcometree.Ophr_eval
        ((copy_out_value x0),
          (copy_out_type x1))
  | From.Outcometree.Ophr_signature x0 ->
      To.Outcometree.Ophr_signature
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_sig_item x0),
                (copy_option copy_out_value x1))) x0)
  | From.Outcometree.Ophr_exception x0 ->
      To.Outcometree.Ophr_exception
        (let (x0,x1) = x0  in
         ((copy_exn x0), (copy_out_value x1)))

and copy_exn : exn -> exn = fun x  -> x

and copy_out_sig_item :
  From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item =
  function
  | From.Outcometree.Osig_class (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_class_type (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class_type
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_typext (x0,x1) ->
      To.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0),
          (copy_out_ext_status x1))
  | From.Outcometree.Osig_modtype (x0,x1) ->
      To.Outcometree.Osig_modtype
        (x0, (copy_out_module_type x1))
  | From.Outcometree.Osig_module (x0,x1,x2) ->
      To.Outcometree.Osig_module
        (x0, (copy_out_module_type x1),
          (copy_out_rec_status x2))
  | From.Outcometree.Osig_type (x0,x1) ->
      To.Outcometree.Osig_type
        ((copy_out_type_decl x0),
          (copy_out_rec_status x1))
  | From.Outcometree.Osig_value x0 ->
      To.Outcometree.Osig_value
        (copy_out_val_decl x0)
  | From.Outcometree.Osig_ellipsis  -> To.Outcometree.Osig_ellipsis

and copy_out_val_decl :
  From.Outcometree.out_val_decl -> To.Outcometree.out_val_decl =
  fun
    { From.Outcometree.oval_name = oval_name;
      From.Outcometree.oval_type = oval_type;
      From.Outcometree.oval_prims = oval_prims;
      From.Outcometree.oval_attributes = oval_attributes }
     ->
    {
      To.Outcometree.oval_name = oval_name;
      To.Outcometree.oval_type =
        (copy_out_type oval_type);
      To.Outcometree.oval_prims = (List.map (fun x  -> x) oval_prims);
      To.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }

and copy_out_type_decl :
  From.Outcometree.out_type_decl -> To.Outcometree.out_type_decl =
  fun
    { From.Outcometree.otype_name = otype_name;
      From.Outcometree.otype_params = otype_params;
      From.Outcometree.otype_type = otype_type;
      From.Outcometree.otype_private = otype_private;
      From.Outcometree.otype_immediate = otype_immediate;
      From.Outcometree.otype_unboxed = _otype_unboxed;
      From.Outcometree.otype_cstrs = otype_cstrs }
     ->
    {
      To.Outcometree.otype_name = otype_name;
      To.Outcometree.otype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
           otype_params);
      To.Outcometree.otype_type =
        (copy_out_type otype_type);
      To.Outcometree.otype_private =
        (copy_From_Asttypes_private_flag otype_private);
      To.Outcometree.otype_immediate = (copy_bool otype_immediate);
      To.Outcometree.otype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_type x0),
                (copy_out_type x1))) otype_cstrs)
    }

and copy_out_module_type :
  From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  =
  function
  | From.Outcometree.Omty_abstract  -> To.Outcometree.Omty_abstract
  | From.Outcometree.Omty_functor (x0,x1,x2) ->
      To.Outcometree.Omty_functor
        (x0, (copy_option copy_out_module_type x1),
          (copy_out_module_type x2))
  | From.Outcometree.Omty_ident x0 ->
      To.Outcometree.Omty_ident (copy_out_ident x0)
  | From.Outcometree.Omty_signature x0 ->
      To.Outcometree.Omty_signature
        (List.map copy_out_sig_item x0)
  | From.Outcometree.Omty_alias x0 ->
      To.Outcometree.Omty_alias (copy_out_ident x0)

and copy_out_ext_status :
  From.Outcometree.out_ext_status -> To.Outcometree.out_ext_status =
  function
  | From.Outcometree.Oext_first  -> To.Outcometree.Oext_first
  | From.Outcometree.Oext_next  -> To.Outcometree.Oext_next
  | From.Outcometree.Oext_exception  -> To.Outcometree.Oext_exception

and copy_out_extension_constructor :
  From.Outcometree.out_extension_constructor ->
    To.Outcometree.out_extension_constructor
  =
  fun
    { From.Outcometree.oext_name = oext_name;
      From.Outcometree.oext_type_name = oext_type_name;
      From.Outcometree.oext_type_params = oext_type_params;
      From.Outcometree.oext_args = oext_args;
      From.Outcometree.oext_ret_type = oext_ret_type;
      From.Outcometree.oext_private = oext_private }
     ->
    {
      To.Outcometree.oext_name = oext_name;
      To.Outcometree.oext_type_name = oext_type_name;
      To.Outcometree.oext_type_params =
        (List.map (fun x  -> x) oext_type_params);
      To.Outcometree.oext_args =
        (List.map copy_out_type oext_args);
      To.Outcometree.oext_ret_type =
        (copy_option copy_out_type oext_ret_type);
      To.Outcometree.oext_private =
        (copy_From_Asttypes_private_flag oext_private)
    }

and copy_From_Asttypes_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_out_rec_status :
  From.Outcometree.out_rec_status -> To.Outcometree.out_rec_status =
  function
  | From.Outcometree.Orec_not  -> To.Outcometree.Orec_not
  | From.Outcometree.Orec_first  -> To.Outcometree.Orec_first
  | From.Outcometree.Orec_next  -> To.Outcometree.Orec_next

and copy_out_class_type :
  From.Outcometree.out_class_type -> To.Outcometree.out_class_type =
  function
  | From.Outcometree.Octy_constr (x0,x1) ->
      To.Outcometree.Octy_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Octy_arrow (x0,x1,x2) ->
      To.Outcometree.Octy_arrow
        (x0, (copy_out_type x1),
          (copy_out_class_type x2))
  | From.Outcometree.Octy_signature (x0,x1) ->
      To.Outcometree.Octy_signature
        ((copy_option copy_out_type x0),
          (List.map copy_out_class_sig_item x1))

and copy_out_class_sig_item :
  From.Outcometree.out_class_sig_item ->
    To.Outcometree.out_class_sig_item
  =
  function
  | From.Outcometree.Ocsg_constraint (x0,x1) ->
      To.Outcometree.Ocsg_constraint
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Ocsg_method (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_method
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))
  | From.Outcometree.Ocsg_value (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_value
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))

and copy_out_type :
  From.Outcometree.out_type -> To.Outcometree.out_type =
  function
  | From.Outcometree.Otyp_abstract  -> To.Outcometree.Otyp_abstract
  | From.Outcometree.Otyp_open  -> To.Outcometree.Otyp_open
  | From.Outcometree.Otyp_alias (x0,x1) ->
      To.Outcometree.Otyp_alias
        ((copy_out_type x0), x1)
  | From.Outcometree.Otyp_arrow (x0,x1,x2) ->
      To.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1),
          (copy_out_type x2))
  | From.Outcometree.Otyp_class (x0,x1,x2) ->
      To.Outcometree.Otyp_class
        ((copy_bool x0), (copy_out_ident x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_constr (x0,x1) ->
      To.Outcometree.Otyp_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Otyp_manifest (x0,x1) ->
      To.Outcometree.Otyp_manifest
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Otyp_object (x0,x1) ->
      To.Outcometree.Otyp_object
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               (x0, (copy_out_type x1))) x0),
          (copy_option copy_bool x1))
  | From.Outcometree.Otyp_record x0 ->
      To.Outcometree.Otyp_record
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1), (copy_out_type x2)))
           x0)
  | From.Outcometree.Otyp_stuff x0 -> To.Outcometree.Otyp_stuff x0
  | From.Outcometree.Otyp_sum x0 ->
      To.Outcometree.Otyp_sum
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2))) x0)
  | From.Outcometree.Otyp_tuple x0 ->
      To.Outcometree.Otyp_tuple
        (List.map copy_out_type x0)
  | From.Outcometree.Otyp_var (x0,x1) ->
      To.Outcometree.Otyp_var ((copy_bool x0), x1)
  | From.Outcometree.Otyp_variant (x0,x1,x2,x3) ->
      To.Outcometree.Otyp_variant
        ((copy_bool x0), (copy_out_variant x1),
          (copy_bool x2),
          (copy_option (fun x  -> List.map (fun x  -> x) x) x3))
  | From.Outcometree.Otyp_poly (x0,x1) ->
      To.Outcometree.Otyp_poly
        ((List.map (fun x  -> x) x0), (copy_out_type x1))
  | From.Outcometree.Otyp_module (x0,x1,x2) ->
      To.Outcometree.Otyp_module
        (x0, (List.map (fun x  -> x) x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_attribute (x0,x1) ->
      To.Outcometree.Otyp_attribute
        ((copy_out_type x0),
          (copy_out_attribute x1))

and copy_out_attribute :
  From.Outcometree.out_attribute -> To.Outcometree.out_attribute =
  fun { From.Outcometree.oattr_name = oattr_name }  ->
    { To.Outcometree.oattr_name = oattr_name }

and copy_out_variant :
  From.Outcometree.out_variant -> To.Outcometree.out_variant =
  function
  | From.Outcometree.Ovar_fields x0 ->
      To.Outcometree.Ovar_fields
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1),
                (List.map copy_out_type x2))) x0)
  | From.Outcometree.Ovar_name (x0,x1) ->
      To.Outcometree.Ovar_name
        ((copy_out_ident x0),
          (List.map copy_out_type x1))

and copy_out_value :
  From.Outcometree.out_value -> To.Outcometree.out_value =
  function
  | From.Outcometree.Oval_array x0 ->
      To.Outcometree.Oval_array
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_char x0 -> To.Outcometree.Oval_char x0
  | From.Outcometree.Oval_constr (x0,x1) ->
      To.Outcometree.Oval_constr
        ((copy_out_ident x0),
          (List.map copy_out_value x1))
  | From.Outcometree.Oval_ellipsis  -> To.Outcometree.Oval_ellipsis
  | From.Outcometree.Oval_float x0 ->
      To.Outcometree.Oval_float (copy_float x0)
  | From.Outcometree.Oval_int x0 -> To.Outcometree.Oval_int x0
  | From.Outcometree.Oval_int32 x0 -> To.Outcometree.Oval_int32 x0
  | From.Outcometree.Oval_int64 x0 -> To.Outcometree.Oval_int64 x0
  | From.Outcometree.Oval_nativeint x0 ->
      To.Outcometree.Oval_nativeint x0
  | From.Outcometree.Oval_list x0 ->
      To.Outcometree.Oval_list
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_printer x0 ->
      To.Outcometree.Oval_printer x0
  | From.Outcometree.Oval_record x0 ->
      To.Outcometree.Oval_record
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_ident x0),
                (copy_out_value x1))) x0)
  | From.Outcometree.Oval_string x0 -> To.Outcometree.Oval_string x0
  | From.Outcometree.Oval_stuff x0 -> To.Outcometree.Oval_stuff x0
  | From.Outcometree.Oval_tuple x0 ->
      To.Outcometree.Oval_tuple
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_variant (x0,x1) ->
      To.Outcometree.Oval_variant
        (x0, (copy_option copy_out_value x1))

and copy_float : float -> float = fun x  -> x

and copy_out_ident :
  From.Outcometree.out_ident -> To.Outcometree.out_ident =
  function
  | From.Outcometree.Oide_apply (x0,x1) ->
      To.Outcometree.Oide_apply ((copy_out_ident x0), (copy_out_ident x1))
  | From.Outcometree.Oide_dot (x0,x1) ->
      To.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | From.Outcometree.Oide_ident x0 -> To.Outcometree.Oide_ident x0

let rec copy_toplevel_phrase :
  From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase =
  function
  | From.Parsetree.Ptop_def x0 ->
      To.Parsetree.Ptop_def (copy_structure x0)
  | From.Parsetree.Ptop_dir (x0,x1) ->
      To.Parsetree.Ptop_dir (x0, (copy_directive_argument x1))

and copy_directive_argument :
  From.Parsetree.directive_argument -> To.Parsetree.directive_argument =
  function
  | From.Parsetree.Pdir_none  -> To.Parsetree.Pdir_none
  | From.Parsetree.Pdir_string x0 -> To.Parsetree.Pdir_string x0
  | From.Parsetree.Pdir_int (x0,x1) ->
      To.Parsetree.Pdir_int (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pdir_ident x0 ->
      To.Parsetree.Pdir_ident (copy_longident x0)
  | From.Parsetree.Pdir_bool x0 ->
      To.Parsetree.Pdir_bool (copy_bool x0)

let copy_out_type_extension :
  From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension =
  fun
    { From.Outcometree.otyext_name = otyext_name;
      From.Outcometree.otyext_params = otyext_params;
      From.Outcometree.otyext_constructors = otyext_constructors;
      From.Outcometree.otyext_private = otyext_private }
     ->
    {
      To.Outcometree.otyext_name = otyext_name;
      To.Outcometree.otyext_params =
        (List.map (fun x  -> x) otyext_params);
      To.Outcometree.otyext_constructors =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2)))
           otyext_constructors);
      To.Outcometree.otyext_private =
        (copy_private_flag otyext_private)
    }

let copy_cases x = List.map copy_case x
let copy_pat = copy_pattern
let copy_expr = copy_expression
let copy_typ = copy_core_type

end
module Migrate_parsetree_403_404
= struct
#1 "migrate_parsetree_403_404.ml"
# 1 "src/migrate_parsetree_403_404.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Migrate_parsetree_403_404_migrate

(*$ open Printf
    let fields = [
      "attribute"; "attributes"; "case"; "cases"; "class_declaration";
      "class_description"; "class_expr"; "class_field"; "class_signature";
      "class_structure"; "class_type"; "class_type_declaration";
      "class_type_field"; "constructor_declaration"; "expr"; "extension";
      "extension_constructor"; "include_declaration"; "include_description";
      "label_declaration"; "location"; "module_binding"; "module_declaration";
      "module_expr"; "module_type"; "module_type_declaration";
      "open_description"; "pat"; "signature"; "signature_item"; "structure";
      "structure_item"; "typ"; "type_declaration"; "type_extension";
      "type_kind"; "value_binding"; "value_description";
      "with_constraint"; "payload"
    ]
  let foreach_field f =
    printf "\n";
    List.iter f fields
*)(*$*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$ foreach_field (printf "%s;\n")*)
     attribute;
     attributes;
     case;
     cases;
     class_declaration;
     class_description;
     class_expr;
     class_field;
     class_signature;
     class_structure;
     class_type;
     class_type_declaration;
     class_type_field;
     constructor_declaration;
     expr;
     extension;
     extension_constructor;
     include_declaration;
     include_description;
     label_declaration;
     location;
     module_binding;
     module_declaration;
     module_expr;
     module_type;
     module_type_declaration;
     open_description;
     pat;
     signature;
     signature_item;
     structure;
     structure_item;
     typ;
     type_declaration;
     type_extension;
     type_kind;
     value_binding;
     value_description;
     with_constraint;
     payload;
     (*$*)
   } as mapper) ->
  let module R = Migrate_parsetree_404_403_migrate in
  {
    To.Ast_mapper.
    (*$ foreach_field (fun s ->
          printf
          "%s = (fun _ x -> copy_%s (%s mapper (R.copy_%s x)));\n" s s s s)
    *)
    attribute = (fun _ x -> copy_attribute (attribute mapper (R.copy_attribute x)));
    attributes = (fun _ x -> copy_attributes (attributes mapper (R.copy_attributes x)));
    case = (fun _ x -> copy_case (case mapper (R.copy_case x)));
    cases = (fun _ x -> copy_cases (cases mapper (R.copy_cases x)));
    class_declaration = (fun _ x -> copy_class_declaration (class_declaration mapper (R.copy_class_declaration x)));
    class_description = (fun _ x -> copy_class_description (class_description mapper (R.copy_class_description x)));
    class_expr = (fun _ x -> copy_class_expr (class_expr mapper (R.copy_class_expr x)));
    class_field = (fun _ x -> copy_class_field (class_field mapper (R.copy_class_field x)));
    class_signature = (fun _ x -> copy_class_signature (class_signature mapper (R.copy_class_signature x)));
    class_structure = (fun _ x -> copy_class_structure (class_structure mapper (R.copy_class_structure x)));
    class_type = (fun _ x -> copy_class_type (class_type mapper (R.copy_class_type x)));
    class_type_declaration = (fun _ x -> copy_class_type_declaration (class_type_declaration mapper (R.copy_class_type_declaration x)));
    class_type_field = (fun _ x -> copy_class_type_field (class_type_field mapper (R.copy_class_type_field x)));
    constructor_declaration = (fun _ x -> copy_constructor_declaration (constructor_declaration mapper (R.copy_constructor_declaration x)));
    expr = (fun _ x -> copy_expr (expr mapper (R.copy_expr x)));
    extension = (fun _ x -> copy_extension (extension mapper (R.copy_extension x)));
    extension_constructor = (fun _ x -> copy_extension_constructor (extension_constructor mapper (R.copy_extension_constructor x)));
    include_declaration = (fun _ x -> copy_include_declaration (include_declaration mapper (R.copy_include_declaration x)));
    include_description = (fun _ x -> copy_include_description (include_description mapper (R.copy_include_description x)));
    label_declaration = (fun _ x -> copy_label_declaration (label_declaration mapper (R.copy_label_declaration x)));
    location = (fun _ x -> copy_location (location mapper (R.copy_location x)));
    module_binding = (fun _ x -> copy_module_binding (module_binding mapper (R.copy_module_binding x)));
    module_declaration = (fun _ x -> copy_module_declaration (module_declaration mapper (R.copy_module_declaration x)));
    module_expr = (fun _ x -> copy_module_expr (module_expr mapper (R.copy_module_expr x)));
    module_type = (fun _ x -> copy_module_type (module_type mapper (R.copy_module_type x)));
    module_type_declaration = (fun _ x -> copy_module_type_declaration (module_type_declaration mapper (R.copy_module_type_declaration x)));
    open_description = (fun _ x -> copy_open_description (open_description mapper (R.copy_open_description x)));
    pat = (fun _ x -> copy_pat (pat mapper (R.copy_pat x)));
    signature = (fun _ x -> copy_signature (signature mapper (R.copy_signature x)));
    signature_item = (fun _ x -> copy_signature_item (signature_item mapper (R.copy_signature_item x)));
    structure = (fun _ x -> copy_structure (structure mapper (R.copy_structure x)));
    structure_item = (fun _ x -> copy_structure_item (structure_item mapper (R.copy_structure_item x)));
    typ = (fun _ x -> copy_typ (typ mapper (R.copy_typ x)));
    type_declaration = (fun _ x -> copy_type_declaration (type_declaration mapper (R.copy_type_declaration x)));
    type_extension = (fun _ x -> copy_type_extension (type_extension mapper (R.copy_type_extension x)));
    type_kind = (fun _ x -> copy_type_kind (type_kind mapper (R.copy_type_kind x)));
    value_binding = (fun _ x -> copy_value_binding (value_binding mapper (R.copy_value_binding x)));
    value_description = (fun _ x -> copy_value_description (value_description mapper (R.copy_value_description x)));
    with_constraint = (fun _ x -> copy_with_constraint (with_constraint mapper (R.copy_with_constraint x)));
    payload = (fun _ x -> copy_payload (payload mapper (R.copy_payload x)));
    (*$*)
  }

end
module Migrate_parsetree_404_403
= struct
#1 "migrate_parsetree_404_403.ml"
# 1 "src/migrate_parsetree_404_403.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Migrate_parsetree_404_403_migrate

(*$ open Printf
    let fields = [
      "attribute"; "attributes"; "case"; "cases"; "class_declaration";
      "class_description"; "class_expr"; "class_field"; "class_signature";
      "class_structure"; "class_type"; "class_type_declaration";
      "class_type_field"; "constructor_declaration"; "expr"; "extension";
      "extension_constructor"; "include_declaration"; "include_description";
      "label_declaration"; "location"; "module_binding"; "module_declaration";
      "module_expr"; "module_type"; "module_type_declaration";
      "open_description"; "pat"; "signature"; "signature_item"; "structure";
      "structure_item"; "typ"; "type_declaration"; "type_extension";
      "type_kind"; "value_binding"; "value_description";
      "with_constraint"; "payload"
    ]
  let foreach_field f =
    printf "\n";
    List.iter f fields
*)(*$*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$ foreach_field (printf "%s;\n")*)
     attribute;
     attributes;
     case;
     cases;
     class_declaration;
     class_description;
     class_expr;
     class_field;
     class_signature;
     class_structure;
     class_type;
     class_type_declaration;
     class_type_field;
     constructor_declaration;
     expr;
     extension;
     extension_constructor;
     include_declaration;
     include_description;
     label_declaration;
     location;
     module_binding;
     module_declaration;
     module_expr;
     module_type;
     module_type_declaration;
     open_description;
     pat;
     signature;
     signature_item;
     structure;
     structure_item;
     typ;
     type_declaration;
     type_extension;
     type_kind;
     value_binding;
     value_description;
     with_constraint;
     payload;
     (*$*)
   } as mapper) ->
  let module R = Migrate_parsetree_403_404_migrate in
  {
    To.Ast_mapper.
    (*$ foreach_field (fun s ->
          printf
          "%s = (fun _ x -> copy_%s (%s mapper (R.copy_%s x)));\n" s s s s)
    *)
    attribute = (fun _ x -> copy_attribute (attribute mapper (R.copy_attribute x)));
    attributes = (fun _ x -> copy_attributes (attributes mapper (R.copy_attributes x)));
    case = (fun _ x -> copy_case (case mapper (R.copy_case x)));
    cases = (fun _ x -> copy_cases (cases mapper (R.copy_cases x)));
    class_declaration = (fun _ x -> copy_class_declaration (class_declaration mapper (R.copy_class_declaration x)));
    class_description = (fun _ x -> copy_class_description (class_description mapper (R.copy_class_description x)));
    class_expr = (fun _ x -> copy_class_expr (class_expr mapper (R.copy_class_expr x)));
    class_field = (fun _ x -> copy_class_field (class_field mapper (R.copy_class_field x)));
    class_signature = (fun _ x -> copy_class_signature (class_signature mapper (R.copy_class_signature x)));
    class_structure = (fun _ x -> copy_class_structure (class_structure mapper (R.copy_class_structure x)));
    class_type = (fun _ x -> copy_class_type (class_type mapper (R.copy_class_type x)));
    class_type_declaration = (fun _ x -> copy_class_type_declaration (class_type_declaration mapper (R.copy_class_type_declaration x)));
    class_type_field = (fun _ x -> copy_class_type_field (class_type_field mapper (R.copy_class_type_field x)));
    constructor_declaration = (fun _ x -> copy_constructor_declaration (constructor_declaration mapper (R.copy_constructor_declaration x)));
    expr = (fun _ x -> copy_expr (expr mapper (R.copy_expr x)));
    extension = (fun _ x -> copy_extension (extension mapper (R.copy_extension x)));
    extension_constructor = (fun _ x -> copy_extension_constructor (extension_constructor mapper (R.copy_extension_constructor x)));
    include_declaration = (fun _ x -> copy_include_declaration (include_declaration mapper (R.copy_include_declaration x)));
    include_description = (fun _ x -> copy_include_description (include_description mapper (R.copy_include_description x)));
    label_declaration = (fun _ x -> copy_label_declaration (label_declaration mapper (R.copy_label_declaration x)));
    location = (fun _ x -> copy_location (location mapper (R.copy_location x)));
    module_binding = (fun _ x -> copy_module_binding (module_binding mapper (R.copy_module_binding x)));
    module_declaration = (fun _ x -> copy_module_declaration (module_declaration mapper (R.copy_module_declaration x)));
    module_expr = (fun _ x -> copy_module_expr (module_expr mapper (R.copy_module_expr x)));
    module_type = (fun _ x -> copy_module_type (module_type mapper (R.copy_module_type x)));
    module_type_declaration = (fun _ x -> copy_module_type_declaration (module_type_declaration mapper (R.copy_module_type_declaration x)));
    open_description = (fun _ x -> copy_open_description (open_description mapper (R.copy_open_description x)));
    pat = (fun _ x -> copy_pat (pat mapper (R.copy_pat x)));
    signature = (fun _ x -> copy_signature (signature mapper (R.copy_signature x)));
    signature_item = (fun _ x -> copy_signature_item (signature_item mapper (R.copy_signature_item x)));
    structure = (fun _ x -> copy_structure (structure mapper (R.copy_structure x)));
    structure_item = (fun _ x -> copy_structure_item (structure_item mapper (R.copy_structure_item x)));
    typ = (fun _ x -> copy_typ (typ mapper (R.copy_typ x)));
    type_declaration = (fun _ x -> copy_type_declaration (type_declaration mapper (R.copy_type_declaration x)));
    type_extension = (fun _ x -> copy_type_extension (type_extension mapper (R.copy_type_extension x)));
    type_kind = (fun _ x -> copy_type_kind (type_kind mapper (R.copy_type_kind x)));
    value_binding = (fun _ x -> copy_value_binding (value_binding mapper (R.copy_value_binding x)));
    value_description = (fun _ x -> copy_value_description (value_description mapper (R.copy_value_description x)));
    with_constraint = (fun _ x -> copy_with_constraint (with_constraint mapper (R.copy_with_constraint x)));
    payload = (fun _ x -> copy_payload (payload mapper (R.copy_payload x)));
    (*$*)
  }

end
module Migrate_parsetree_404_405_migrate
= struct
#1 "migrate_parsetree_404_405_migrate.ml"
# 1 "src/migrate_parsetree_404_405_migrate.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module From = Ast_404
module To = Ast_405

let noloc x = { Location. txt = x; loc = Location.none }

let rec copy_expression :
  From.Parsetree.expression -> To.Parsetree.expression =
  fun
    { From.Parsetree.pexp_desc = pexp_desc;
      From.Parsetree.pexp_loc = pexp_loc;
      From.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      To.Parsetree.pexp_desc = (copy_expression_desc pexp_desc);
      To.Parsetree.pexp_loc = (copy_location pexp_loc);
      To.Parsetree.pexp_attributes = (copy_attributes pexp_attributes)
    }

and copy_expression_desc :
  From.Parsetree.expression_desc -> To.Parsetree.expression_desc =
  function
  | From.Parsetree.Pexp_ident x0 ->
      To.Parsetree.Pexp_ident
        (copy_loc copy_longident x0)
  | From.Parsetree.Pexp_constant x0 ->
      To.Parsetree.Pexp_constant (copy_constant x0)
  | From.Parsetree.Pexp_let (x0,x1,x2) ->
      To.Parsetree.Pexp_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_function x0 ->
      To.Parsetree.Pexp_function
        (List.map copy_case x0)
  | From.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pexp_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_expression x3))
  | From.Parsetree.Pexp_apply (x0,x1) ->
      To.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pexp_match (x0,x1) ->
      To.Parsetree.Pexp_match
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_try (x0,x1) ->
      To.Parsetree.Pexp_try
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_tuple x0 ->
      To.Parsetree.Pexp_tuple
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_construct (x0,x1) ->
      To.Parsetree.Pexp_construct
        ((copy_loc copy_longident x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_variant (x0,x1) ->
      To.Parsetree.Pexp_variant
        ((copy_label x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_record (x0,x1) ->
      To.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc copy_longident x0),
                 (copy_expression x1))) x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_field (x0,x1) ->
      To.Parsetree.Pexp_field
        ((copy_expression x0),
          (copy_loc copy_longident x1))
  | From.Parsetree.Pexp_setfield (x0,x1,x2) ->
      To.Parsetree.Pexp_setfield
        ((copy_expression x0),
          (copy_loc copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_array x0 ->
      To.Parsetree.Pexp_array
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      To.Parsetree.Pexp_ifthenelse
        ((copy_expression x0),
          (copy_expression x1),
          (copy_option copy_expression x2))
  | From.Parsetree.Pexp_sequence (x0,x1) ->
      To.Parsetree.Pexp_sequence
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_while (x0,x1) ->
      To.Parsetree.Pexp_while
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      To.Parsetree.Pexp_for
        ((copy_pattern x0),
          (copy_expression x1),
          (copy_expression x2),
          (copy_direction_flag x3),
          (copy_expression x4))
  | From.Parsetree.Pexp_constraint (x0,x1) ->
      To.Parsetree.Pexp_constraint
        ((copy_expression x0),
          (copy_core_type x1))
  | From.Parsetree.Pexp_coerce (x0,x1,x2) ->
      To.Parsetree.Pexp_coerce
        ((copy_expression x0),
          (copy_option copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Pexp_send (x0,x1) ->
      To.Parsetree.Pexp_send
        ((copy_expression x0), noloc x1)
  | From.Parsetree.Pexp_new x0 ->
      To.Parsetree.Pexp_new
        (copy_loc copy_longident x0)
  | From.Parsetree.Pexp_setinstvar (x0,x1) ->
      To.Parsetree.Pexp_setinstvar
        ((copy_loc (fun x  -> x) x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_override x0 ->
      To.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_loc (fun x  -> x) x0),
                (copy_expression x1))) x0)
  | From.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      To.Parsetree.Pexp_letmodule
        ((copy_loc (fun x  -> x) x0),
          (copy_module_expr x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_letexception (x0,x1) ->
      To.Parsetree.Pexp_letexception
        ((copy_extension_constructor x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_assert x0 ->
      To.Parsetree.Pexp_assert (copy_expression x0)
  | From.Parsetree.Pexp_lazy x0 ->
      To.Parsetree.Pexp_lazy (copy_expression x0)
  | From.Parsetree.Pexp_poly (x0,x1) ->
      To.Parsetree.Pexp_poly
        ((copy_expression x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pexp_object x0 ->
      To.Parsetree.Pexp_object
        (copy_class_structure x0)
  | From.Parsetree.Pexp_newtype (x0,x1) ->
      To.Parsetree.Pexp_newtype
        (noloc x0, (copy_expression x1))
  | From.Parsetree.Pexp_pack x0 ->
      To.Parsetree.Pexp_pack (copy_module_expr x0)
  | From.Parsetree.Pexp_open (x0,x1,x2) ->
      To.Parsetree.Pexp_open
        ((copy_override_flag x0),
          (copy_loc copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_extension x0 ->
      To.Parsetree.Pexp_extension (copy_extension x0)
  | From.Parsetree.Pexp_unreachable  -> To.Parsetree.Pexp_unreachable

and copy_direction_flag :
  From.Asttypes.direction_flag -> To.Asttypes.direction_flag =
  function
  | From.Asttypes.Upto  -> To.Asttypes.Upto
  | From.Asttypes.Downto  -> To.Asttypes.Downto

and copy_case :
  From.Parsetree.case -> To.Parsetree.case =
  fun
    { From.Parsetree.pc_lhs = pc_lhs;
      From.Parsetree.pc_guard = pc_guard;
      From.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      To.Parsetree.pc_lhs = (copy_pattern pc_lhs);
      To.Parsetree.pc_guard =
        (copy_option copy_expression pc_guard);
      To.Parsetree.pc_rhs = (copy_expression pc_rhs)
    }

and copy_value_binding :
  From.Parsetree.value_binding -> To.Parsetree.value_binding =
  fun
    { From.Parsetree.pvb_pat = pvb_pat;
      From.Parsetree.pvb_expr = pvb_expr;
      From.Parsetree.pvb_attributes = pvb_attributes;
      From.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      To.Parsetree.pvb_pat = (copy_pattern pvb_pat);
      To.Parsetree.pvb_expr =
        (copy_expression pvb_expr);
      To.Parsetree.pvb_attributes =
        (copy_attributes pvb_attributes);
      To.Parsetree.pvb_loc = (copy_location pvb_loc)
    }

and copy_pattern :
  From.Parsetree.pattern -> To.Parsetree.pattern =
  fun
    { From.Parsetree.ppat_desc = ppat_desc;
      From.Parsetree.ppat_loc = ppat_loc;
      From.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      To.Parsetree.ppat_desc =
        (copy_pattern_desc ppat_desc);
      To.Parsetree.ppat_loc = (copy_location ppat_loc);
      To.Parsetree.ppat_attributes =
        (copy_attributes ppat_attributes)
    }

and copy_pattern_desc :
  From.Parsetree.pattern_desc -> To.Parsetree.pattern_desc =
  function
  | From.Parsetree.Ppat_any  -> To.Parsetree.Ppat_any
  | From.Parsetree.Ppat_var x0 ->
      To.Parsetree.Ppat_var (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_alias (x0,x1) ->
      To.Parsetree.Ppat_alias
        ((copy_pattern x0),
          (copy_loc (fun x  -> x) x1))
  | From.Parsetree.Ppat_constant x0 ->
      To.Parsetree.Ppat_constant (copy_constant x0)
  | From.Parsetree.Ppat_interval (x0,x1) ->
      To.Parsetree.Ppat_interval
        ((copy_constant x0),
          (copy_constant x1))
  | From.Parsetree.Ppat_tuple x0 ->
      To.Parsetree.Ppat_tuple
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_construct (x0,x1) ->
      To.Parsetree.Ppat_construct
        ((copy_loc copy_longident x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_variant (x0,x1) ->
      To.Parsetree.Ppat_variant
        ((copy_label x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_record (x0,x1) ->
      To.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc copy_longident x0),
                 (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ppat_array x0 ->
      To.Parsetree.Ppat_array
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_or (x0,x1) ->
      To.Parsetree.Ppat_or
        ((copy_pattern x0),
          (copy_pattern x1))
  | From.Parsetree.Ppat_constraint (x0,x1) ->
      To.Parsetree.Ppat_constraint
        ((copy_pattern x0),
          (copy_core_type x1))
  | From.Parsetree.Ppat_type x0 ->
      To.Parsetree.Ppat_type
        (copy_loc copy_longident x0)
  | From.Parsetree.Ppat_lazy x0 ->
      To.Parsetree.Ppat_lazy (copy_pattern x0)
  | From.Parsetree.Ppat_unpack x0 ->
      To.Parsetree.Ppat_unpack
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_exception x0 ->
      To.Parsetree.Ppat_exception (copy_pattern x0)
  | From.Parsetree.Ppat_extension x0 ->
      To.Parsetree.Ppat_extension (copy_extension x0)
  | From.Parsetree.Ppat_open (x0,x1) ->
      To.Parsetree.Ppat_open
        ((copy_loc copy_longident x0),
          (copy_pattern x1))

and copy_core_type :
  From.Parsetree.core_type -> To.Parsetree.core_type =
  fun
    { From.Parsetree.ptyp_desc = ptyp_desc;
      From.Parsetree.ptyp_loc = ptyp_loc;
      From.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      To.Parsetree.ptyp_desc =
        (copy_core_type_desc ptyp_desc);
      To.Parsetree.ptyp_loc = (copy_location ptyp_loc);
      To.Parsetree.ptyp_attributes =
        (copy_attributes ptyp_attributes)
    }

and copy_core_type_desc :
  From.Parsetree.core_type_desc -> To.Parsetree.core_type_desc =
  function
  | From.Parsetree.Ptyp_any  -> To.Parsetree.Ptyp_any
  | From.Parsetree.Ptyp_var x0 -> To.Parsetree.Ptyp_var x0
  | From.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      To.Parsetree.Ptyp_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Ptyp_tuple x0 ->
      To.Parsetree.Ptyp_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Ptyp_constr (x0,x1) ->
      To.Parsetree.Ptyp_constr
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_object (x0,x1) ->
      To.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (noloc x0, (copy_attributes x1),
                 (copy_core_type x2))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ptyp_class (x0,x1) ->
      To.Parsetree.Ptyp_class
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_alias (x0,x1) ->
      To.Parsetree.Ptyp_alias
        ((copy_core_type x0), x1)
  | From.Parsetree.Ptyp_variant (x0,x1,x2) ->
      To.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0),
          (copy_closed_flag x1),
          (copy_option (fun x  -> List.map copy_label x) x2))
  | From.Parsetree.Ptyp_poly (x0,x1) ->
      To.Parsetree.Ptyp_poly
        ((List.map (fun x  -> noloc x) x0), (copy_core_type x1))
  | From.Parsetree.Ptyp_package x0 ->
      To.Parsetree.Ptyp_package (copy_package_type x0)
  | From.Parsetree.Ptyp_extension x0 ->
      To.Parsetree.Ptyp_extension (copy_extension x0)

and copy_package_type :
  From.Parsetree.package_type -> To.Parsetree.package_type =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc copy_longident x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_loc copy_longident x0),
              (copy_core_type x1))) x1))

and copy_row_field :
  From.Parsetree.row_field -> To.Parsetree.row_field =
  function
  | From.Parsetree.Rtag (x0,x1,x2,x3) ->
      To.Parsetree.Rtag
        ((copy_label x0),
          (copy_attributes x1), (copy_bool x2),
          (List.map copy_core_type x3))
  | From.Parsetree.Rinherit x0 ->
      To.Parsetree.Rinherit (copy_core_type x0)

and copy_attributes :
  From.Parsetree.attributes -> To.Parsetree.attributes =
  fun x  -> List.map copy_attribute x

and copy_attribute :
  From.Parsetree.attribute -> To.Parsetree.attribute =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_payload :
  From.Parsetree.payload -> To.Parsetree.payload =
  function
  | From.Parsetree.PStr x0 ->
      To.Parsetree.PStr (copy_structure x0)
  | From.Parsetree.PSig x0 ->
      To.Parsetree.PSig (copy_signature x0)
  | From.Parsetree.PTyp x0 ->
      To.Parsetree.PTyp (copy_core_type x0)
  | From.Parsetree.PPat (x0,x1) ->
      To.Parsetree.PPat
        ((copy_pattern x0),
          (copy_option copy_expression x1))

and copy_structure :
  From.Parsetree.structure -> To.Parsetree.structure =
  fun x  -> List.map copy_structure_item x

and copy_structure_item :
  From.Parsetree.structure_item -> To.Parsetree.structure_item =
  fun
    { From.Parsetree.pstr_desc = pstr_desc;
      From.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      To.Parsetree.pstr_desc =
        (copy_structure_item_desc pstr_desc);
      To.Parsetree.pstr_loc = (copy_location pstr_loc)
    }

and copy_structure_item_desc :
  From.Parsetree.structure_item_desc ->
    To.Parsetree.structure_item_desc
  =
  function
  | From.Parsetree.Pstr_eval (x0,x1) ->
      To.Parsetree.Pstr_eval
        ((copy_expression x0),
          (copy_attributes x1))
  | From.Parsetree.Pstr_value (x0,x1) ->
      To.Parsetree.Pstr_value
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1))
  | From.Parsetree.Pstr_primitive x0 ->
      To.Parsetree.Pstr_primitive
        (copy_value_description x0)
  | From.Parsetree.Pstr_type (x0,x1) ->
      To.Parsetree.Pstr_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Pstr_typext x0 ->
      To.Parsetree.Pstr_typext
        (copy_type_extension x0)
  | From.Parsetree.Pstr_exception x0 ->
      To.Parsetree.Pstr_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Pstr_module x0 ->
      To.Parsetree.Pstr_module
        (copy_module_binding x0)
  | From.Parsetree.Pstr_recmodule x0 ->
      To.Parsetree.Pstr_recmodule
        (List.map copy_module_binding x0)
  | From.Parsetree.Pstr_modtype x0 ->
      To.Parsetree.Pstr_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Pstr_open x0 ->
      To.Parsetree.Pstr_open
        (copy_open_description x0)
  | From.Parsetree.Pstr_class x0 ->
      To.Parsetree.Pstr_class
        (List.map copy_class_declaration x0)
  | From.Parsetree.Pstr_class_type x0 ->
      To.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Pstr_include x0 ->
      To.Parsetree.Pstr_include
        (copy_include_declaration x0)
  | From.Parsetree.Pstr_attribute x0 ->
      To.Parsetree.Pstr_attribute (copy_attribute x0)
  | From.Parsetree.Pstr_extension (x0,x1) ->
      To.Parsetree.Pstr_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_include_declaration :
  From.Parsetree.include_declaration ->
    To.Parsetree.include_declaration
  =
  fun x  ->
    copy_include_infos copy_module_expr x

and copy_class_declaration :
  From.Parsetree.class_declaration -> To.Parsetree.class_declaration
  =
  fun x  ->
    copy_class_infos copy_class_expr x

and copy_class_expr :
  From.Parsetree.class_expr -> To.Parsetree.class_expr =
  fun
    { From.Parsetree.pcl_desc = pcl_desc;
      From.Parsetree.pcl_loc = pcl_loc;
      From.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      To.Parsetree.pcl_desc =
        (copy_class_expr_desc pcl_desc);
      To.Parsetree.pcl_loc = (copy_location pcl_loc);
      To.Parsetree.pcl_attributes =
        (copy_attributes pcl_attributes)
    }

and copy_class_expr_desc :
  From.Parsetree.class_expr_desc -> To.Parsetree.class_expr_desc =
  function
  | From.Parsetree.Pcl_constr (x0,x1) ->
      To.Parsetree.Pcl_constr
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcl_structure x0 ->
      To.Parsetree.Pcl_structure
        (copy_class_structure x0)
  | From.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pcl_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_class_expr x3))
  | From.Parsetree.Pcl_apply (x0,x1) ->
      To.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pcl_let (x0,x1,x2) ->
      To.Parsetree.Pcl_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | From.Parsetree.Pcl_constraint (x0,x1) ->
      To.Parsetree.Pcl_constraint
        ((copy_class_expr x0),
          (copy_class_type x1))
  | From.Parsetree.Pcl_extension x0 ->
      To.Parsetree.Pcl_extension (copy_extension x0)

and copy_class_structure :
  From.Parsetree.class_structure -> To.Parsetree.class_structure =
  fun
    { From.Parsetree.pcstr_self = pcstr_self;
      From.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      To.Parsetree.pcstr_self =
        (copy_pattern pcstr_self);
      To.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }

and copy_class_field :
  From.Parsetree.class_field -> To.Parsetree.class_field =
  fun
    { From.Parsetree.pcf_desc = pcf_desc;
      From.Parsetree.pcf_loc = pcf_loc;
      From.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      To.Parsetree.pcf_desc =
        (copy_class_field_desc pcf_desc);
      To.Parsetree.pcf_loc = (copy_location pcf_loc);
      To.Parsetree.pcf_attributes =
        (copy_attributes pcf_attributes)
    }

and copy_class_field_desc :
  From.Parsetree.class_field_desc -> To.Parsetree.class_field_desc =
  function
  | From.Parsetree.Pcf_inherit (x0,x1,x2) ->
      To.Parsetree.Pcf_inherit
        ((copy_override_flag x0),
          (copy_class_expr x1),
          (copy_option (fun x  -> noloc x) x2))
  | From.Parsetree.Pcf_val x0 ->
      To.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_method x0 ->
      To.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_constraint x0 ->
      To.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pcf_initializer x0 ->
      To.Parsetree.Pcf_initializer
        (copy_expression x0)
  | From.Parsetree.Pcf_attribute x0 ->
      To.Parsetree.Pcf_attribute (copy_attribute x0)
  | From.Parsetree.Pcf_extension x0 ->
      To.Parsetree.Pcf_extension (copy_extension x0)

and copy_class_field_kind :
  From.Parsetree.class_field_kind -> To.Parsetree.class_field_kind =
  function
  | From.Parsetree.Cfk_virtual x0 ->
      To.Parsetree.Cfk_virtual (copy_core_type x0)
  | From.Parsetree.Cfk_concrete (x0,x1) ->
      To.Parsetree.Cfk_concrete
        ((copy_override_flag x0),
          (copy_expression x1))

and copy_module_binding :
  From.Parsetree.module_binding -> To.Parsetree.module_binding =
  fun
    { From.Parsetree.pmb_name = pmb_name;
      From.Parsetree.pmb_expr = pmb_expr;
      From.Parsetree.pmb_attributes = pmb_attributes;
      From.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      To.Parsetree.pmb_name =
        (copy_loc (fun x  -> x) pmb_name);
      To.Parsetree.pmb_expr =
        (copy_module_expr pmb_expr);
      To.Parsetree.pmb_attributes =
        (copy_attributes pmb_attributes);
      To.Parsetree.pmb_loc = (copy_location pmb_loc)
    }

and copy_module_expr :
  From.Parsetree.module_expr -> To.Parsetree.module_expr =
  fun
    { From.Parsetree.pmod_desc = pmod_desc;
      From.Parsetree.pmod_loc = pmod_loc;
      From.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      To.Parsetree.pmod_desc =
        (copy_module_expr_desc pmod_desc);
      To.Parsetree.pmod_loc = (copy_location pmod_loc);
      To.Parsetree.pmod_attributes =
        (copy_attributes pmod_attributes)
    }

and copy_module_expr_desc :
  From.Parsetree.module_expr_desc -> To.Parsetree.module_expr_desc =
  function
  | From.Parsetree.Pmod_ident x0 ->
      To.Parsetree.Pmod_ident
        (copy_loc copy_longident x0)
  | From.Parsetree.Pmod_structure x0 ->
      To.Parsetree.Pmod_structure (copy_structure x0)
  | From.Parsetree.Pmod_functor (x0,x1,x2) ->
      To.Parsetree.Pmod_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_expr x2))
  | From.Parsetree.Pmod_apply (x0,x1) ->
      To.Parsetree.Pmod_apply
        ((copy_module_expr x0),
          (copy_module_expr x1))
  | From.Parsetree.Pmod_constraint (x0,x1) ->
      To.Parsetree.Pmod_constraint
        ((copy_module_expr x0),
          (copy_module_type x1))
  | From.Parsetree.Pmod_unpack x0 ->
      To.Parsetree.Pmod_unpack (copy_expression x0)
  | From.Parsetree.Pmod_extension x0 ->
      To.Parsetree.Pmod_extension (copy_extension x0)

and copy_module_type :
  From.Parsetree.module_type -> To.Parsetree.module_type =
  fun
    { From.Parsetree.pmty_desc = pmty_desc;
      From.Parsetree.pmty_loc = pmty_loc;
      From.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      To.Parsetree.pmty_desc =
        (copy_module_type_desc pmty_desc);
      To.Parsetree.pmty_loc = (copy_location pmty_loc);
      To.Parsetree.pmty_attributes =
        (copy_attributes pmty_attributes)
    }

and copy_module_type_desc :
  From.Parsetree.module_type_desc -> To.Parsetree.module_type_desc =
  function
  | From.Parsetree.Pmty_ident x0 ->
      To.Parsetree.Pmty_ident
        (copy_loc copy_longident x0)
  | From.Parsetree.Pmty_signature x0 ->
      To.Parsetree.Pmty_signature (copy_signature x0)
  | From.Parsetree.Pmty_functor (x0,x1,x2) ->
      To.Parsetree.Pmty_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_type x2))
  | From.Parsetree.Pmty_with (x0,x1) ->
      To.Parsetree.Pmty_with
        ((copy_module_type x0),
          (List.map copy_with_constraint x1))
  | From.Parsetree.Pmty_typeof x0 ->
      To.Parsetree.Pmty_typeof (copy_module_expr x0)
  | From.Parsetree.Pmty_extension x0 ->
      To.Parsetree.Pmty_extension (copy_extension x0)
  | From.Parsetree.Pmty_alias x0 ->
      To.Parsetree.Pmty_alias
        (copy_loc copy_longident x0)

and copy_with_constraint :
  From.Parsetree.with_constraint -> To.Parsetree.with_constraint =
  function
  | From.Parsetree.Pwith_type (x0,x1) ->
      To.Parsetree.Pwith_type
        ((copy_loc copy_longident x0),
          (copy_type_declaration x1))
  | From.Parsetree.Pwith_module (x0,x1) ->
      To.Parsetree.Pwith_module
        ((copy_loc copy_longident x0),
          (copy_loc copy_longident x1))
  | From.Parsetree.Pwith_typesubst x0 ->
      To.Parsetree.Pwith_typesubst
        (copy_type_declaration x0)
  | From.Parsetree.Pwith_modsubst (x0,x1) ->
      To.Parsetree.Pwith_modsubst
        ((copy_loc (fun x  -> x) x0),
          (copy_loc copy_longident x1))

and copy_signature :
  From.Parsetree.signature -> To.Parsetree.signature =
  fun x  -> List.map copy_signature_item x

and copy_signature_item :
  From.Parsetree.signature_item -> To.Parsetree.signature_item =
  fun
    { From.Parsetree.psig_desc = psig_desc;
      From.Parsetree.psig_loc = psig_loc }
     ->
    {
      To.Parsetree.psig_desc =
        (copy_signature_item_desc psig_desc);
      To.Parsetree.psig_loc = (copy_location psig_loc)
    }

and copy_signature_item_desc :
  From.Parsetree.signature_item_desc ->
    To.Parsetree.signature_item_desc
  =
  function
  | From.Parsetree.Psig_value x0 ->
      To.Parsetree.Psig_value
        (copy_value_description x0)
  | From.Parsetree.Psig_type (x0,x1) ->
      To.Parsetree.Psig_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Psig_typext x0 ->
      To.Parsetree.Psig_typext
        (copy_type_extension x0)
  | From.Parsetree.Psig_exception x0 ->
      To.Parsetree.Psig_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Psig_module x0 ->
      To.Parsetree.Psig_module
        (copy_module_declaration x0)
  | From.Parsetree.Psig_recmodule x0 ->
      To.Parsetree.Psig_recmodule
        (List.map copy_module_declaration x0)
  | From.Parsetree.Psig_modtype x0 ->
      To.Parsetree.Psig_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Psig_open x0 ->
      To.Parsetree.Psig_open
        (copy_open_description x0)
  | From.Parsetree.Psig_include x0 ->
      To.Parsetree.Psig_include
        (copy_include_description x0)
  | From.Parsetree.Psig_class x0 ->
      To.Parsetree.Psig_class
        (List.map copy_class_description x0)
  | From.Parsetree.Psig_class_type x0 ->
      To.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Psig_attribute x0 ->
      To.Parsetree.Psig_attribute (copy_attribute x0)
  | From.Parsetree.Psig_extension (x0,x1) ->
      To.Parsetree.Psig_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_class_type_declaration :
  From.Parsetree.class_type_declaration ->
    To.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_class_infos copy_class_type x

and copy_class_description :
  From.Parsetree.class_description -> To.Parsetree.class_description
  =
  fun x  ->
    copy_class_infos copy_class_type x

and copy_class_type :
  From.Parsetree.class_type -> To.Parsetree.class_type =
  fun
    { From.Parsetree.pcty_desc = pcty_desc;
      From.Parsetree.pcty_loc = pcty_loc;
      From.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      To.Parsetree.pcty_desc =
        (copy_class_type_desc pcty_desc);
      To.Parsetree.pcty_loc = (copy_location pcty_loc);
      To.Parsetree.pcty_attributes =
        (copy_attributes pcty_attributes)
    }

and copy_class_type_desc :
  From.Parsetree.class_type_desc -> To.Parsetree.class_type_desc =
  function
  | From.Parsetree.Pcty_constr (x0,x1) ->
      To.Parsetree.Pcty_constr
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcty_signature x0 ->
      To.Parsetree.Pcty_signature
        (copy_class_signature x0)
  | From.Parsetree.Pcty_arrow (x0,x1,x2) ->
      To.Parsetree.Pcty_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_class_type x2))
  | From.Parsetree.Pcty_extension x0 ->
      To.Parsetree.Pcty_extension (copy_extension x0)

and copy_class_signature :
  From.Parsetree.class_signature -> To.Parsetree.class_signature =
  fun
    { From.Parsetree.pcsig_self = pcsig_self;
      From.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      To.Parsetree.pcsig_self =
        (copy_core_type pcsig_self);
      To.Parsetree.pcsig_fields =
        (List.map copy_class_type_field pcsig_fields)
    }

and copy_class_type_field :
  From.Parsetree.class_type_field -> To.Parsetree.class_type_field =
  fun
    { From.Parsetree.pctf_desc = pctf_desc;
      From.Parsetree.pctf_loc = pctf_loc;
      From.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      To.Parsetree.pctf_desc =
        (copy_class_type_field_desc pctf_desc);
      To.Parsetree.pctf_loc = (copy_location pctf_loc);
      To.Parsetree.pctf_attributes =
        (copy_attributes pctf_attributes)
    }

and copy_class_type_field_desc :
  From.Parsetree.class_type_field_desc ->
    To.Parsetree.class_type_field_desc
  =
  function
  | From.Parsetree.Pctf_inherit x0 ->
      To.Parsetree.Pctf_inherit (copy_class_type x0)
  | From.Parsetree.Pctf_val x0 ->
      To.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (noloc x0, (copy_mutable_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_method x0 ->
      To.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (noloc x0, (copy_private_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_constraint x0 ->
      To.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pctf_attribute x0 ->
      To.Parsetree.Pctf_attribute (copy_attribute x0)
  | From.Parsetree.Pctf_extension x0 ->
      To.Parsetree.Pctf_extension (copy_extension x0)

and copy_extension :
  From.Parsetree.extension -> To.Parsetree.extension =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.class_infos -> 'g0 To.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pci_virt = pci_virt;
        From.Parsetree.pci_params = pci_params;
        From.Parsetree.pci_name = pci_name;
        From.Parsetree.pci_expr = pci_expr;
        From.Parsetree.pci_loc = pci_loc;
        From.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        To.Parsetree.pci_virt =
          (copy_virtual_flag pci_virt);
        To.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_core_type x0),
                  (copy_variance x1))) pci_params);
        To.Parsetree.pci_name =
          (copy_loc (fun x  -> x) pci_name);
        To.Parsetree.pci_expr = (f0 pci_expr);
        To.Parsetree.pci_loc = (copy_location pci_loc);
        To.Parsetree.pci_attributes =
          (copy_attributes pci_attributes)
      }

and copy_virtual_flag :
  From.Asttypes.virtual_flag -> To.Asttypes.virtual_flag =
  function
  | From.Asttypes.Virtual  -> To.Asttypes.Virtual
  | From.Asttypes.Concrete  -> To.Asttypes.Concrete

and copy_include_description :
  From.Parsetree.include_description ->
    To.Parsetree.include_description
  =
  fun x  ->
    copy_include_infos copy_module_type x

and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.include_infos ->
        'g0 To.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pincl_mod = pincl_mod;
        From.Parsetree.pincl_loc = pincl_loc;
        From.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        To.Parsetree.pincl_mod = (f0 pincl_mod);
        To.Parsetree.pincl_loc = (copy_location pincl_loc);
        To.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }

and copy_open_description :
  From.Parsetree.open_description -> To.Parsetree.open_description =
  fun
    { From.Parsetree.popen_lid = popen_lid;
      From.Parsetree.popen_override = popen_override;
      From.Parsetree.popen_loc = popen_loc;
      From.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      To.Parsetree.popen_lid =
        (copy_loc copy_longident popen_lid);
      To.Parsetree.popen_override =
        (copy_override_flag popen_override);
      To.Parsetree.popen_loc = (copy_location popen_loc);
      To.Parsetree.popen_attributes =
        (copy_attributes popen_attributes)
    }

and copy_override_flag :
  From.Asttypes.override_flag -> To.Asttypes.override_flag =
  function
  | From.Asttypes.Override  -> To.Asttypes.Override
  | From.Asttypes.Fresh  -> To.Asttypes.Fresh

and copy_module_type_declaration :
  From.Parsetree.module_type_declaration ->
    To.Parsetree.module_type_declaration
  =
  fun
    { From.Parsetree.pmtd_name = pmtd_name;
      From.Parsetree.pmtd_type = pmtd_type;
      From.Parsetree.pmtd_attributes = pmtd_attributes;
      From.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      To.Parsetree.pmtd_name =
        (copy_loc (fun x  -> x) pmtd_name);
      To.Parsetree.pmtd_type =
        (copy_option copy_module_type pmtd_type);
      To.Parsetree.pmtd_attributes =
        (copy_attributes pmtd_attributes);
      To.Parsetree.pmtd_loc = (copy_location pmtd_loc)
    }

and copy_module_declaration :
  From.Parsetree.module_declaration ->
    To.Parsetree.module_declaration
  =
  fun
    { From.Parsetree.pmd_name = pmd_name;
      From.Parsetree.pmd_type = pmd_type;
      From.Parsetree.pmd_attributes = pmd_attributes;
      From.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      To.Parsetree.pmd_name =
        (copy_loc (fun x  -> x) pmd_name);
      To.Parsetree.pmd_type =
        (copy_module_type pmd_type);
      To.Parsetree.pmd_attributes =
        (copy_attributes pmd_attributes);
      To.Parsetree.pmd_loc = (copy_location pmd_loc)
    }

and copy_type_extension :
  From.Parsetree.type_extension -> To.Parsetree.type_extension =
  fun
    { From.Parsetree.ptyext_path = ptyext_path;
      From.Parsetree.ptyext_params = ptyext_params;
      From.Parsetree.ptyext_constructors = ptyext_constructors;
      From.Parsetree.ptyext_private = ptyext_private;
      From.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      To.Parsetree.ptyext_path =
        (copy_loc copy_longident ptyext_path);
      To.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptyext_params);
      To.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor
           ptyext_constructors);
      To.Parsetree.ptyext_private =
        (copy_private_flag ptyext_private);
      To.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }

and copy_extension_constructor :
  From.Parsetree.extension_constructor ->
    To.Parsetree.extension_constructor
  =
  fun
    { From.Parsetree.pext_name = pext_name;
      From.Parsetree.pext_kind = pext_kind;
      From.Parsetree.pext_loc = pext_loc;
      From.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      To.Parsetree.pext_name =
        (copy_loc (fun x  -> x) pext_name);
      To.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      To.Parsetree.pext_loc = (copy_location pext_loc);
      To.Parsetree.pext_attributes =
        (copy_attributes pext_attributes)
    }

and copy_extension_constructor_kind :
  From.Parsetree.extension_constructor_kind ->
    To.Parsetree.extension_constructor_kind
  =
  function
  | From.Parsetree.Pext_decl (x0,x1) ->
      To.Parsetree.Pext_decl
        ((copy_constructor_arguments x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pext_rebind x0 ->
      To.Parsetree.Pext_rebind
        (copy_loc copy_longident x0)

and copy_type_declaration :
  From.Parsetree.type_declaration -> To.Parsetree.type_declaration =
  fun
    { From.Parsetree.ptype_name = ptype_name;
      From.Parsetree.ptype_params = ptype_params;
      From.Parsetree.ptype_cstrs = ptype_cstrs;
      From.Parsetree.ptype_kind = ptype_kind;
      From.Parsetree.ptype_private = ptype_private;
      From.Parsetree.ptype_manifest = ptype_manifest;
      From.Parsetree.ptype_attributes = ptype_attributes;
      From.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      To.Parsetree.ptype_name =
        (copy_loc (fun x  -> x) ptype_name);
      To.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptype_params);
      To.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_core_type x0),
                (copy_core_type x1),
                (copy_location x2))) ptype_cstrs);
      To.Parsetree.ptype_kind =
        (copy_type_kind ptype_kind);
      To.Parsetree.ptype_private =
        (copy_private_flag ptype_private);
      To.Parsetree.ptype_manifest =
        (copy_option copy_core_type ptype_manifest);
      To.Parsetree.ptype_attributes =
        (copy_attributes ptype_attributes);
      To.Parsetree.ptype_loc = (copy_location ptype_loc)
    }

and copy_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_type_kind :
  From.Parsetree.type_kind -> To.Parsetree.type_kind =
  function
  | From.Parsetree.Ptype_abstract  -> To.Parsetree.Ptype_abstract
  | From.Parsetree.Ptype_variant x0 ->
      To.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | From.Parsetree.Ptype_record x0 ->
      To.Parsetree.Ptype_record
        (List.map copy_label_declaration x0)
  | From.Parsetree.Ptype_open  -> To.Parsetree.Ptype_open

and copy_constructor_declaration :
  From.Parsetree.constructor_declaration ->
    To.Parsetree.constructor_declaration
  =
  fun
    { From.Parsetree.pcd_name = pcd_name;
      From.Parsetree.pcd_args = pcd_args;
      From.Parsetree.pcd_res = pcd_res;
      From.Parsetree.pcd_loc = pcd_loc;
      From.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      To.Parsetree.pcd_name =
        (copy_loc (fun x  -> x) pcd_name);
      To.Parsetree.pcd_args =
        (copy_constructor_arguments pcd_args);
      To.Parsetree.pcd_res =
        (copy_option copy_core_type pcd_res);
      To.Parsetree.pcd_loc = (copy_location pcd_loc);
      To.Parsetree.pcd_attributes =
        (copy_attributes pcd_attributes)
    }

and copy_constructor_arguments :
  From.Parsetree.constructor_arguments ->
    To.Parsetree.constructor_arguments
  =
  function
  | From.Parsetree.Pcstr_tuple x0 ->
      To.Parsetree.Pcstr_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Pcstr_record x0 ->
      To.Parsetree.Pcstr_record
        (List.map copy_label_declaration x0)

and copy_label_declaration :
  From.Parsetree.label_declaration -> To.Parsetree.label_declaration
  =
  fun
    { From.Parsetree.pld_name = pld_name;
      From.Parsetree.pld_mutable = pld_mutable;
      From.Parsetree.pld_type = pld_type;
      From.Parsetree.pld_loc = pld_loc;
      From.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      To.Parsetree.pld_name =
        (copy_loc (fun x  -> x) pld_name);
      To.Parsetree.pld_mutable =
        (copy_mutable_flag pld_mutable);
      To.Parsetree.pld_type =
        (copy_core_type pld_type);
      To.Parsetree.pld_loc = (copy_location pld_loc);
      To.Parsetree.pld_attributes =
        (copy_attributes pld_attributes)
    }

and copy_mutable_flag :
  From.Asttypes.mutable_flag -> To.Asttypes.mutable_flag =
  function
  | From.Asttypes.Immutable  -> To.Asttypes.Immutable
  | From.Asttypes.Mutable  -> To.Asttypes.Mutable

and copy_variance :
  From.Asttypes.variance -> To.Asttypes.variance =
  function
  | From.Asttypes.Covariant  -> To.Asttypes.Covariant
  | From.Asttypes.Contravariant  -> To.Asttypes.Contravariant
  | From.Asttypes.Invariant  -> To.Asttypes.Invariant

and copy_value_description :
  From.Parsetree.value_description -> To.Parsetree.value_description
  =
  fun
    { From.Parsetree.pval_name = pval_name;
      From.Parsetree.pval_type = pval_type;
      From.Parsetree.pval_prim = pval_prim;
      From.Parsetree.pval_attributes = pval_attributes;
      From.Parsetree.pval_loc = pval_loc }
     ->
    {
      To.Parsetree.pval_name =
        (copy_loc (fun x  -> x) pval_name);
      To.Parsetree.pval_type =
        (copy_core_type pval_type);
      To.Parsetree.pval_prim = (List.map (fun x  -> x) pval_prim);
      To.Parsetree.pval_attributes =
        (copy_attributes pval_attributes);
      To.Parsetree.pval_loc = (copy_location pval_loc)
    }

and copy_arg_label :
  From.Asttypes.arg_label -> To.Asttypes.arg_label =
  function
  | From.Asttypes.Nolabel  -> To.Asttypes.Nolabel
  | From.Asttypes.Labelled x0 -> To.Asttypes.Labelled x0
  | From.Asttypes.Optional x0 -> To.Asttypes.Optional x0

and copy_closed_flag :
  From.Asttypes.closed_flag -> To.Asttypes.closed_flag =
  function
  | From.Asttypes.Closed  -> To.Asttypes.Closed
  | From.Asttypes.Open  -> To.Asttypes.Open

and copy_label :
  From.Asttypes.label -> To.Asttypes.label = fun x  -> x

and copy_rec_flag :
  From.Asttypes.rec_flag -> To.Asttypes.rec_flag =
  function
  | From.Asttypes.Nonrecursive  -> To.Asttypes.Nonrecursive
  | From.Asttypes.Recursive  -> To.Asttypes.Recursive

and copy_constant :
  From.Parsetree.constant -> To.Parsetree.constant =
  function
  | From.Parsetree.Pconst_integer (x0,x1) ->
      To.Parsetree.Pconst_integer (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_char x0 -> To.Parsetree.Pconst_char x0
  | From.Parsetree.Pconst_string (x0,x1) ->
      To.Parsetree.Pconst_string (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_float (x0,x1) ->
      To.Parsetree.Pconst_float (x0, (copy_option (fun x  -> x) x1))

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_longident : From.Longident.t -> To.Longident.t =
  function
  | From.Longident.Lident x0 -> To.Longident.Lident x0
  | From.Longident.Ldot (x0,x1) ->
      To.Longident.Ldot ((copy_longident x0), x1)
  | From.Longident.Lapply (x0,x1) ->
      To.Longident.Lapply
        ((copy_longident x0), (copy_longident x1))

and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) -> 'f0 From.Asttypes.loc -> 'g0 To.Asttypes.loc
  =
  fun f0  ->
    fun { From.Asttypes.txt = txt; From.Asttypes.loc = loc }  ->
      {
        To.Asttypes.txt = (f0 txt);
        To.Asttypes.loc = (copy_location loc)
      }

and copy_location : From.Location.t -> To.Location.t =
  fun
    { From.Location.loc_start = loc_start;
      From.Location.loc_end = loc_end;
      From.Location.loc_ghost = loc_ghost }
     ->
    {
      To.Location.loc_start = (copy_Lexing_position loc_start);
      To.Location.loc_end = (copy_Lexing_position loc_end);
      To.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }

let rec copy_out_phrase :
  From.Outcometree.out_phrase -> To.Outcometree.out_phrase =
  function
  | From.Outcometree.Ophr_eval (x0,x1) ->
      To.Outcometree.Ophr_eval
        ((copy_out_value x0),
          (copy_out_type x1))
  | From.Outcometree.Ophr_signature x0 ->
      To.Outcometree.Ophr_signature
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_sig_item x0),
                (copy_option copy_out_value x1))) x0)
  | From.Outcometree.Ophr_exception x0 ->
      To.Outcometree.Ophr_exception
        (let (x0,x1) = x0  in
         ((copy_exn x0), (copy_out_value x1)))

and copy_exn : exn -> exn = fun x  -> x

and copy_out_sig_item :
  From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item =
  function
  | From.Outcometree.Osig_class (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_class_type (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class_type
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_typext (x0,x1) ->
      To.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0),
          (copy_out_ext_status x1))
  | From.Outcometree.Osig_modtype (x0,x1) ->
      To.Outcometree.Osig_modtype
        (x0, (copy_out_module_type x1))
  | From.Outcometree.Osig_module (x0,x1,x2) ->
      To.Outcometree.Osig_module
        (x0, (copy_out_module_type x1),
          (copy_out_rec_status x2))
  | From.Outcometree.Osig_type (x0,x1) ->
      To.Outcometree.Osig_type
        ((copy_out_type_decl x0),
          (copy_out_rec_status x1))
  | From.Outcometree.Osig_value x0 ->
      To.Outcometree.Osig_value
        (copy_out_val_decl x0)
  | From.Outcometree.Osig_ellipsis  -> To.Outcometree.Osig_ellipsis

and copy_out_val_decl :
  From.Outcometree.out_val_decl -> To.Outcometree.out_val_decl =
  fun
    { From.Outcometree.oval_name = oval_name;
      From.Outcometree.oval_type = oval_type;
      From.Outcometree.oval_prims = oval_prims;
      From.Outcometree.oval_attributes = oval_attributes }
     ->
    {
      To.Outcometree.oval_name = oval_name;
      To.Outcometree.oval_type =
        (copy_out_type oval_type);
      To.Outcometree.oval_prims = (List.map (fun x  -> x) oval_prims);
      To.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }

and copy_out_type_decl :
  From.Outcometree.out_type_decl -> To.Outcometree.out_type_decl =
  fun
    { From.Outcometree.otype_name = otype_name;
      From.Outcometree.otype_params = otype_params;
      From.Outcometree.otype_type = otype_type;
      From.Outcometree.otype_private = otype_private;
      From.Outcometree.otype_immediate = otype_immediate;
      From.Outcometree.otype_unboxed = otype_unboxed;
      From.Outcometree.otype_cstrs = otype_cstrs }
     ->
    {
      To.Outcometree.otype_name = otype_name;
      To.Outcometree.otype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
           otype_params);
      To.Outcometree.otype_type =
        (copy_out_type otype_type);
      To.Outcometree.otype_private =
        (copy_From_Asttypes_private_flag otype_private);
      To.Outcometree.otype_immediate = (copy_bool otype_immediate);
      To.Outcometree.otype_unboxed = (copy_bool otype_unboxed);
      To.Outcometree.otype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_type x0),
                (copy_out_type x1))) otype_cstrs)
    }

and copy_out_module_type :
  From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  =
  function
  | From.Outcometree.Omty_abstract  -> To.Outcometree.Omty_abstract
  | From.Outcometree.Omty_functor (x0,x1,x2) ->
      To.Outcometree.Omty_functor
        (x0, (copy_option copy_out_module_type x1),
          (copy_out_module_type x2))
  | From.Outcometree.Omty_ident x0 ->
      To.Outcometree.Omty_ident (copy_out_ident x0)
  | From.Outcometree.Omty_signature x0 ->
      To.Outcometree.Omty_signature
        (List.map copy_out_sig_item x0)
  | From.Outcometree.Omty_alias x0 ->
      To.Outcometree.Omty_alias (copy_out_ident x0)

and copy_out_ext_status :
  From.Outcometree.out_ext_status -> To.Outcometree.out_ext_status =
  function
  | From.Outcometree.Oext_first  -> To.Outcometree.Oext_first
  | From.Outcometree.Oext_next  -> To.Outcometree.Oext_next
  | From.Outcometree.Oext_exception  -> To.Outcometree.Oext_exception

and copy_out_extension_constructor :
  From.Outcometree.out_extension_constructor ->
    To.Outcometree.out_extension_constructor
  =
  fun
    { From.Outcometree.oext_name = oext_name;
      From.Outcometree.oext_type_name = oext_type_name;
      From.Outcometree.oext_type_params = oext_type_params;
      From.Outcometree.oext_args = oext_args;
      From.Outcometree.oext_ret_type = oext_ret_type;
      From.Outcometree.oext_private = oext_private }
     ->
    {
      To.Outcometree.oext_name = oext_name;
      To.Outcometree.oext_type_name = oext_type_name;
      To.Outcometree.oext_type_params =
        (List.map (fun x  -> x) oext_type_params);
      To.Outcometree.oext_args =
        (List.map copy_out_type oext_args);
      To.Outcometree.oext_ret_type =
        (copy_option copy_out_type oext_ret_type);
      To.Outcometree.oext_private =
        (copy_From_Asttypes_private_flag oext_private)
    }

and copy_From_Asttypes_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_out_rec_status :
  From.Outcometree.out_rec_status -> To.Outcometree.out_rec_status =
  function
  | From.Outcometree.Orec_not  -> To.Outcometree.Orec_not
  | From.Outcometree.Orec_first  -> To.Outcometree.Orec_first
  | From.Outcometree.Orec_next  -> To.Outcometree.Orec_next

and copy_out_class_type :
  From.Outcometree.out_class_type -> To.Outcometree.out_class_type =
  function
  | From.Outcometree.Octy_constr (x0,x1) ->
      To.Outcometree.Octy_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Octy_arrow (x0,x1,x2) ->
      To.Outcometree.Octy_arrow
        (x0, (copy_out_type x1),
          (copy_out_class_type x2))
  | From.Outcometree.Octy_signature (x0,x1) ->
      To.Outcometree.Octy_signature
        ((copy_option copy_out_type x0),
          (List.map copy_out_class_sig_item x1))

and copy_out_class_sig_item :
  From.Outcometree.out_class_sig_item ->
    To.Outcometree.out_class_sig_item
  =
  function
  | From.Outcometree.Ocsg_constraint (x0,x1) ->
      To.Outcometree.Ocsg_constraint
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Ocsg_method (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_method
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))
  | From.Outcometree.Ocsg_value (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_value
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))

and copy_out_type :
  From.Outcometree.out_type -> To.Outcometree.out_type =
  function
  | From.Outcometree.Otyp_abstract  -> To.Outcometree.Otyp_abstract
  | From.Outcometree.Otyp_open  -> To.Outcometree.Otyp_open
  | From.Outcometree.Otyp_alias (x0,x1) ->
      To.Outcometree.Otyp_alias
        ((copy_out_type x0), x1)
  | From.Outcometree.Otyp_arrow (x0,x1,x2) ->
      To.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1),
          (copy_out_type x2))
  | From.Outcometree.Otyp_class (x0,x1,x2) ->
      To.Outcometree.Otyp_class
        ((copy_bool x0), (copy_out_ident x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_constr (x0,x1) ->
      To.Outcometree.Otyp_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Otyp_manifest (x0,x1) ->
      To.Outcometree.Otyp_manifest
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Otyp_object (x0,x1) ->
      To.Outcometree.Otyp_object
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               (x0, (copy_out_type x1))) x0),
          (copy_option copy_bool x1))
  | From.Outcometree.Otyp_record x0 ->
      To.Outcometree.Otyp_record
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1), (copy_out_type x2)))
           x0)
  | From.Outcometree.Otyp_stuff x0 -> To.Outcometree.Otyp_stuff x0
  | From.Outcometree.Otyp_sum x0 ->
      To.Outcometree.Otyp_sum
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2))) x0)
  | From.Outcometree.Otyp_tuple x0 ->
      To.Outcometree.Otyp_tuple
        (List.map copy_out_type x0)
  | From.Outcometree.Otyp_var (x0,x1) ->
      To.Outcometree.Otyp_var ((copy_bool x0), x1)
  | From.Outcometree.Otyp_variant (x0,x1,x2,x3) ->
      To.Outcometree.Otyp_variant
        ((copy_bool x0), (copy_out_variant x1),
          (copy_bool x2),
          (copy_option (fun x  -> List.map (fun x  -> x) x) x3))
  | From.Outcometree.Otyp_poly (x0,x1) ->
      To.Outcometree.Otyp_poly
        ((List.map (fun x  -> x) x0), (copy_out_type x1))
  | From.Outcometree.Otyp_module (x0,x1,x2) ->
      To.Outcometree.Otyp_module
        (x0, (List.map (fun x  -> x) x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_attribute (x0,x1) ->
      To.Outcometree.Otyp_attribute
        ((copy_out_type x0),
          (copy_out_attribute x1))

and copy_out_attribute :
  From.Outcometree.out_attribute -> To.Outcometree.out_attribute =
  fun { From.Outcometree.oattr_name = oattr_name }  ->
    { To.Outcometree.oattr_name = oattr_name }

and copy_out_variant :
  From.Outcometree.out_variant -> To.Outcometree.out_variant =
  function
  | From.Outcometree.Ovar_fields x0 ->
      To.Outcometree.Ovar_fields
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1),
                (List.map copy_out_type x2))) x0)
  | From.Outcometree.Ovar_name (x0,x1) ->
      To.Outcometree.Ovar_typ
        (To.Outcometree.Otyp_constr
           ((copy_out_ident x0),
            (List.map copy_out_type x1)))

and copy_out_value :
  From.Outcometree.out_value -> To.Outcometree.out_value =
  function
  | From.Outcometree.Oval_array x0 ->
      To.Outcometree.Oval_array
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_char x0 -> To.Outcometree.Oval_char x0
  | From.Outcometree.Oval_constr (x0,x1) ->
      To.Outcometree.Oval_constr
        ((copy_out_ident x0),
          (List.map copy_out_value x1))
  | From.Outcometree.Oval_ellipsis  -> To.Outcometree.Oval_ellipsis
  | From.Outcometree.Oval_float x0 ->
      To.Outcometree.Oval_float (copy_float x0)
  | From.Outcometree.Oval_int x0 -> To.Outcometree.Oval_int x0
  | From.Outcometree.Oval_int32 x0 -> To.Outcometree.Oval_int32 x0
  | From.Outcometree.Oval_int64 x0 -> To.Outcometree.Oval_int64 x0
  | From.Outcometree.Oval_nativeint x0 ->
      To.Outcometree.Oval_nativeint x0
  | From.Outcometree.Oval_list x0 ->
      To.Outcometree.Oval_list
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_printer x0 ->
      To.Outcometree.Oval_printer x0
  | From.Outcometree.Oval_record x0 ->
      To.Outcometree.Oval_record
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_ident x0),
                (copy_out_value x1))) x0)
  | From.Outcometree.Oval_string x0 -> To.Outcometree.Oval_string x0
  | From.Outcometree.Oval_stuff x0 -> To.Outcometree.Oval_stuff x0
  | From.Outcometree.Oval_tuple x0 ->
      To.Outcometree.Oval_tuple
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_variant (x0,x1) ->
      To.Outcometree.Oval_variant
        (x0, (copy_option copy_out_value x1))

and copy_float : float -> float = fun x  -> x

and copy_out_ident :
  From.Outcometree.out_ident -> To.Outcometree.out_ident =
  function
  | From.Outcometree.Oide_apply (x0,x1) ->
      To.Outcometree.Oide_apply
        ((copy_out_ident x0),
          (copy_out_ident x1))
  | From.Outcometree.Oide_dot (x0,x1) ->
      To.Outcometree.Oide_dot
        ((copy_out_ident x0), x1)
  | From.Outcometree.Oide_ident x0 -> To.Outcometree.Oide_ident x0

let rec copy_toplevel_phrase :
  From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase =
  function
  | From.Parsetree.Ptop_def x0 ->
      To.Parsetree.Ptop_def (copy_structure x0)
  | From.Parsetree.Ptop_dir (x0,x1) ->
      To.Parsetree.Ptop_dir
        (x0, (copy_directive_argument x1))

and copy_directive_argument :
  From.Parsetree.directive_argument -> To.Parsetree.directive_argument =
  function
  | From.Parsetree.Pdir_none  -> To.Parsetree.Pdir_none
  | From.Parsetree.Pdir_string x0 -> To.Parsetree.Pdir_string x0
  | From.Parsetree.Pdir_int (x0,x1) ->
      To.Parsetree.Pdir_int (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pdir_ident x0 ->
      To.Parsetree.Pdir_ident (copy_longident x0)
  | From.Parsetree.Pdir_bool x0 ->
      To.Parsetree.Pdir_bool (copy_bool x0)

let copy_out_type_extension :
  From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension =
  fun
    { From.Outcometree.otyext_name = otyext_name;
      From.Outcometree.otyext_params = otyext_params;
      From.Outcometree.otyext_constructors = otyext_constructors;
      From.Outcometree.otyext_private = otyext_private }
     ->
    {
      To.Outcometree.otyext_name = otyext_name;
      To.Outcometree.otyext_params =
        (List.map (fun x  -> x) otyext_params);
      To.Outcometree.otyext_constructors =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2)))
           otyext_constructors);
      To.Outcometree.otyext_private =
        (copy_private_flag otyext_private)
    }

let copy_cases x = List.map copy_case x
let copy_pat = copy_pattern
let copy_expr = copy_expression
let copy_typ = copy_core_type

end
module Migrate_parsetree_405_404_migrate
= struct
#1 "migrate_parsetree_405_404_migrate.ml"
# 1 "src/migrate_parsetree_405_404_migrate.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module From = Ast_405
module To = Ast_404

let rec copy_expression :
  From.Parsetree.expression -> To.Parsetree.expression =
  fun
    { From.Parsetree.pexp_desc = pexp_desc;
      From.Parsetree.pexp_loc = pexp_loc;
      From.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      To.Parsetree.pexp_desc =
        (copy_expression_desc pexp_desc);
      To.Parsetree.pexp_loc = (copy_location pexp_loc);
      To.Parsetree.pexp_attributes =
        (copy_attributes pexp_attributes)
    }

and copy_expression_desc :
  From.Parsetree.expression_desc -> To.Parsetree.expression_desc =
  function
  | From.Parsetree.Pexp_ident x0 ->
      To.Parsetree.Pexp_ident
        (copy_loc copy_longident x0)
  | From.Parsetree.Pexp_constant x0 ->
      To.Parsetree.Pexp_constant (copy_constant x0)
  | From.Parsetree.Pexp_let (x0,x1,x2) ->
      To.Parsetree.Pexp_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_function x0 ->
      To.Parsetree.Pexp_function
        (List.map copy_case x0)
  | From.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pexp_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_expression x3))
  | From.Parsetree.Pexp_apply (x0,x1) ->
      To.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pexp_match (x0,x1) ->
      To.Parsetree.Pexp_match
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_try (x0,x1) ->
      To.Parsetree.Pexp_try
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_tuple x0 ->
      To.Parsetree.Pexp_tuple
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_construct (x0,x1) ->
      To.Parsetree.Pexp_construct
        ((copy_loc copy_longident x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_variant (x0,x1) ->
      To.Parsetree.Pexp_variant
        ((copy_label x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_record (x0,x1) ->
      To.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc copy_longident x0),
                 (copy_expression x1))) x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_field (x0,x1) ->
      To.Parsetree.Pexp_field
        ((copy_expression x0),
          (copy_loc copy_longident x1))
  | From.Parsetree.Pexp_setfield (x0,x1,x2) ->
      To.Parsetree.Pexp_setfield
        ((copy_expression x0),
          (copy_loc copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_array x0 ->
      To.Parsetree.Pexp_array
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      To.Parsetree.Pexp_ifthenelse
        ((copy_expression x0),
          (copy_expression x1),
          (copy_option copy_expression x2))
  | From.Parsetree.Pexp_sequence (x0,x1) ->
      To.Parsetree.Pexp_sequence
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_while (x0,x1) ->
      To.Parsetree.Pexp_while
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      To.Parsetree.Pexp_for
        ((copy_pattern x0),
          (copy_expression x1),
          (copy_expression x2),
          (copy_direction_flag x3),
          (copy_expression x4))
  | From.Parsetree.Pexp_constraint (x0,x1) ->
      To.Parsetree.Pexp_constraint
        ((copy_expression x0),
          (copy_core_type x1))
  | From.Parsetree.Pexp_coerce (x0,x1,x2) ->
      To.Parsetree.Pexp_coerce
        ((copy_expression x0),
          (copy_option copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Pexp_send (x0,x1) ->
      To.Parsetree.Pexp_send
        ((copy_expression x0), x1.From.Asttypes.txt)
  | From.Parsetree.Pexp_new x0 ->
      To.Parsetree.Pexp_new
        (copy_loc copy_longident x0)
  | From.Parsetree.Pexp_setinstvar (x0,x1) ->
      To.Parsetree.Pexp_setinstvar
        ((copy_loc (fun x  -> x) x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_override x0 ->
      To.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_loc (fun x  -> x) x0),
                (copy_expression x1))) x0)
  | From.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      To.Parsetree.Pexp_letmodule
        ((copy_loc (fun x  -> x) x0),
          (copy_module_expr x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_letexception (x0,x1) ->
      To.Parsetree.Pexp_letexception
        ((copy_extension_constructor x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_assert x0 ->
      To.Parsetree.Pexp_assert (copy_expression x0)
  | From.Parsetree.Pexp_lazy x0 ->
      To.Parsetree.Pexp_lazy (copy_expression x0)
  | From.Parsetree.Pexp_poly (x0,x1) ->
      To.Parsetree.Pexp_poly
        ((copy_expression x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pexp_object x0 ->
      To.Parsetree.Pexp_object
        (copy_class_structure x0)
  | From.Parsetree.Pexp_newtype (x0,x1) ->
      To.Parsetree.Pexp_newtype
        (x0.From.Asttypes.txt, (copy_expression x1))
  | From.Parsetree.Pexp_pack x0 ->
      To.Parsetree.Pexp_pack (copy_module_expr x0)
  | From.Parsetree.Pexp_open (x0,x1,x2) ->
      To.Parsetree.Pexp_open
        ((copy_override_flag x0),
          (copy_loc copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_extension x0 ->
      To.Parsetree.Pexp_extension (copy_extension x0)
  | From.Parsetree.Pexp_unreachable  -> To.Parsetree.Pexp_unreachable

and copy_direction_flag :
  From.Asttypes.direction_flag -> To.Asttypes.direction_flag =
  function
  | From.Asttypes.Upto  -> To.Asttypes.Upto
  | From.Asttypes.Downto  -> To.Asttypes.Downto

and copy_case :
  From.Parsetree.case -> To.Parsetree.case =
  fun
    { From.Parsetree.pc_lhs = pc_lhs;
      From.Parsetree.pc_guard = pc_guard;
      From.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      To.Parsetree.pc_lhs = (copy_pattern pc_lhs);
      To.Parsetree.pc_guard =
        (copy_option copy_expression pc_guard);
      To.Parsetree.pc_rhs = (copy_expression pc_rhs)
    }

and copy_value_binding :
  From.Parsetree.value_binding -> To.Parsetree.value_binding =
  fun
    { From.Parsetree.pvb_pat = pvb_pat;
      From.Parsetree.pvb_expr = pvb_expr;
      From.Parsetree.pvb_attributes = pvb_attributes;
      From.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      To.Parsetree.pvb_pat = (copy_pattern pvb_pat);
      To.Parsetree.pvb_expr =
        (copy_expression pvb_expr);
      To.Parsetree.pvb_attributes =
        (copy_attributes pvb_attributes);
      To.Parsetree.pvb_loc = (copy_location pvb_loc)
    }

and copy_pattern :
  From.Parsetree.pattern -> To.Parsetree.pattern =
  fun
    { From.Parsetree.ppat_desc = ppat_desc;
      From.Parsetree.ppat_loc = ppat_loc;
      From.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      To.Parsetree.ppat_desc =
        (copy_pattern_desc ppat_desc);
      To.Parsetree.ppat_loc = (copy_location ppat_loc);
      To.Parsetree.ppat_attributes =
        (copy_attributes ppat_attributes)
    }

and copy_pattern_desc :
  From.Parsetree.pattern_desc -> To.Parsetree.pattern_desc =
  function
  | From.Parsetree.Ppat_any  -> To.Parsetree.Ppat_any
  | From.Parsetree.Ppat_var x0 ->
      To.Parsetree.Ppat_var (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_alias (x0,x1) ->
      To.Parsetree.Ppat_alias
        ((copy_pattern x0),
          (copy_loc (fun x  -> x) x1))
  | From.Parsetree.Ppat_constant x0 ->
      To.Parsetree.Ppat_constant (copy_constant x0)
  | From.Parsetree.Ppat_interval (x0,x1) ->
      To.Parsetree.Ppat_interval
        ((copy_constant x0),
          (copy_constant x1))
  | From.Parsetree.Ppat_tuple x0 ->
      To.Parsetree.Ppat_tuple
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_construct (x0,x1) ->
      To.Parsetree.Ppat_construct
        ((copy_loc copy_longident x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_variant (x0,x1) ->
      To.Parsetree.Ppat_variant
        ((copy_label x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_record (x0,x1) ->
      To.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc copy_longident x0),
                 (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ppat_array x0 ->
      To.Parsetree.Ppat_array
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_or (x0,x1) ->
      To.Parsetree.Ppat_or
        ((copy_pattern x0),
          (copy_pattern x1))
  | From.Parsetree.Ppat_constraint (x0,x1) ->
      To.Parsetree.Ppat_constraint
        ((copy_pattern x0),
          (copy_core_type x1))
  | From.Parsetree.Ppat_type x0 ->
      To.Parsetree.Ppat_type
        (copy_loc copy_longident x0)
  | From.Parsetree.Ppat_lazy x0 ->
      To.Parsetree.Ppat_lazy (copy_pattern x0)
  | From.Parsetree.Ppat_unpack x0 ->
      To.Parsetree.Ppat_unpack
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_exception x0 ->
      To.Parsetree.Ppat_exception (copy_pattern x0)
  | From.Parsetree.Ppat_extension x0 ->
      To.Parsetree.Ppat_extension (copy_extension x0)
  | From.Parsetree.Ppat_open (x0,x1) ->
      To.Parsetree.Ppat_open
        ((copy_loc copy_longident x0),
          (copy_pattern x1))

and copy_core_type :
  From.Parsetree.core_type -> To.Parsetree.core_type =
  fun
    { From.Parsetree.ptyp_desc = ptyp_desc;
      From.Parsetree.ptyp_loc = ptyp_loc;
      From.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      To.Parsetree.ptyp_desc =
        (copy_core_type_desc ptyp_desc);
      To.Parsetree.ptyp_loc = (copy_location ptyp_loc);
      To.Parsetree.ptyp_attributes =
        (copy_attributes ptyp_attributes)
    }

and copy_core_type_desc :
  From.Parsetree.core_type_desc -> To.Parsetree.core_type_desc =
  function
  | From.Parsetree.Ptyp_any  -> To.Parsetree.Ptyp_any
  | From.Parsetree.Ptyp_var x0 -> To.Parsetree.Ptyp_var x0
  | From.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      To.Parsetree.Ptyp_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Ptyp_tuple x0 ->
      To.Parsetree.Ptyp_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Ptyp_constr (x0,x1) ->
      To.Parsetree.Ptyp_constr
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_object (x0,x1) ->
      To.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (x0.From.Asttypes.txt, (copy_attributes x1),
                 (copy_core_type x2))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ptyp_class (x0,x1) ->
      To.Parsetree.Ptyp_class
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_alias (x0,x1) ->
      To.Parsetree.Ptyp_alias
        ((copy_core_type x0), x1)
  | From.Parsetree.Ptyp_variant (x0,x1,x2) ->
      To.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0),
          (copy_closed_flag x1),
          (copy_option (fun x  -> List.map copy_label x) x2))
  | From.Parsetree.Ptyp_poly (x0,x1) ->
      To.Parsetree.Ptyp_poly
        ((List.map (fun x  -> x.From.Asttypes.txt) x0), (copy_core_type x1))
  | From.Parsetree.Ptyp_package x0 ->
      To.Parsetree.Ptyp_package (copy_package_type x0)
  | From.Parsetree.Ptyp_extension x0 ->
      To.Parsetree.Ptyp_extension (copy_extension x0)

and copy_package_type :
  From.Parsetree.package_type -> To.Parsetree.package_type =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc copy_longident x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_loc copy_longident x0),
              (copy_core_type x1))) x1))

and copy_row_field :
  From.Parsetree.row_field -> To.Parsetree.row_field =
  function
  | From.Parsetree.Rtag (x0,x1,x2,x3) ->
      To.Parsetree.Rtag
        ((copy_label x0),
          (copy_attributes x1), (copy_bool x2),
          (List.map copy_core_type x3))
  | From.Parsetree.Rinherit x0 ->
      To.Parsetree.Rinherit (copy_core_type x0)

and copy_attributes :
  From.Parsetree.attributes -> To.Parsetree.attributes =
  fun x  -> List.map copy_attribute x

and copy_attribute :
  From.Parsetree.attribute -> To.Parsetree.attribute =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_payload :
  From.Parsetree.payload -> To.Parsetree.payload =
  function
  | From.Parsetree.PStr x0 ->
      To.Parsetree.PStr (copy_structure x0)
  | From.Parsetree.PSig x0 ->
      To.Parsetree.PSig (copy_signature x0)
  | From.Parsetree.PTyp x0 ->
      To.Parsetree.PTyp (copy_core_type x0)
  | From.Parsetree.PPat (x0,x1) ->
      To.Parsetree.PPat
        ((copy_pattern x0),
          (copy_option copy_expression x1))

and copy_structure :
  From.Parsetree.structure -> To.Parsetree.structure =
  fun x  -> List.map copy_structure_item x

and copy_structure_item :
  From.Parsetree.structure_item -> To.Parsetree.structure_item =
  fun
    { From.Parsetree.pstr_desc = pstr_desc;
      From.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      To.Parsetree.pstr_desc =
        (copy_structure_item_desc pstr_desc);
      To.Parsetree.pstr_loc = (copy_location pstr_loc)
    }

and copy_structure_item_desc :
  From.Parsetree.structure_item_desc ->
    To.Parsetree.structure_item_desc
  =
  function
  | From.Parsetree.Pstr_eval (x0,x1) ->
      To.Parsetree.Pstr_eval
        ((copy_expression x0),
          (copy_attributes x1))
  | From.Parsetree.Pstr_value (x0,x1) ->
      To.Parsetree.Pstr_value
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1))
  | From.Parsetree.Pstr_primitive x0 ->
      To.Parsetree.Pstr_primitive
        (copy_value_description x0)
  | From.Parsetree.Pstr_type (x0,x1) ->
      To.Parsetree.Pstr_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Pstr_typext x0 ->
      To.Parsetree.Pstr_typext
        (copy_type_extension x0)
  | From.Parsetree.Pstr_exception x0 ->
      To.Parsetree.Pstr_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Pstr_module x0 ->
      To.Parsetree.Pstr_module
        (copy_module_binding x0)
  | From.Parsetree.Pstr_recmodule x0 ->
      To.Parsetree.Pstr_recmodule
        (List.map copy_module_binding x0)
  | From.Parsetree.Pstr_modtype x0 ->
      To.Parsetree.Pstr_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Pstr_open x0 ->
      To.Parsetree.Pstr_open
        (copy_open_description x0)
  | From.Parsetree.Pstr_class x0 ->
      To.Parsetree.Pstr_class
        (List.map copy_class_declaration x0)
  | From.Parsetree.Pstr_class_type x0 ->
      To.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Pstr_include x0 ->
      To.Parsetree.Pstr_include
        (copy_include_declaration x0)
  | From.Parsetree.Pstr_attribute x0 ->
      To.Parsetree.Pstr_attribute (copy_attribute x0)
  | From.Parsetree.Pstr_extension (x0,x1) ->
      To.Parsetree.Pstr_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_include_declaration :
  From.Parsetree.include_declaration ->
    To.Parsetree.include_declaration
  =
  fun x  ->
    copy_include_infos copy_module_expr x

and copy_class_declaration :
  From.Parsetree.class_declaration -> To.Parsetree.class_declaration
  =
  fun x  ->
    copy_class_infos copy_class_expr x

and copy_class_expr :
  From.Parsetree.class_expr -> To.Parsetree.class_expr =
  fun
    { From.Parsetree.pcl_desc = pcl_desc;
      From.Parsetree.pcl_loc = pcl_loc;
      From.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      To.Parsetree.pcl_desc =
        (copy_class_expr_desc pcl_desc);
      To.Parsetree.pcl_loc = (copy_location pcl_loc);
      To.Parsetree.pcl_attributes =
        (copy_attributes pcl_attributes)
    }

and copy_class_expr_desc :
  From.Parsetree.class_expr_desc -> To.Parsetree.class_expr_desc =
  function
  | From.Parsetree.Pcl_constr (x0,x1) ->
      To.Parsetree.Pcl_constr
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcl_structure x0 ->
      To.Parsetree.Pcl_structure
        (copy_class_structure x0)
  | From.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pcl_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_class_expr x3))
  | From.Parsetree.Pcl_apply (x0,x1) ->
      To.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pcl_let (x0,x1,x2) ->
      To.Parsetree.Pcl_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | From.Parsetree.Pcl_constraint (x0,x1) ->
      To.Parsetree.Pcl_constraint
        ((copy_class_expr x0),
          (copy_class_type x1))
  | From.Parsetree.Pcl_extension x0 ->
      To.Parsetree.Pcl_extension (copy_extension x0)

and copy_class_structure :
  From.Parsetree.class_structure -> To.Parsetree.class_structure =
  fun
    { From.Parsetree.pcstr_self = pcstr_self;
      From.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      To.Parsetree.pcstr_self =
        (copy_pattern pcstr_self);
      To.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }

and copy_class_field :
  From.Parsetree.class_field -> To.Parsetree.class_field =
  fun
    { From.Parsetree.pcf_desc = pcf_desc;
      From.Parsetree.pcf_loc = pcf_loc;
      From.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      To.Parsetree.pcf_desc =
        (copy_class_field_desc pcf_desc);
      To.Parsetree.pcf_loc = (copy_location pcf_loc);
      To.Parsetree.pcf_attributes =
        (copy_attributes pcf_attributes)
    }

and copy_class_field_desc :
  From.Parsetree.class_field_desc -> To.Parsetree.class_field_desc =
  function
  | From.Parsetree.Pcf_inherit (x0,x1,x2) ->
      To.Parsetree.Pcf_inherit
        ((copy_override_flag x0),
          (copy_class_expr x1),
          (copy_option (fun x  -> x.From.Asttypes.txt) x2))
  | From.Parsetree.Pcf_val x0 ->
      To.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_method x0 ->
      To.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_constraint x0 ->
      To.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pcf_initializer x0 ->
      To.Parsetree.Pcf_initializer
        (copy_expression x0)
  | From.Parsetree.Pcf_attribute x0 ->
      To.Parsetree.Pcf_attribute (copy_attribute x0)
  | From.Parsetree.Pcf_extension x0 ->
      To.Parsetree.Pcf_extension (copy_extension x0)

and copy_class_field_kind :
  From.Parsetree.class_field_kind -> To.Parsetree.class_field_kind =
  function
  | From.Parsetree.Cfk_virtual x0 ->
      To.Parsetree.Cfk_virtual (copy_core_type x0)
  | From.Parsetree.Cfk_concrete (x0,x1) ->
      To.Parsetree.Cfk_concrete
        ((copy_override_flag x0),
          (copy_expression x1))

and copy_module_binding :
  From.Parsetree.module_binding -> To.Parsetree.module_binding =
  fun
    { From.Parsetree.pmb_name = pmb_name;
      From.Parsetree.pmb_expr = pmb_expr;
      From.Parsetree.pmb_attributes = pmb_attributes;
      From.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      To.Parsetree.pmb_name =
        (copy_loc (fun x  -> x) pmb_name);
      To.Parsetree.pmb_expr =
        (copy_module_expr pmb_expr);
      To.Parsetree.pmb_attributes =
        (copy_attributes pmb_attributes);
      To.Parsetree.pmb_loc = (copy_location pmb_loc)
    }

and copy_module_expr :
  From.Parsetree.module_expr -> To.Parsetree.module_expr =
  fun
    { From.Parsetree.pmod_desc = pmod_desc;
      From.Parsetree.pmod_loc = pmod_loc;
      From.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      To.Parsetree.pmod_desc =
        (copy_module_expr_desc pmod_desc);
      To.Parsetree.pmod_loc = (copy_location pmod_loc);
      To.Parsetree.pmod_attributes =
        (copy_attributes pmod_attributes)
    }

and copy_module_expr_desc :
  From.Parsetree.module_expr_desc -> To.Parsetree.module_expr_desc =
  function
  | From.Parsetree.Pmod_ident x0 ->
      To.Parsetree.Pmod_ident
        (copy_loc copy_longident x0)
  | From.Parsetree.Pmod_structure x0 ->
      To.Parsetree.Pmod_structure (copy_structure x0)
  | From.Parsetree.Pmod_functor (x0,x1,x2) ->
      To.Parsetree.Pmod_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_expr x2))
  | From.Parsetree.Pmod_apply (x0,x1) ->
      To.Parsetree.Pmod_apply
        ((copy_module_expr x0),
          (copy_module_expr x1))
  | From.Parsetree.Pmod_constraint (x0,x1) ->
      To.Parsetree.Pmod_constraint
        ((copy_module_expr x0),
          (copy_module_type x1))
  | From.Parsetree.Pmod_unpack x0 ->
      To.Parsetree.Pmod_unpack (copy_expression x0)
  | From.Parsetree.Pmod_extension x0 ->
      To.Parsetree.Pmod_extension (copy_extension x0)

and copy_module_type :
  From.Parsetree.module_type -> To.Parsetree.module_type =
  fun
    { From.Parsetree.pmty_desc = pmty_desc;
      From.Parsetree.pmty_loc = pmty_loc;
      From.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      To.Parsetree.pmty_desc =
        (copy_module_type_desc pmty_desc);
      To.Parsetree.pmty_loc = (copy_location pmty_loc);
      To.Parsetree.pmty_attributes =
        (copy_attributes pmty_attributes)
    }

and copy_module_type_desc :
  From.Parsetree.module_type_desc -> To.Parsetree.module_type_desc =
  function
  | From.Parsetree.Pmty_ident x0 ->
      To.Parsetree.Pmty_ident
        (copy_loc copy_longident x0)
  | From.Parsetree.Pmty_signature x0 ->
      To.Parsetree.Pmty_signature (copy_signature x0)
  | From.Parsetree.Pmty_functor (x0,x1,x2) ->
      To.Parsetree.Pmty_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_type x2))
  | From.Parsetree.Pmty_with (x0,x1) ->
      To.Parsetree.Pmty_with
        ((copy_module_type x0),
          (List.map copy_with_constraint x1))
  | From.Parsetree.Pmty_typeof x0 ->
      To.Parsetree.Pmty_typeof (copy_module_expr x0)
  | From.Parsetree.Pmty_extension x0 ->
      To.Parsetree.Pmty_extension (copy_extension x0)
  | From.Parsetree.Pmty_alias x0 ->
      To.Parsetree.Pmty_alias
        (copy_loc copy_longident x0)

and copy_with_constraint :
  From.Parsetree.with_constraint -> To.Parsetree.with_constraint =
  function
  | From.Parsetree.Pwith_type (x0,x1) ->
      To.Parsetree.Pwith_type
        ((copy_loc copy_longident x0),
          (copy_type_declaration x1))
  | From.Parsetree.Pwith_module (x0,x1) ->
      To.Parsetree.Pwith_module
        ((copy_loc copy_longident x0),
          (copy_loc copy_longident x1))
  | From.Parsetree.Pwith_typesubst x0 ->
      To.Parsetree.Pwith_typesubst
        (copy_type_declaration x0)
  | From.Parsetree.Pwith_modsubst (x0,x1) ->
      To.Parsetree.Pwith_modsubst
        ((copy_loc (fun x  -> x) x0),
          (copy_loc copy_longident x1))

and copy_signature :
  From.Parsetree.signature -> To.Parsetree.signature =
  fun x  -> List.map copy_signature_item x

and copy_signature_item :
  From.Parsetree.signature_item -> To.Parsetree.signature_item =
  fun
    { From.Parsetree.psig_desc = psig_desc;
      From.Parsetree.psig_loc = psig_loc }
     ->
    {
      To.Parsetree.psig_desc =
        (copy_signature_item_desc psig_desc);
      To.Parsetree.psig_loc = (copy_location psig_loc)
    }

and copy_signature_item_desc :
  From.Parsetree.signature_item_desc ->
    To.Parsetree.signature_item_desc
  =
  function
  | From.Parsetree.Psig_value x0 ->
      To.Parsetree.Psig_value
        (copy_value_description x0)
  | From.Parsetree.Psig_type (x0,x1) ->
      To.Parsetree.Psig_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Psig_typext x0 ->
      To.Parsetree.Psig_typext
        (copy_type_extension x0)
  | From.Parsetree.Psig_exception x0 ->
      To.Parsetree.Psig_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Psig_module x0 ->
      To.Parsetree.Psig_module
        (copy_module_declaration x0)
  | From.Parsetree.Psig_recmodule x0 ->
      To.Parsetree.Psig_recmodule
        (List.map copy_module_declaration x0)
  | From.Parsetree.Psig_modtype x0 ->
      To.Parsetree.Psig_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Psig_open x0 ->
      To.Parsetree.Psig_open
        (copy_open_description x0)
  | From.Parsetree.Psig_include x0 ->
      To.Parsetree.Psig_include
        (copy_include_description x0)
  | From.Parsetree.Psig_class x0 ->
      To.Parsetree.Psig_class
        (List.map copy_class_description x0)
  | From.Parsetree.Psig_class_type x0 ->
      To.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Psig_attribute x0 ->
      To.Parsetree.Psig_attribute (copy_attribute x0)
  | From.Parsetree.Psig_extension (x0,x1) ->
      To.Parsetree.Psig_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_class_type_declaration :
  From.Parsetree.class_type_declaration ->
    To.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_class_infos copy_class_type x

and copy_class_description :
  From.Parsetree.class_description -> To.Parsetree.class_description
  =
  fun x  ->
    copy_class_infos copy_class_type x

and copy_class_type :
  From.Parsetree.class_type -> To.Parsetree.class_type =
  fun
    { From.Parsetree.pcty_desc = pcty_desc;
      From.Parsetree.pcty_loc = pcty_loc;
      From.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      To.Parsetree.pcty_desc =
        (copy_class_type_desc pcty_desc);
      To.Parsetree.pcty_loc = (copy_location pcty_loc);
      To.Parsetree.pcty_attributes =
        (copy_attributes pcty_attributes)
    }

and copy_class_type_desc :
  From.Parsetree.class_type_desc -> To.Parsetree.class_type_desc =
  function
  | From.Parsetree.Pcty_constr (x0,x1) ->
      To.Parsetree.Pcty_constr
        ((copy_loc copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcty_signature x0 ->
      To.Parsetree.Pcty_signature
        (copy_class_signature x0)
  | From.Parsetree.Pcty_arrow (x0,x1,x2) ->
      To.Parsetree.Pcty_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_class_type x2))
  | From.Parsetree.Pcty_extension x0 ->
      To.Parsetree.Pcty_extension (copy_extension x0)

and copy_class_signature :
  From.Parsetree.class_signature -> To.Parsetree.class_signature =
  fun
    { From.Parsetree.pcsig_self = pcsig_self;
      From.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      To.Parsetree.pcsig_self =
        (copy_core_type pcsig_self);
      To.Parsetree.pcsig_fields =
        (List.map copy_class_type_field pcsig_fields)
    }

and copy_class_type_field :
  From.Parsetree.class_type_field -> To.Parsetree.class_type_field =
  fun
    { From.Parsetree.pctf_desc = pctf_desc;
      From.Parsetree.pctf_loc = pctf_loc;
      From.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      To.Parsetree.pctf_desc =
        (copy_class_type_field_desc pctf_desc);
      To.Parsetree.pctf_loc = (copy_location pctf_loc);
      To.Parsetree.pctf_attributes =
        (copy_attributes pctf_attributes)
    }

and copy_class_type_field_desc :
  From.Parsetree.class_type_field_desc ->
    To.Parsetree.class_type_field_desc
  =
  function
  | From.Parsetree.Pctf_inherit x0 ->
      To.Parsetree.Pctf_inherit (copy_class_type x0)
  | From.Parsetree.Pctf_val x0 ->
      To.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (x0.From.Asttypes.txt, (copy_mutable_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_method x0 ->
      To.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (x0.From.Asttypes.txt, (copy_private_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_constraint x0 ->
      To.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pctf_attribute x0 ->
      To.Parsetree.Pctf_attribute (copy_attribute x0)
  | From.Parsetree.Pctf_extension x0 ->
      To.Parsetree.Pctf_extension (copy_extension x0)

and copy_extension :
  From.Parsetree.extension -> To.Parsetree.extension =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.class_infos -> 'g0 To.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pci_virt = pci_virt;
        From.Parsetree.pci_params = pci_params;
        From.Parsetree.pci_name = pci_name;
        From.Parsetree.pci_expr = pci_expr;
        From.Parsetree.pci_loc = pci_loc;
        From.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        To.Parsetree.pci_virt =
          (copy_virtual_flag pci_virt);
        To.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_core_type x0),
                  (copy_variance x1))) pci_params);
        To.Parsetree.pci_name =
          (copy_loc (fun x  -> x) pci_name);
        To.Parsetree.pci_expr = (f0 pci_expr);
        To.Parsetree.pci_loc = (copy_location pci_loc);
        To.Parsetree.pci_attributes =
          (copy_attributes pci_attributes)
      }

and copy_virtual_flag :
  From.Asttypes.virtual_flag -> To.Asttypes.virtual_flag =
  function
  | From.Asttypes.Virtual  -> To.Asttypes.Virtual
  | From.Asttypes.Concrete  -> To.Asttypes.Concrete

and copy_include_description :
  From.Parsetree.include_description ->
    To.Parsetree.include_description
  =
  fun x  ->
    copy_include_infos copy_module_type x

and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.include_infos ->
        'g0 To.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pincl_mod = pincl_mod;
        From.Parsetree.pincl_loc = pincl_loc;
        From.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        To.Parsetree.pincl_mod = (f0 pincl_mod);
        To.Parsetree.pincl_loc = (copy_location pincl_loc);
        To.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }

and copy_open_description :
  From.Parsetree.open_description -> To.Parsetree.open_description =
  fun
    { From.Parsetree.popen_lid = popen_lid;
      From.Parsetree.popen_override = popen_override;
      From.Parsetree.popen_loc = popen_loc;
      From.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      To.Parsetree.popen_lid =
        (copy_loc copy_longident popen_lid);
      To.Parsetree.popen_override =
        (copy_override_flag popen_override);
      To.Parsetree.popen_loc = (copy_location popen_loc);
      To.Parsetree.popen_attributes =
        (copy_attributes popen_attributes)
    }

and copy_override_flag :
  From.Asttypes.override_flag -> To.Asttypes.override_flag =
  function
  | From.Asttypes.Override  -> To.Asttypes.Override
  | From.Asttypes.Fresh  -> To.Asttypes.Fresh

and copy_module_type_declaration :
  From.Parsetree.module_type_declaration ->
    To.Parsetree.module_type_declaration
  =
  fun
    { From.Parsetree.pmtd_name = pmtd_name;
      From.Parsetree.pmtd_type = pmtd_type;
      From.Parsetree.pmtd_attributes = pmtd_attributes;
      From.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      To.Parsetree.pmtd_name =
        (copy_loc (fun x  -> x) pmtd_name);
      To.Parsetree.pmtd_type =
        (copy_option copy_module_type pmtd_type);
      To.Parsetree.pmtd_attributes =
        (copy_attributes pmtd_attributes);
      To.Parsetree.pmtd_loc = (copy_location pmtd_loc)
    }

and copy_module_declaration :
  From.Parsetree.module_declaration ->
    To.Parsetree.module_declaration
  =
  fun
    { From.Parsetree.pmd_name = pmd_name;
      From.Parsetree.pmd_type = pmd_type;
      From.Parsetree.pmd_attributes = pmd_attributes;
      From.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      To.Parsetree.pmd_name =
        (copy_loc (fun x  -> x) pmd_name);
      To.Parsetree.pmd_type =
        (copy_module_type pmd_type);
      To.Parsetree.pmd_attributes =
        (copy_attributes pmd_attributes);
      To.Parsetree.pmd_loc = (copy_location pmd_loc)
    }

and copy_type_extension :
  From.Parsetree.type_extension -> To.Parsetree.type_extension =
  fun
    { From.Parsetree.ptyext_path = ptyext_path;
      From.Parsetree.ptyext_params = ptyext_params;
      From.Parsetree.ptyext_constructors = ptyext_constructors;
      From.Parsetree.ptyext_private = ptyext_private;
      From.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      To.Parsetree.ptyext_path =
        (copy_loc copy_longident ptyext_path);
      To.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptyext_params);
      To.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor
           ptyext_constructors);
      To.Parsetree.ptyext_private =
        (copy_private_flag ptyext_private);
      To.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }

and copy_extension_constructor :
  From.Parsetree.extension_constructor ->
    To.Parsetree.extension_constructor
  =
  fun
    { From.Parsetree.pext_name = pext_name;
      From.Parsetree.pext_kind = pext_kind;
      From.Parsetree.pext_loc = pext_loc;
      From.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      To.Parsetree.pext_name =
        (copy_loc (fun x  -> x) pext_name);
      To.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      To.Parsetree.pext_loc = (copy_location pext_loc);
      To.Parsetree.pext_attributes =
        (copy_attributes pext_attributes)
    }

and copy_extension_constructor_kind :
  From.Parsetree.extension_constructor_kind ->
    To.Parsetree.extension_constructor_kind
  =
  function
  | From.Parsetree.Pext_decl (x0,x1) ->
      To.Parsetree.Pext_decl
        ((copy_constructor_arguments x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pext_rebind x0 ->
      To.Parsetree.Pext_rebind
        (copy_loc copy_longident x0)

and copy_type_declaration :
  From.Parsetree.type_declaration -> To.Parsetree.type_declaration =
  fun
    { From.Parsetree.ptype_name = ptype_name;
      From.Parsetree.ptype_params = ptype_params;
      From.Parsetree.ptype_cstrs = ptype_cstrs;
      From.Parsetree.ptype_kind = ptype_kind;
      From.Parsetree.ptype_private = ptype_private;
      From.Parsetree.ptype_manifest = ptype_manifest;
      From.Parsetree.ptype_attributes = ptype_attributes;
      From.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      To.Parsetree.ptype_name =
        (copy_loc (fun x  -> x) ptype_name);
      To.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptype_params);
      To.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_core_type x0),
                (copy_core_type x1),
                (copy_location x2))) ptype_cstrs);
      To.Parsetree.ptype_kind =
        (copy_type_kind ptype_kind);
      To.Parsetree.ptype_private =
        (copy_private_flag ptype_private);
      To.Parsetree.ptype_manifest =
        (copy_option copy_core_type ptype_manifest);
      To.Parsetree.ptype_attributes =
        (copy_attributes ptype_attributes);
      To.Parsetree.ptype_loc = (copy_location ptype_loc)
    }

and copy_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_type_kind :
  From.Parsetree.type_kind -> To.Parsetree.type_kind =
  function
  | From.Parsetree.Ptype_abstract  -> To.Parsetree.Ptype_abstract
  | From.Parsetree.Ptype_variant x0 ->
      To.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | From.Parsetree.Ptype_record x0 ->
      To.Parsetree.Ptype_record
        (List.map copy_label_declaration x0)
  | From.Parsetree.Ptype_open  -> To.Parsetree.Ptype_open

and copy_constructor_declaration :
  From.Parsetree.constructor_declaration ->
    To.Parsetree.constructor_declaration
  =
  fun
    { From.Parsetree.pcd_name = pcd_name;
      From.Parsetree.pcd_args = pcd_args;
      From.Parsetree.pcd_res = pcd_res;
      From.Parsetree.pcd_loc = pcd_loc;
      From.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      To.Parsetree.pcd_name =
        (copy_loc (fun x  -> x) pcd_name);
      To.Parsetree.pcd_args =
        (copy_constructor_arguments pcd_args);
      To.Parsetree.pcd_res =
        (copy_option copy_core_type pcd_res);
      To.Parsetree.pcd_loc = (copy_location pcd_loc);
      To.Parsetree.pcd_attributes =
        (copy_attributes pcd_attributes)
    }

and copy_constructor_arguments :
  From.Parsetree.constructor_arguments ->
    To.Parsetree.constructor_arguments
  =
  function
  | From.Parsetree.Pcstr_tuple x0 ->
      To.Parsetree.Pcstr_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Pcstr_record x0 ->
      To.Parsetree.Pcstr_record
        (List.map copy_label_declaration x0)

and copy_label_declaration :
  From.Parsetree.label_declaration -> To.Parsetree.label_declaration
  =
  fun
    { From.Parsetree.pld_name = pld_name;
      From.Parsetree.pld_mutable = pld_mutable;
      From.Parsetree.pld_type = pld_type;
      From.Parsetree.pld_loc = pld_loc;
      From.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      To.Parsetree.pld_name =
        (copy_loc (fun x  -> x) pld_name);
      To.Parsetree.pld_mutable =
        (copy_mutable_flag pld_mutable);
      To.Parsetree.pld_type =
        (copy_core_type pld_type);
      To.Parsetree.pld_loc = (copy_location pld_loc);
      To.Parsetree.pld_attributes =
        (copy_attributes pld_attributes)
    }

and copy_mutable_flag :
  From.Asttypes.mutable_flag -> To.Asttypes.mutable_flag =
  function
  | From.Asttypes.Immutable  -> To.Asttypes.Immutable
  | From.Asttypes.Mutable  -> To.Asttypes.Mutable

and copy_variance :
  From.Asttypes.variance -> To.Asttypes.variance =
  function
  | From.Asttypes.Covariant  -> To.Asttypes.Covariant
  | From.Asttypes.Contravariant  -> To.Asttypes.Contravariant
  | From.Asttypes.Invariant  -> To.Asttypes.Invariant

and copy_value_description :
  From.Parsetree.value_description -> To.Parsetree.value_description
  =
  fun
    { From.Parsetree.pval_name = pval_name;
      From.Parsetree.pval_type = pval_type;
      From.Parsetree.pval_prim = pval_prim;
      From.Parsetree.pval_attributes = pval_attributes;
      From.Parsetree.pval_loc = pval_loc }
     ->
    {
      To.Parsetree.pval_name =
        (copy_loc (fun x  -> x) pval_name);
      To.Parsetree.pval_type =
        (copy_core_type pval_type);
      To.Parsetree.pval_prim = (List.map (fun x  -> x) pval_prim);
      To.Parsetree.pval_attributes =
        (copy_attributes pval_attributes);
      To.Parsetree.pval_loc = (copy_location pval_loc)
    }

and copy_arg_label :
  From.Asttypes.arg_label -> To.Asttypes.arg_label =
  function
  | From.Asttypes.Nolabel  -> To.Asttypes.Nolabel
  | From.Asttypes.Labelled x0 -> To.Asttypes.Labelled x0
  | From.Asttypes.Optional x0 -> To.Asttypes.Optional x0

and copy_closed_flag :
  From.Asttypes.closed_flag -> To.Asttypes.closed_flag =
  function
  | From.Asttypes.Closed  -> To.Asttypes.Closed
  | From.Asttypes.Open  -> To.Asttypes.Open

and copy_label :
  From.Asttypes.label -> To.Asttypes.label = fun x  -> x

and copy_rec_flag :
  From.Asttypes.rec_flag -> To.Asttypes.rec_flag =
  function
  | From.Asttypes.Nonrecursive  -> To.Asttypes.Nonrecursive
  | From.Asttypes.Recursive  -> To.Asttypes.Recursive

and copy_constant :
  From.Parsetree.constant -> To.Parsetree.constant =
  function
  | From.Parsetree.Pconst_integer (x0,x1) ->
      To.Parsetree.Pconst_integer (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_char x0 -> To.Parsetree.Pconst_char x0
  | From.Parsetree.Pconst_string (x0,x1) ->
      To.Parsetree.Pconst_string (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_float (x0,x1) ->
      To.Parsetree.Pconst_float (x0, (copy_option (fun x  -> x) x1))

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_longident : From.Longident.t -> To.Longident.t =
  function
  | From.Longident.Lident x0 -> To.Longident.Lident x0
  | From.Longident.Ldot (x0,x1) ->
      To.Longident.Ldot ((copy_longident x0), x1)
  | From.Longident.Lapply (x0,x1) ->
      To.Longident.Lapply
        ((copy_longident x0), (copy_longident x1))

and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) -> 'f0 From.Asttypes.loc -> 'g0 To.Asttypes.loc
  =
  fun f0  ->
    fun { From.Asttypes.txt = txt; From.Asttypes.loc = loc }  ->
      {
        To.Asttypes.txt = (f0 txt);
        To.Asttypes.loc = (copy_location loc)
      }

and copy_location : From.Location.t -> To.Location.t =
  fun
    { From.Location.loc_start = loc_start;
      From.Location.loc_end = loc_end;
      From.Location.loc_ghost = loc_ghost }
     ->
    {
      To.Location.loc_start = (copy_Lexing_position loc_start);
      To.Location.loc_end = (copy_Lexing_position loc_end);
      To.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }

let rec copy_out_phrase :
  From.Outcometree.out_phrase -> To.Outcometree.out_phrase =
  function
  | From.Outcometree.Ophr_eval (x0,x1) ->
      To.Outcometree.Ophr_eval
        ((copy_out_value x0),
          (copy_out_type x1))
  | From.Outcometree.Ophr_signature x0 ->
      To.Outcometree.Ophr_signature
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_sig_item x0),
                (copy_option copy_out_value x1))) x0)
  | From.Outcometree.Ophr_exception x0 ->
      To.Outcometree.Ophr_exception
        (let (x0,x1) = x0  in
         ((copy_exn x0), (copy_out_value x1)))

and copy_exn : exn -> exn = fun x  -> x

and copy_out_sig_item :
  From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item =
  function
  | From.Outcometree.Osig_class (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_class_type (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class_type
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_typext (x0,x1) ->
      To.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0),
          (copy_out_ext_status x1))
  | From.Outcometree.Osig_modtype (x0,x1) ->
      To.Outcometree.Osig_modtype
        (x0, (copy_out_module_type x1))
  | From.Outcometree.Osig_module (x0,x1,x2) ->
      To.Outcometree.Osig_module
        (x0, (copy_out_module_type x1),
          (copy_out_rec_status x2))
  | From.Outcometree.Osig_type (x0,x1) ->
      To.Outcometree.Osig_type
        ((copy_out_type_decl x0),
          (copy_out_rec_status x1))
  | From.Outcometree.Osig_value x0 ->
      To.Outcometree.Osig_value
        (copy_out_val_decl x0)
  | From.Outcometree.Osig_ellipsis  -> To.Outcometree.Osig_ellipsis

and copy_out_val_decl :
  From.Outcometree.out_val_decl -> To.Outcometree.out_val_decl =
  fun
    { From.Outcometree.oval_name = oval_name;
      From.Outcometree.oval_type = oval_type;
      From.Outcometree.oval_prims = oval_prims;
      From.Outcometree.oval_attributes = oval_attributes }
     ->
    {
      To.Outcometree.oval_name = oval_name;
      To.Outcometree.oval_type =
        (copy_out_type oval_type);
      To.Outcometree.oval_prims = (List.map (fun x  -> x) oval_prims);
      To.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }

and copy_out_type_decl :
  From.Outcometree.out_type_decl -> To.Outcometree.out_type_decl =
  fun
    { From.Outcometree.otype_name = otype_name;
      From.Outcometree.otype_params = otype_params;
      From.Outcometree.otype_type = otype_type;
      From.Outcometree.otype_private = otype_private;
      From.Outcometree.otype_immediate = otype_immediate;
      From.Outcometree.otype_unboxed = otype_unboxed;
      From.Outcometree.otype_cstrs = otype_cstrs }
     ->
    {
      To.Outcometree.otype_name = otype_name;
      To.Outcometree.otype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
           otype_params);
      To.Outcometree.otype_type =
        (copy_out_type otype_type);
      To.Outcometree.otype_private =
        (copy_From_Asttypes_private_flag otype_private);
      To.Outcometree.otype_immediate = (copy_bool otype_immediate);
      To.Outcometree.otype_unboxed = (copy_bool otype_unboxed);
      To.Outcometree.otype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_type x0),
                (copy_out_type x1))) otype_cstrs)
    }

and copy_out_module_type :
  From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  =
  function
  | From.Outcometree.Omty_abstract  -> To.Outcometree.Omty_abstract
  | From.Outcometree.Omty_functor (x0,x1,x2) ->
      To.Outcometree.Omty_functor
        (x0, (copy_option copy_out_module_type x1),
          (copy_out_module_type x2))
  | From.Outcometree.Omty_ident x0 ->
      To.Outcometree.Omty_ident (copy_out_ident x0)
  | From.Outcometree.Omty_signature x0 ->
      To.Outcometree.Omty_signature
        (List.map copy_out_sig_item x0)
  | From.Outcometree.Omty_alias x0 ->
      To.Outcometree.Omty_alias (copy_out_ident x0)

and copy_out_ext_status :
  From.Outcometree.out_ext_status -> To.Outcometree.out_ext_status =
  function
  | From.Outcometree.Oext_first  -> To.Outcometree.Oext_first
  | From.Outcometree.Oext_next  -> To.Outcometree.Oext_next
  | From.Outcometree.Oext_exception  -> To.Outcometree.Oext_exception

and copy_out_extension_constructor :
  From.Outcometree.out_extension_constructor ->
    To.Outcometree.out_extension_constructor
  =
  fun
    { From.Outcometree.oext_name = oext_name;
      From.Outcometree.oext_type_name = oext_type_name;
      From.Outcometree.oext_type_params = oext_type_params;
      From.Outcometree.oext_args = oext_args;
      From.Outcometree.oext_ret_type = oext_ret_type;
      From.Outcometree.oext_private = oext_private }
     ->
    {
      To.Outcometree.oext_name = oext_name;
      To.Outcometree.oext_type_name = oext_type_name;
      To.Outcometree.oext_type_params =
        (List.map (fun x  -> x) oext_type_params);
      To.Outcometree.oext_args =
        (List.map copy_out_type oext_args);
      To.Outcometree.oext_ret_type =
        (copy_option copy_out_type oext_ret_type);
      To.Outcometree.oext_private =
        (copy_From_Asttypes_private_flag oext_private)
    }

and copy_From_Asttypes_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_out_rec_status :
  From.Outcometree.out_rec_status -> To.Outcometree.out_rec_status =
  function
  | From.Outcometree.Orec_not  -> To.Outcometree.Orec_not
  | From.Outcometree.Orec_first  -> To.Outcometree.Orec_first
  | From.Outcometree.Orec_next  -> To.Outcometree.Orec_next

and copy_out_class_type :
  From.Outcometree.out_class_type -> To.Outcometree.out_class_type =
  function
  | From.Outcometree.Octy_constr (x0,x1) ->
      To.Outcometree.Octy_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Octy_arrow (x0,x1,x2) ->
      To.Outcometree.Octy_arrow
        (x0, (copy_out_type x1),
          (copy_out_class_type x2))
  | From.Outcometree.Octy_signature (x0,x1) ->
      To.Outcometree.Octy_signature
        ((copy_option copy_out_type x0),
          (List.map copy_out_class_sig_item x1))

and copy_out_class_sig_item :
  From.Outcometree.out_class_sig_item ->
    To.Outcometree.out_class_sig_item
  =
  function
  | From.Outcometree.Ocsg_constraint (x0,x1) ->
      To.Outcometree.Ocsg_constraint
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Ocsg_method (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_method
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))
  | From.Outcometree.Ocsg_value (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_value
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))

and copy_out_type :
  From.Outcometree.out_type -> To.Outcometree.out_type =
  function
  | From.Outcometree.Otyp_abstract  -> To.Outcometree.Otyp_abstract
  | From.Outcometree.Otyp_open  -> To.Outcometree.Otyp_open
  | From.Outcometree.Otyp_alias (x0,x1) ->
      To.Outcometree.Otyp_alias
        ((copy_out_type x0), x1)
  | From.Outcometree.Otyp_arrow (x0,x1,x2) ->
      To.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1),
          (copy_out_type x2))
  | From.Outcometree.Otyp_class (x0,x1,x2) ->
      To.Outcometree.Otyp_class
        ((copy_bool x0), (copy_out_ident x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_constr (x0,x1) ->
      To.Outcometree.Otyp_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Otyp_manifest (x0,x1) ->
      To.Outcometree.Otyp_manifest
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Otyp_object (x0,x1) ->
      To.Outcometree.Otyp_object
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               (x0, (copy_out_type x1))) x0),
          (copy_option copy_bool x1))
  | From.Outcometree.Otyp_record x0 ->
      To.Outcometree.Otyp_record
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1), (copy_out_type x2)))
           x0)
  | From.Outcometree.Otyp_stuff x0 -> To.Outcometree.Otyp_stuff x0
  | From.Outcometree.Otyp_sum x0 ->
      To.Outcometree.Otyp_sum
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2))) x0)
  | From.Outcometree.Otyp_tuple x0 ->
      To.Outcometree.Otyp_tuple
        (List.map copy_out_type x0)
  | From.Outcometree.Otyp_var (x0,x1) ->
      To.Outcometree.Otyp_var ((copy_bool x0), x1)
  | From.Outcometree.Otyp_variant (x0,x1,x2,x3) ->
      To.Outcometree.Otyp_variant
        ((copy_bool x0), (copy_out_variant x1),
          (copy_bool x2),
          (copy_option (fun x  -> List.map (fun x  -> x) x) x3))
  | From.Outcometree.Otyp_poly (x0,x1) ->
      To.Outcometree.Otyp_poly
        ((List.map (fun x  -> x) x0), (copy_out_type x1))
  | From.Outcometree.Otyp_module (x0,x1,x2) ->
      To.Outcometree.Otyp_module
        (x0, (List.map (fun x  -> x) x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_attribute (x0,x1) ->
      To.Outcometree.Otyp_attribute
        ((copy_out_type x0),
          (copy_out_attribute x1))

and copy_out_attribute :
  From.Outcometree.out_attribute -> To.Outcometree.out_attribute =
  fun { From.Outcometree.oattr_name = oattr_name }  ->
    { To.Outcometree.oattr_name = oattr_name }

and copy_out_variant :
  From.Outcometree.out_variant -> To.Outcometree.out_variant =
  function
  | From.Outcometree.Ovar_fields x0 ->
      To.Outcometree.Ovar_fields
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1),
                (List.map copy_out_type x2))) x0)
  | From.Outcometree.Ovar_typ (From.Outcometree.Otyp_constr (id,tyl)) ->
      To.Outcometree.Ovar_name (copy_out_ident id, List.map copy_out_type tyl)
  | From.Outcometree.Ovar_typ x0 ->
      To.Outcometree.Ovar_name
        (To.Outcometree.Oide_ident "", [copy_out_type x0])

and copy_out_value :
  From.Outcometree.out_value -> To.Outcometree.out_value =
  function
  | From.Outcometree.Oval_array x0 ->
      To.Outcometree.Oval_array
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_char x0 -> To.Outcometree.Oval_char x0
  | From.Outcometree.Oval_constr (x0,x1) ->
      To.Outcometree.Oval_constr
        ((copy_out_ident x0),
          (List.map copy_out_value x1))
  | From.Outcometree.Oval_ellipsis  -> To.Outcometree.Oval_ellipsis
  | From.Outcometree.Oval_float x0 ->
      To.Outcometree.Oval_float (copy_float x0)
  | From.Outcometree.Oval_int x0 -> To.Outcometree.Oval_int x0
  | From.Outcometree.Oval_int32 x0 -> To.Outcometree.Oval_int32 x0
  | From.Outcometree.Oval_int64 x0 -> To.Outcometree.Oval_int64 x0
  | From.Outcometree.Oval_nativeint x0 ->
      To.Outcometree.Oval_nativeint x0
  | From.Outcometree.Oval_list x0 ->
      To.Outcometree.Oval_list
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_printer x0 ->
      To.Outcometree.Oval_printer x0
  | From.Outcometree.Oval_record x0 ->
      To.Outcometree.Oval_record
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_ident x0),
                (copy_out_value x1))) x0)
  | From.Outcometree.Oval_string x0 -> To.Outcometree.Oval_string x0
  | From.Outcometree.Oval_stuff x0 -> To.Outcometree.Oval_stuff x0
  | From.Outcometree.Oval_tuple x0 ->
      To.Outcometree.Oval_tuple
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_variant (x0,x1) ->
      To.Outcometree.Oval_variant
        (x0, (copy_option copy_out_value x1))

and copy_float : float -> float = fun x  -> x

and copy_out_ident :
  From.Outcometree.out_ident -> To.Outcometree.out_ident =
  function
  | From.Outcometree.Oide_apply (x0,x1) ->
      To.Outcometree.Oide_apply
        ((copy_out_ident x0),
          (copy_out_ident x1))
  | From.Outcometree.Oide_dot (x0,x1) ->
      To.Outcometree.Oide_dot
        ((copy_out_ident x0), x1)
  | From.Outcometree.Oide_ident x0 -> To.Outcometree.Oide_ident x0

let rec copy_toplevel_phrase :
  From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase =
  function
  | From.Parsetree.Ptop_def x0 ->
      To.Parsetree.Ptop_def (copy_structure x0)
  | From.Parsetree.Ptop_dir (x0,x1) ->
      To.Parsetree.Ptop_dir
        (x0, (copy_directive_argument x1))

and copy_directive_argument :
  From.Parsetree.directive_argument -> To.Parsetree.directive_argument =
  function
  | From.Parsetree.Pdir_none  -> To.Parsetree.Pdir_none
  | From.Parsetree.Pdir_string x0 -> To.Parsetree.Pdir_string x0
  | From.Parsetree.Pdir_int (x0,x1) ->
      To.Parsetree.Pdir_int (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pdir_ident x0 ->
      To.Parsetree.Pdir_ident (copy_longident x0)
  | From.Parsetree.Pdir_bool x0 ->
      To.Parsetree.Pdir_bool (copy_bool x0)

let copy_out_type_extension :
  From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension =
  fun
    { From.Outcometree.otyext_name = otyext_name;
      From.Outcometree.otyext_params = otyext_params;
      From.Outcometree.otyext_constructors = otyext_constructors;
      From.Outcometree.otyext_private = otyext_private }
     ->
    {
      To.Outcometree.otyext_name = otyext_name;
      To.Outcometree.otyext_params =
        (List.map (fun x  -> x) otyext_params);
      To.Outcometree.otyext_constructors =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2)))
           otyext_constructors);
      To.Outcometree.otyext_private =
        (copy_private_flag otyext_private)
    }

let copy_cases x = List.map copy_case x
let copy_pat = copy_pattern
let copy_expr = copy_expression
let copy_typ = copy_core_type

end
module Migrate_parsetree_404_405
= struct
#1 "migrate_parsetree_404_405.ml"
# 1 "src/migrate_parsetree_404_405.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Migrate_parsetree_404_405_migrate

(*$ open Printf
    let fields = [
      "attribute"; "attributes"; "case"; "cases"; "class_declaration";
      "class_description"; "class_expr"; "class_field"; "class_signature";
      "class_structure"; "class_type"; "class_type_declaration";
      "class_type_field"; "constructor_declaration"; "expr"; "extension";
      "extension_constructor"; "include_declaration"; "include_description";
      "label_declaration"; "location"; "module_binding"; "module_declaration";
      "module_expr"; "module_type"; "module_type_declaration";
      "open_description"; "pat"; "signature"; "signature_item"; "structure";
      "structure_item"; "typ"; "type_declaration"; "type_extension";
      "type_kind"; "value_binding"; "value_description";
      "with_constraint"; "payload"
    ]
  let foreach_field f =
    printf "\n";
    List.iter f fields
*)(*$*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$ foreach_field (printf "%s;\n")*)
     attribute;
     attributes;
     case;
     cases;
     class_declaration;
     class_description;
     class_expr;
     class_field;
     class_signature;
     class_structure;
     class_type;
     class_type_declaration;
     class_type_field;
     constructor_declaration;
     expr;
     extension;
     extension_constructor;
     include_declaration;
     include_description;
     label_declaration;
     location;
     module_binding;
     module_declaration;
     module_expr;
     module_type;
     module_type_declaration;
     open_description;
     pat;
     signature;
     signature_item;
     structure;
     structure_item;
     typ;
     type_declaration;
     type_extension;
     type_kind;
     value_binding;
     value_description;
     with_constraint;
     payload;
     (*$*)
   } as mapper) ->
  let module R = Migrate_parsetree_405_404_migrate in
  {
    To.Ast_mapper.
    (*$ foreach_field (fun s ->
          printf
          "%s = (fun _ x -> copy_%s (%s mapper (R.copy_%s x)));\n" s s s s)
    *)
    attribute = (fun _ x -> copy_attribute (attribute mapper (R.copy_attribute x)));
    attributes = (fun _ x -> copy_attributes (attributes mapper (R.copy_attributes x)));
    case = (fun _ x -> copy_case (case mapper (R.copy_case x)));
    cases = (fun _ x -> copy_cases (cases mapper (R.copy_cases x)));
    class_declaration = (fun _ x -> copy_class_declaration (class_declaration mapper (R.copy_class_declaration x)));
    class_description = (fun _ x -> copy_class_description (class_description mapper (R.copy_class_description x)));
    class_expr = (fun _ x -> copy_class_expr (class_expr mapper (R.copy_class_expr x)));
    class_field = (fun _ x -> copy_class_field (class_field mapper (R.copy_class_field x)));
    class_signature = (fun _ x -> copy_class_signature (class_signature mapper (R.copy_class_signature x)));
    class_structure = (fun _ x -> copy_class_structure (class_structure mapper (R.copy_class_structure x)));
    class_type = (fun _ x -> copy_class_type (class_type mapper (R.copy_class_type x)));
    class_type_declaration = (fun _ x -> copy_class_type_declaration (class_type_declaration mapper (R.copy_class_type_declaration x)));
    class_type_field = (fun _ x -> copy_class_type_field (class_type_field mapper (R.copy_class_type_field x)));
    constructor_declaration = (fun _ x -> copy_constructor_declaration (constructor_declaration mapper (R.copy_constructor_declaration x)));
    expr = (fun _ x -> copy_expr (expr mapper (R.copy_expr x)));
    extension = (fun _ x -> copy_extension (extension mapper (R.copy_extension x)));
    extension_constructor = (fun _ x -> copy_extension_constructor (extension_constructor mapper (R.copy_extension_constructor x)));
    include_declaration = (fun _ x -> copy_include_declaration (include_declaration mapper (R.copy_include_declaration x)));
    include_description = (fun _ x -> copy_include_description (include_description mapper (R.copy_include_description x)));
    label_declaration = (fun _ x -> copy_label_declaration (label_declaration mapper (R.copy_label_declaration x)));
    location = (fun _ x -> copy_location (location mapper (R.copy_location x)));
    module_binding = (fun _ x -> copy_module_binding (module_binding mapper (R.copy_module_binding x)));
    module_declaration = (fun _ x -> copy_module_declaration (module_declaration mapper (R.copy_module_declaration x)));
    module_expr = (fun _ x -> copy_module_expr (module_expr mapper (R.copy_module_expr x)));
    module_type = (fun _ x -> copy_module_type (module_type mapper (R.copy_module_type x)));
    module_type_declaration = (fun _ x -> copy_module_type_declaration (module_type_declaration mapper (R.copy_module_type_declaration x)));
    open_description = (fun _ x -> copy_open_description (open_description mapper (R.copy_open_description x)));
    pat = (fun _ x -> copy_pat (pat mapper (R.copy_pat x)));
    signature = (fun _ x -> copy_signature (signature mapper (R.copy_signature x)));
    signature_item = (fun _ x -> copy_signature_item (signature_item mapper (R.copy_signature_item x)));
    structure = (fun _ x -> copy_structure (structure mapper (R.copy_structure x)));
    structure_item = (fun _ x -> copy_structure_item (structure_item mapper (R.copy_structure_item x)));
    typ = (fun _ x -> copy_typ (typ mapper (R.copy_typ x)));
    type_declaration = (fun _ x -> copy_type_declaration (type_declaration mapper (R.copy_type_declaration x)));
    type_extension = (fun _ x -> copy_type_extension (type_extension mapper (R.copy_type_extension x)));
    type_kind = (fun _ x -> copy_type_kind (type_kind mapper (R.copy_type_kind x)));
    value_binding = (fun _ x -> copy_value_binding (value_binding mapper (R.copy_value_binding x)));
    value_description = (fun _ x -> copy_value_description (value_description mapper (R.copy_value_description x)));
    with_constraint = (fun _ x -> copy_with_constraint (with_constraint mapper (R.copy_with_constraint x)));
    payload = (fun _ x -> copy_payload (payload mapper (R.copy_payload x)));
    (*$*)
  }

end
module Migrate_parsetree_405_404
= struct
#1 "migrate_parsetree_405_404.ml"
# 1 "src/migrate_parsetree_405_404.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Migrate_parsetree_405_404_migrate

(*$ open Printf
    let fields = [
      "attribute"; "attributes"; "case"; "cases"; "class_declaration";
      "class_description"; "class_expr"; "class_field"; "class_signature";
      "class_structure"; "class_type"; "class_type_declaration";
      "class_type_field"; "constructor_declaration"; "expr"; "extension";
      "extension_constructor"; "include_declaration"; "include_description";
      "label_declaration"; "location"; "module_binding"; "module_declaration";
      "module_expr"; "module_type"; "module_type_declaration";
      "open_description"; "pat"; "signature"; "signature_item"; "structure";
      "structure_item"; "typ"; "type_declaration"; "type_extension";
      "type_kind"; "value_binding"; "value_description";
      "with_constraint"; "payload"
    ]
  let foreach_field f =
    printf "\n";
    List.iter f fields
*)(*$*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$ foreach_field (printf "%s;\n")*)
     attribute;
     attributes;
     case;
     cases;
     class_declaration;
     class_description;
     class_expr;
     class_field;
     class_signature;
     class_structure;
     class_type;
     class_type_declaration;
     class_type_field;
     constructor_declaration;
     expr;
     extension;
     extension_constructor;
     include_declaration;
     include_description;
     label_declaration;
     location;
     module_binding;
     module_declaration;
     module_expr;
     module_type;
     module_type_declaration;
     open_description;
     pat;
     signature;
     signature_item;
     structure;
     structure_item;
     typ;
     type_declaration;
     type_extension;
     type_kind;
     value_binding;
     value_description;
     with_constraint;
     payload;
     (*$*)
   } as mapper) ->
  let module R = Migrate_parsetree_404_405_migrate in
  {
    To.Ast_mapper.
    (*$ foreach_field (fun s ->
          printf
          "%s = (fun _ x -> copy_%s (%s mapper (R.copy_%s x)));\n" s s s s)
    *)
    attribute = (fun _ x -> copy_attribute (attribute mapper (R.copy_attribute x)));
    attributes = (fun _ x -> copy_attributes (attributes mapper (R.copy_attributes x)));
    case = (fun _ x -> copy_case (case mapper (R.copy_case x)));
    cases = (fun _ x -> copy_cases (cases mapper (R.copy_cases x)));
    class_declaration = (fun _ x -> copy_class_declaration (class_declaration mapper (R.copy_class_declaration x)));
    class_description = (fun _ x -> copy_class_description (class_description mapper (R.copy_class_description x)));
    class_expr = (fun _ x -> copy_class_expr (class_expr mapper (R.copy_class_expr x)));
    class_field = (fun _ x -> copy_class_field (class_field mapper (R.copy_class_field x)));
    class_signature = (fun _ x -> copy_class_signature (class_signature mapper (R.copy_class_signature x)));
    class_structure = (fun _ x -> copy_class_structure (class_structure mapper (R.copy_class_structure x)));
    class_type = (fun _ x -> copy_class_type (class_type mapper (R.copy_class_type x)));
    class_type_declaration = (fun _ x -> copy_class_type_declaration (class_type_declaration mapper (R.copy_class_type_declaration x)));
    class_type_field = (fun _ x -> copy_class_type_field (class_type_field mapper (R.copy_class_type_field x)));
    constructor_declaration = (fun _ x -> copy_constructor_declaration (constructor_declaration mapper (R.copy_constructor_declaration x)));
    expr = (fun _ x -> copy_expr (expr mapper (R.copy_expr x)));
    extension = (fun _ x -> copy_extension (extension mapper (R.copy_extension x)));
    extension_constructor = (fun _ x -> copy_extension_constructor (extension_constructor mapper (R.copy_extension_constructor x)));
    include_declaration = (fun _ x -> copy_include_declaration (include_declaration mapper (R.copy_include_declaration x)));
    include_description = (fun _ x -> copy_include_description (include_description mapper (R.copy_include_description x)));
    label_declaration = (fun _ x -> copy_label_declaration (label_declaration mapper (R.copy_label_declaration x)));
    location = (fun _ x -> copy_location (location mapper (R.copy_location x)));
    module_binding = (fun _ x -> copy_module_binding (module_binding mapper (R.copy_module_binding x)));
    module_declaration = (fun _ x -> copy_module_declaration (module_declaration mapper (R.copy_module_declaration x)));
    module_expr = (fun _ x -> copy_module_expr (module_expr mapper (R.copy_module_expr x)));
    module_type = (fun _ x -> copy_module_type (module_type mapper (R.copy_module_type x)));
    module_type_declaration = (fun _ x -> copy_module_type_declaration (module_type_declaration mapper (R.copy_module_type_declaration x)));
    open_description = (fun _ x -> copy_open_description (open_description mapper (R.copy_open_description x)));
    pat = (fun _ x -> copy_pat (pat mapper (R.copy_pat x)));
    signature = (fun _ x -> copy_signature (signature mapper (R.copy_signature x)));
    signature_item = (fun _ x -> copy_signature_item (signature_item mapper (R.copy_signature_item x)));
    structure = (fun _ x -> copy_structure (structure mapper (R.copy_structure x)));
    structure_item = (fun _ x -> copy_structure_item (structure_item mapper (R.copy_structure_item x)));
    typ = (fun _ x -> copy_typ (typ mapper (R.copy_typ x)));
    type_declaration = (fun _ x -> copy_type_declaration (type_declaration mapper (R.copy_type_declaration x)));
    type_extension = (fun _ x -> copy_type_extension (type_extension mapper (R.copy_type_extension x)));
    type_kind = (fun _ x -> copy_type_kind (type_kind mapper (R.copy_type_kind x)));
    value_binding = (fun _ x -> copy_value_binding (value_binding mapper (R.copy_value_binding x)));
    value_description = (fun _ x -> copy_value_description (value_description mapper (R.copy_value_description x)));
    with_constraint = (fun _ x -> copy_with_constraint (with_constraint mapper (R.copy_with_constraint x)));
    payload = (fun _ x -> copy_payload (payload mapper (R.copy_payload x)));
    (*$*)
  }

end
module Migrate_parsetree_versions : sig 
#1 "migrate_parsetree_versions.mli"
# 1 "src/migrate_parsetree_versions.mli"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*$ #use "src/cinaps_helpers" $*)

(** {1 Abstracting an OCaml frontend} *)

(** Abstract view of a version of an OCaml Ast *)
module type Ast = sig
  (*$ foreach_module (fun m types ->
        printf "module %s : sig\n" m;
        List.iter types ~f:(printf "type %s\n");
        printf "end\n"
      )
  *)
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
  end
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  module Ast_mapper : sig
    type mapper
  end
  (*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
  val map_signature : Ast_mapper.mapper -> Parsetree.signature -> Parsetree.signature
  val map_structure : Ast_mapper.mapper -> Parsetree.structure -> Parsetree.structure
  val make_top_mapper
    :  signature:(Parsetree.signature -> Parsetree.signature)
    -> structure:(Parsetree.structure -> Parsetree.structure)
    -> Ast_mapper.mapper
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
  (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)
  structure             : _;
  signature             : _;
  toplevel_phrase       : _;
  core_type             : _;
  expression            : _;
  pattern               : _;
  case                  : _;
  type_declaration      : _;
  type_extension        : _;
  extension_constructor : _;
  out_value             : _;
  out_type              : _;
  out_class_type        : _;
  out_module_type       : _;
  out_sig_item          : _;
  out_type_extension    : _;
  out_phrase            : _;
  mapper                : _;
  (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
      printf "type 'a get_%s = 'x constraint 'a _types = < %s : 'x; .. >\n" s s
    );
    printf ";;\n" *)
type 'a get_structure = 'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature = 'x constraint 'a _types = < signature : 'x; .. >
type 'a get_toplevel_phrase = 'x constraint 'a _types = < toplevel_phrase : 'x; .. >
type 'a get_core_type = 'x constraint 'a _types = < core_type : 'x; .. >
type 'a get_expression = 'x constraint 'a _types = < expression : 'x; .. >
type 'a get_pattern = 'x constraint 'a _types = < pattern : 'x; .. >
type 'a get_case = 'x constraint 'a _types = < case : 'x; .. >
type 'a get_type_declaration = 'x constraint 'a _types = < type_declaration : 'x; .. >
type 'a get_type_extension = 'x constraint 'a _types = < type_extension : 'x; .. >
type 'a get_extension_constructor = 'x constraint 'a _types = < extension_constructor : 'x; .. >
type 'a get_out_value = 'x constraint 'a _types = < out_value : 'x; .. >
type 'a get_out_type = 'x constraint 'a _types = < out_type : 'x; .. >
type 'a get_out_class_type = 'x constraint 'a _types = < out_class_type : 'x; .. >
type 'a get_out_module_type = 'x constraint 'a _types = < out_module_type : 'x; .. >
type 'a get_out_sig_item = 'x constraint 'a _types = < out_sig_item : 'x; .. >
type 'a get_out_type_extension = 'x constraint 'a _types = < out_type_extension : 'x; .. >
type 'a get_out_phrase = 'x constraint 'a _types = < out_phrase : 'x; .. >
type 'a get_mapper = 'x constraint 'a _types = < mapper : 'x; .. >
;;
(*$*)

(** A version of the OCaml frontend packs the ast with type witnesses
    so that equalities can be recovered dynamically. *)
type _ witnesses

(** [migration_info] is an opaque type that is used to generate migration
    functions. *)
type _ migration_info

(** An OCaml frontend versions an Ast, version number and some witnesses for
    conversion. *)
module type OCaml_version = sig

  (** Ast definition for this version *)
  module Ast : Ast

  (* Version number as an integer, 402, 403, 404, ... *)
  val version : int

  (* Version number as a user-friendly string *)
  val string_version : string (* 4.02, 4.03, 4.04, ... *)

  (** Shortcut for talking about Ast types *)
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s) *)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  > _types

  (** A construtor for recovering type equalities between two arbitrary
      versions. *)
  type _ witnesses += Version : types witnesses

  (** Information used to derive migration functions, see below *)
  val migration_info : types migration_info
end

(** Representing an ocaml version in type language *)
type 'types ocaml_version =
  (module OCaml_version
    (*$ let sep = with_then_and () in
        foreach_type (fun m s ->
          printf "%t type Ast.%s.%s = 'types get_%s\n" sep m s s) *)
    with type Ast.Parsetree.structure = 'types get_structure
     and type Ast.Parsetree.signature = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type = 'types get_core_type
     and type Ast.Parsetree.expression = 'types get_expression
     and type Ast.Parsetree.pattern = 'types get_pattern
     and type Ast.Parsetree.case = 'types get_case
     and type Ast.Parsetree.type_declaration = 'types get_type_declaration
     and type Ast.Parsetree.type_extension = 'types get_type_extension
     and type Ast.Parsetree.extension_constructor = 'types get_extension_constructor
     and type Ast.Outcometree.out_value = 'types get_out_value
     and type Ast.Outcometree.out_type = 'types get_out_type
     and type Ast.Outcometree.out_class_type = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase = 'types get_out_phrase
     and type Ast.Ast_mapper.mapper = 'types get_mapper
     (*$*)
  )

(** {1 Concrete frontend instances} *)

(*$foreach_version (fun suffix _ ->
     printf "module OCaml_%s : OCaml_version with module Ast = Ast_%s\n"
       suffix suffix;
     printf "val ocaml_%s : OCaml_%s.types ocaml_version\n" suffix suffix;
  )*)
module OCaml_402 : OCaml_version with module Ast = Ast_402
val ocaml_402 : OCaml_402.types ocaml_version
module OCaml_403 : OCaml_version with module Ast = Ast_403
val ocaml_403 : OCaml_403.types ocaml_version
module OCaml_404 : OCaml_version with module Ast = Ast_404
val ocaml_404 : OCaml_404.types ocaml_version
module OCaml_405 : OCaml_version with module Ast = Ast_405
val ocaml_405 : OCaml_405.types ocaml_version
(*$*)

(* An alias to the current compiler version *)
module OCaml_current = OCaml_402          
val ocaml_current : OCaml_current.types ocaml_version

val all_versions : (module OCaml_version) list

(** {1 Migrating between different versions} *)

type ('a, 'b) type_comparison =
  | Lt : ('a, 'b) type_comparison
  | Eq : ('a, 'a) type_comparison
  | Gt : ('a, 'b) type_comparison

val compare_ocaml_version : 'a ocaml_version -> 'b ocaml_version -> ('a, 'b) type_comparison

(** A record for migrating each AST construct between two known versions *)
type ('from, 'to_) migration_functions = {
  (*$ foreach_type (fun _ s ->
        printf "copy_%s: 'from get_%s -> 'to_ get_%s;\n" s s s) *)
  copy_structure: 'from get_structure -> 'to_ get_structure;
  copy_signature: 'from get_signature -> 'to_ get_signature;
  copy_toplevel_phrase: 'from get_toplevel_phrase -> 'to_ get_toplevel_phrase;
  copy_core_type: 'from get_core_type -> 'to_ get_core_type;
  copy_expression: 'from get_expression -> 'to_ get_expression;
  copy_pattern: 'from get_pattern -> 'to_ get_pattern;
  copy_case: 'from get_case -> 'to_ get_case;
  copy_type_declaration: 'from get_type_declaration -> 'to_ get_type_declaration;
  copy_type_extension: 'from get_type_extension -> 'to_ get_type_extension;
  copy_extension_constructor: 'from get_extension_constructor -> 'to_ get_extension_constructor;
  copy_out_value: 'from get_out_value -> 'to_ get_out_value;
  copy_out_type: 'from get_out_type -> 'to_ get_out_type;
  copy_out_class_type: 'from get_out_class_type -> 'to_ get_out_class_type;
  copy_out_module_type: 'from get_out_module_type -> 'to_ get_out_module_type;
  copy_out_sig_item: 'from get_out_sig_item -> 'to_ get_out_sig_item;
  copy_out_type_extension: 'from get_out_type_extension -> 'to_ get_out_type_extension;
  copy_out_phrase: 'from get_out_phrase -> 'to_ get_out_phrase;
  copy_mapper: 'from get_mapper -> 'to_ get_mapper;
  (*$*)
}

(** Migrating to the same version is no-op *)
val migration_identity : ('a, 'a) migration_functions

(** Migrations can be composed *)
val migration_compose : ('a, 'b) migration_functions -> ('b, 'c) migration_functions -> ('a, 'c) migration_functions

(** Represent the next or previous version of an Ast *)

type 'from immediate_migration =
  | No_migration : 'from immediate_migration
  (** Cannot migrate earliest or latest supported version *)
  |
    Immediate_migration :
      ('from, 'to_) migration_functions * 'to_ ocaml_version -> 'from immediate_migration
  (** Pack the migration functions and the new version *)

val immediate_migration : 'types ocaml_version -> [< `Next | `Previous ] -> 'types immediate_migration

val migrate : 'from ocaml_version -> 'to_ ocaml_version -> ('from, 'to_) migration_functions

(** {1 Convenience definitions} *)

(** Module level migration *)
module Convert (A : OCaml_version) (B : OCaml_version) : sig
  (*$ foreach_type (fun m s ->
        let fq = sprintf "%s.%s" m s in
        printf "  val copy_%-21s : A.Ast.%-31s -> B.Ast.%s\n" s fq fq) *)
  val copy_structure             : A.Ast.Parsetree.structure             -> B.Ast.Parsetree.structure
  val copy_signature             : A.Ast.Parsetree.signature             -> B.Ast.Parsetree.signature
  val copy_toplevel_phrase       : A.Ast.Parsetree.toplevel_phrase       -> B.Ast.Parsetree.toplevel_phrase
  val copy_core_type             : A.Ast.Parsetree.core_type             -> B.Ast.Parsetree.core_type
  val copy_expression            : A.Ast.Parsetree.expression            -> B.Ast.Parsetree.expression
  val copy_pattern               : A.Ast.Parsetree.pattern               -> B.Ast.Parsetree.pattern
  val copy_case                  : A.Ast.Parsetree.case                  -> B.Ast.Parsetree.case
  val copy_type_declaration      : A.Ast.Parsetree.type_declaration      -> B.Ast.Parsetree.type_declaration
  val copy_type_extension        : A.Ast.Parsetree.type_extension        -> B.Ast.Parsetree.type_extension
  val copy_extension_constructor : A.Ast.Parsetree.extension_constructor -> B.Ast.Parsetree.extension_constructor
  val copy_out_value             : A.Ast.Outcometree.out_value           -> B.Ast.Outcometree.out_value
  val copy_out_type              : A.Ast.Outcometree.out_type            -> B.Ast.Outcometree.out_type
  val copy_out_class_type        : A.Ast.Outcometree.out_class_type      -> B.Ast.Outcometree.out_class_type
  val copy_out_module_type       : A.Ast.Outcometree.out_module_type     -> B.Ast.Outcometree.out_module_type
  val copy_out_sig_item          : A.Ast.Outcometree.out_sig_item        -> B.Ast.Outcometree.out_sig_item
  val copy_out_type_extension    : A.Ast.Outcometree.out_type_extension  -> B.Ast.Outcometree.out_type_extension
  val copy_out_phrase            : A.Ast.Outcometree.out_phrase          -> B.Ast.Outcometree.out_phrase
  val copy_mapper                : A.Ast.Ast_mapper.mapper               -> B.Ast.Ast_mapper.mapper
  (*$*)
end

end = struct
#1 "migrate_parsetree_versions.ml"
# 1 "src/migrate_parsetree_versions.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* BEGIN of BLACK MAGIC *)
(*$ #use "src/cinaps_helpers" $*)

type _ witnesses = ..

type _ migration = ..
type _ migration += Undefined : _ migration

type 'a migration_info = {
  mutable next_version : 'a migration;
  mutable previous_version : 'a migration;
}

(** Abstract view of a version of an OCaml Ast *)
module type Ast = sig
  (*$ foreach_module (fun m types ->
        printf "module %s : sig\n" m;
        List.iter types ~f:(printf "type %s\n");
        printf "end\n"
      )
  *)
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
  end
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  module Ast_mapper : sig
    type mapper
  end
  (*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
  val map_signature : Ast_mapper.mapper -> Parsetree.signature -> Parsetree.signature
  val map_structure : Ast_mapper.mapper -> Parsetree.structure -> Parsetree.structure
  val make_top_mapper
    :  signature:(Parsetree.signature -> Parsetree.signature)
    -> structure:(Parsetree.structure -> Parsetree.structure)
    -> Ast_mapper.mapper
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
  (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)
  structure             : _;
  signature             : _;
  toplevel_phrase       : _;
  core_type             : _;
  expression            : _;
  pattern               : _;
  case                  : _;
  type_declaration      : _;
  type_extension        : _;
  extension_constructor : _;
  out_value             : _;
  out_type              : _;
  out_class_type        : _;
  out_module_type       : _;
  out_sig_item          : _;
  out_type_extension    : _;
  out_phrase            : _;
  mapper                : _;
  (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
      printf "type 'a get_%s =\n" s;
      printf " 'x constraint 'a _types = < %s : 'x; .. >\n" s
    ) *)
type 'a get_structure =
  'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature =
  'x constraint 'a _types = < signature : 'x; .. >
type 'a get_toplevel_phrase =
  'x constraint 'a _types = < toplevel_phrase : 'x; .. >
type 'a get_core_type =
  'x constraint 'a _types = < core_type : 'x; .. >
type 'a get_expression =
  'x constraint 'a _types = < expression : 'x; .. >
type 'a get_pattern =
  'x constraint 'a _types = < pattern : 'x; .. >
type 'a get_case =
  'x constraint 'a _types = < case : 'x; .. >
type 'a get_type_declaration =
  'x constraint 'a _types = < type_declaration : 'x; .. >
type 'a get_type_extension =
  'x constraint 'a _types = < type_extension : 'x; .. >
type 'a get_extension_constructor =
  'x constraint 'a _types = < extension_constructor : 'x; .. >
type 'a get_out_value =
  'x constraint 'a _types = < out_value : 'x; .. >
type 'a get_out_type =
  'x constraint 'a _types = < out_type : 'x; .. >
type 'a get_out_class_type =
  'x constraint 'a _types = < out_class_type : 'x; .. >
type 'a get_out_module_type =
  'x constraint 'a _types = < out_module_type : 'x; .. >
type 'a get_out_sig_item =
  'x constraint 'a _types = < out_sig_item : 'x; .. >
type 'a get_out_type_extension =
  'x constraint 'a _types = < out_type_extension : 'x; .. >
type 'a get_out_phrase =
  'x constraint 'a _types = < out_phrase : 'x; .. >
type 'a get_mapper =
  'x constraint 'a _types = < mapper : 'x; .. >
       (*$*)

module type OCaml_version = sig
  module Ast : Ast
  val version : int
  val string_version : string
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  > _types
  type _ witnesses += Version : types witnesses
  val migration_info : types migration_info
end

module Make_witness(Ast : Ast) =
struct
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  > _types
  type _ witnesses += Version : types witnesses
  let migration_info : types migration_info =
    { next_version = Undefined; previous_version = Undefined }
end

type 'types ocaml_version =
  (module OCaml_version
    (*$ let sep = with_then_and () in
        foreach_type (fun m s ->
          printf "%t type Ast.%s.%s = 'types get_%s\n" sep m s s) *)
    with type Ast.Parsetree.structure = 'types get_structure
     and type Ast.Parsetree.signature = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type = 'types get_core_type
     and type Ast.Parsetree.expression = 'types get_expression
     and type Ast.Parsetree.pattern = 'types get_pattern
     and type Ast.Parsetree.case = 'types get_case
     and type Ast.Parsetree.type_declaration = 'types get_type_declaration
     and type Ast.Parsetree.type_extension = 'types get_type_extension
     and type Ast.Parsetree.extension_constructor = 'types get_extension_constructor
     and type Ast.Outcometree.out_value = 'types get_out_value
     and type Ast.Outcometree.out_type = 'types get_out_type
     and type Ast.Outcometree.out_class_type = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase = 'types get_out_phrase
     and type Ast.Ast_mapper.mapper = 'types get_mapper
     (*$*)
  )

type ('a, 'b) type_comparison =
  | Lt : ('a, 'b) type_comparison
  | Eq : ('a, 'a) type_comparison
  | Gt : ('a, 'b) type_comparison

let compare_ocaml_version
    (*$ foreach_type (fun _ s -> printf "(type %s1) (type %s2)\n" s s) *)
    (type structure1) (type structure2)
    (type signature1) (type signature2)
    (type toplevel_phrase1) (type toplevel_phrase2)
    (type core_type1) (type core_type2)
    (type expression1) (type expression2)
    (type pattern1) (type pattern2)
    (type case1) (type case2)
    (type type_declaration1) (type type_declaration2)
    (type type_extension1) (type type_extension2)
    (type extension_constructor1) (type extension_constructor2)
    (type out_value1) (type out_value2)
    (type out_type1) (type out_type2)
    (type out_class_type1) (type out_class_type2)
    (type out_module_type1) (type out_module_type2)
    (type out_sig_item1) (type out_sig_item2)
    (type out_type_extension1) (type out_type_extension2)
    (type out_phrase1) (type out_phrase2)
    (type mapper1) (type mapper2)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s1;\n" s s) *)
     structure             : structure1;
     signature             : signature1;
     toplevel_phrase       : toplevel_phrase1;
     core_type             : core_type1;
     expression            : expression1;
     pattern               : pattern1;
     case                  : case1;
     type_declaration      : type_declaration1;
     type_extension        : type_extension1;
     extension_constructor : extension_constructor1;
     out_value             : out_value1;
     out_type              : out_type1;
     out_class_type        : out_class_type1;
     out_module_type       : out_module_type1;
     out_sig_item          : out_sig_item1;
     out_type_extension    : out_type_extension1;
     out_phrase            : out_phrase1;
     mapper                : mapper1;
     (*$*)
     > ocaml_version)
    ((module B) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s2;\n" s s) *)
     structure             : structure2;
     signature             : signature2;
     toplevel_phrase       : toplevel_phrase2;
     core_type             : core_type2;
     expression            : expression2;
     pattern               : pattern2;
     case                  : case2;
     type_declaration      : type_declaration2;
     type_extension        : type_extension2;
     extension_constructor : extension_constructor2;
     out_value             : out_value2;
     out_type              : out_type2;
     out_class_type        : out_class_type2;
     out_module_type       : out_module_type2;
     out_sig_item          : out_sig_item2;
     out_type_extension    : out_type_extension2;
     out_phrase            : out_phrase2;
     mapper                : mapper2;
     (*$*)
     > ocaml_version)
  : (A.types, B.types) type_comparison
  =
  match A.Version with
  | B.Version -> Eq
  | _ when A.version < B.version -> Lt
  | _ when A.version > B.version -> Gt
  | _ -> assert false

type ('from, 'to_) migration_functions = {
  (*$ foreach_type (fun _ s ->
        printf "copy_%s: 'from get_%s -> 'to_ get_%s;\n" s s s) *)
  copy_structure: 'from get_structure -> 'to_ get_structure;
  copy_signature: 'from get_signature -> 'to_ get_signature;
  copy_toplevel_phrase: 'from get_toplevel_phrase -> 'to_ get_toplevel_phrase;
  copy_core_type: 'from get_core_type -> 'to_ get_core_type;
  copy_expression: 'from get_expression -> 'to_ get_expression;
  copy_pattern: 'from get_pattern -> 'to_ get_pattern;
  copy_case: 'from get_case -> 'to_ get_case;
  copy_type_declaration: 'from get_type_declaration -> 'to_ get_type_declaration;
  copy_type_extension: 'from get_type_extension -> 'to_ get_type_extension;
  copy_extension_constructor: 'from get_extension_constructor -> 'to_ get_extension_constructor;
  copy_out_value: 'from get_out_value -> 'to_ get_out_value;
  copy_out_type: 'from get_out_type -> 'to_ get_out_type;
  copy_out_class_type: 'from get_out_class_type -> 'to_ get_out_class_type;
  copy_out_module_type: 'from get_out_module_type -> 'to_ get_out_module_type;
  copy_out_sig_item: 'from get_out_sig_item -> 'to_ get_out_sig_item;
  copy_out_type_extension: 'from get_out_type_extension -> 'to_ get_out_type_extension;
  copy_out_phrase: 'from get_out_phrase -> 'to_ get_out_phrase;
  copy_mapper: 'from get_mapper -> 'to_ get_mapper;
  (*$*)
}

let id x = x
let migration_identity : ('a, 'a) migration_functions = {
  (*$ foreach_type (fun _ s -> printf "copy_%s = id;\n" s) *)
  copy_structure = id;
  copy_signature = id;
  copy_toplevel_phrase = id;
  copy_core_type = id;
  copy_expression = id;
  copy_pattern = id;
  copy_case = id;
  copy_type_declaration = id;
  copy_type_extension = id;
  copy_extension_constructor = id;
  copy_out_value = id;
  copy_out_type = id;
  copy_out_class_type = id;
  copy_out_module_type = id;
  copy_out_sig_item = id;
  copy_out_type_extension = id;
  copy_out_phrase = id;
  copy_mapper = id;
  (*$*)
}

let compose f g x = f (g x)
let migration_compose (ab : ('a, 'b) migration_functions) (bc : ('b, 'c) migration_functions) : ('a, 'c) migration_functions = {
  (*$ foreach_type (fun _ s ->
        printf "copy_%-21s = compose bc.copy_%-21s ab.copy_%s;\n" s s s) *)
  copy_structure             = compose bc.copy_structure             ab.copy_structure;
  copy_signature             = compose bc.copy_signature             ab.copy_signature;
  copy_toplevel_phrase       = compose bc.copy_toplevel_phrase       ab.copy_toplevel_phrase;
  copy_core_type             = compose bc.copy_core_type             ab.copy_core_type;
  copy_expression            = compose bc.copy_expression            ab.copy_expression;
  copy_pattern               = compose bc.copy_pattern               ab.copy_pattern;
  copy_case                  = compose bc.copy_case                  ab.copy_case;
  copy_type_declaration      = compose bc.copy_type_declaration      ab.copy_type_declaration;
  copy_type_extension        = compose bc.copy_type_extension        ab.copy_type_extension;
  copy_extension_constructor = compose bc.copy_extension_constructor ab.copy_extension_constructor;
  copy_out_value             = compose bc.copy_out_value             ab.copy_out_value;
  copy_out_type              = compose bc.copy_out_type              ab.copy_out_type;
  copy_out_class_type        = compose bc.copy_out_class_type        ab.copy_out_class_type;
  copy_out_module_type       = compose bc.copy_out_module_type       ab.copy_out_module_type;
  copy_out_sig_item          = compose bc.copy_out_sig_item          ab.copy_out_sig_item;
  copy_out_type_extension    = compose bc.copy_out_type_extension    ab.copy_out_type_extension;
  copy_out_phrase            = compose bc.copy_out_phrase            ab.copy_out_phrase;
  copy_mapper                = compose bc.copy_mapper                ab.copy_mapper;
  (*$*)
}

type _ migration += Migration : 'from ocaml_version * ('from, 'to_) migration_functions * 'to_ ocaml_version -> 'from migration

module type Migrate_module = sig
  module From : Ast
  module To : Ast
  (*$ foreach_type (fun m s ->
        printf "val copy_%-21s: From.%s.%s -> To.%s.%s\n" s m s m s) *)
  val copy_structure            : From.Parsetree.structure -> To.Parsetree.structure
  val copy_signature            : From.Parsetree.signature -> To.Parsetree.signature
  val copy_toplevel_phrase      : From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase
  val copy_core_type            : From.Parsetree.core_type -> To.Parsetree.core_type
  val copy_expression           : From.Parsetree.expression -> To.Parsetree.expression
  val copy_pattern              : From.Parsetree.pattern -> To.Parsetree.pattern
  val copy_case                 : From.Parsetree.case -> To.Parsetree.case
  val copy_type_declaration     : From.Parsetree.type_declaration -> To.Parsetree.type_declaration
  val copy_type_extension       : From.Parsetree.type_extension -> To.Parsetree.type_extension
  val copy_extension_constructor: From.Parsetree.extension_constructor -> To.Parsetree.extension_constructor
  val copy_out_value            : From.Outcometree.out_value -> To.Outcometree.out_value
  val copy_out_type             : From.Outcometree.out_type -> To.Outcometree.out_type
  val copy_out_class_type       : From.Outcometree.out_class_type -> To.Outcometree.out_class_type
  val copy_out_module_type      : From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  val copy_out_sig_item         : From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item
  val copy_out_type_extension   : From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension
  val copy_out_phrase           : From.Outcometree.out_phrase -> To.Outcometree.out_phrase
  val copy_mapper               : From.Ast_mapper.mapper -> To.Ast_mapper.mapper
  (*$*)
end

module Migration_functions
    (A : OCaml_version) (B : OCaml_version)
    (A_to_B : Migrate_module with module From = A.Ast and module To = B.Ast)
=
struct
  let migration_functions : (A.types, B.types) migration_functions =
    let open A_to_B in
    {
      (*$ foreach_type (fun _ s -> printf "copy_%s;\n" s) *)
      copy_structure;
      copy_signature;
      copy_toplevel_phrase;
      copy_core_type;
      copy_expression;
      copy_pattern;
      copy_case;
      copy_type_declaration;
      copy_type_extension;
      copy_extension_constructor;
      copy_out_value;
      copy_out_type;
      copy_out_class_type;
      copy_out_module_type;
      copy_out_sig_item;
      copy_out_type_extension;
      copy_out_phrase;
      copy_mapper;
      (*$*)
    }
end

module Register_migration (A : OCaml_version) (B : OCaml_version)
    (A_to_B : Migrate_module with module From = A.Ast and module To = B.Ast)
    (B_to_A : Migrate_module with module From = B.Ast and module To = A.Ast)
=
struct
  let () = (
    let is_undefined : type a. a migration -> bool = function
      | Undefined -> true
      | _ -> false
    in
    assert (A.version < B.version);
    assert (is_undefined A.migration_info.next_version);
    assert (is_undefined B.migration_info.previous_version);
    let module A_to_B_fun = Migration_functions(A)(B)(A_to_B) in
    let module B_to_A_fun = Migration_functions(B)(A)(B_to_A) in
    A.migration_info.next_version <-
      Migration ((module A), A_to_B_fun.migration_functions, (module B));
    B.migration_info.previous_version <-
      Migration ((module B), B_to_A_fun.migration_functions, (module A));
  )
end

type 'from immediate_migration =
  | No_migration : 'from immediate_migration
  | Immediate_migration
    :  ('from, 'to_) migration_functions * 'to_ ocaml_version
    -> 'from immediate_migration

let immediate_migration
    (*$ foreach_type (fun _ s -> printf "(type %s)\n" s) *)
    (type structure)
    (type signature)
    (type toplevel_phrase)
    (type core_type)
    (type expression)
    (type pattern)
    (type case)
    (type type_declaration)
    (type type_extension)
    (type extension_constructor)
    (type out_value)
    (type out_type)
    (type out_class_type)
    (type out_module_type)
    (type out_sig_item)
    (type out_type_extension)
    (type out_phrase)
    (type mapper)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s;\n" s s) *)
     structure             : structure;
     signature             : signature;
     toplevel_phrase       : toplevel_phrase;
     core_type             : core_type;
     expression            : expression;
     pattern               : pattern;
     case                  : case;
     type_declaration      : type_declaration;
     type_extension        : type_extension;
     extension_constructor : extension_constructor;
     out_value             : out_value;
     out_type              : out_type;
     out_class_type        : out_class_type;
     out_module_type       : out_module_type;
     out_sig_item          : out_sig_item;
     out_type_extension    : out_type_extension;
     out_phrase            : out_phrase;
     mapper                : mapper;
     (*$*)
     > ocaml_version)
    direction
  =
  let version = match direction with
    | `Next -> A.migration_info.next_version
    | `Previous -> A.migration_info.previous_version
  in
  match version with
  | Undefined -> No_migration
  | Migration (_, funs, to_) -> Immediate_migration (funs, to_)
  | _ -> assert false

let migrate
    (*$ foreach_type (fun _ s -> printf "(type %s1) (type %s2)\n" s s) *)
    (type structure1) (type structure2)
    (type signature1) (type signature2)
    (type toplevel_phrase1) (type toplevel_phrase2)
    (type core_type1) (type core_type2)
    (type expression1) (type expression2)
    (type pattern1) (type pattern2)
    (type case1) (type case2)
    (type type_declaration1) (type type_declaration2)
    (type type_extension1) (type type_extension2)
    (type extension_constructor1) (type extension_constructor2)
    (type out_value1) (type out_value2)
    (type out_type1) (type out_type2)
    (type out_class_type1) (type out_class_type2)
    (type out_module_type1) (type out_module_type2)
    (type out_sig_item1) (type out_sig_item2)
    (type out_type_extension1) (type out_type_extension2)
    (type out_phrase1) (type out_phrase2)
    (type mapper1) (type mapper2)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s1;\n" s s) *)
     structure             : structure1;
     signature             : signature1;
     toplevel_phrase       : toplevel_phrase1;
     core_type             : core_type1;
     expression            : expression1;
     pattern               : pattern1;
     case                  : case1;
     type_declaration      : type_declaration1;
     type_extension        : type_extension1;
     extension_constructor : extension_constructor1;
     out_value             : out_value1;
     out_type              : out_type1;
     out_class_type        : out_class_type1;
     out_module_type       : out_module_type1;
     out_sig_item          : out_sig_item1;
     out_type_extension    : out_type_extension1;
     out_phrase            : out_phrase1;
     mapper                : mapper1;
     (*$*)
     > ocaml_version)
    ((module B) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s2;\n" s s) *)
     structure             : structure2;
     signature             : signature2;
     toplevel_phrase       : toplevel_phrase2;
     core_type             : core_type2;
     expression            : expression2;
     pattern               : pattern2;
     case                  : case2;
     type_declaration      : type_declaration2;
     type_extension        : type_extension2;
     extension_constructor : extension_constructor2;
     out_value             : out_value2;
     out_type              : out_type2;
     out_class_type        : out_class_type2;
     out_module_type       : out_module_type2;
     out_sig_item          : out_sig_item2;
     out_type_extension    : out_type_extension2;
     out_phrase            : out_phrase2;
     mapper                : mapper2;
     (*$*)
     > ocaml_version)
  : (A.types, B.types) migration_functions
  =
  match A.Version with
  | B.Version -> migration_identity
  | _ ->
    let direction = if A.version < B.version then `Next else `Previous in
    let rec migrate (m : A.types immediate_migration) : (A.types, B.types) migration_functions =
      match m with
      | No_migration -> assert false
      | Immediate_migration (f, (module To)) ->
        match To.Version with
        | B.Version -> f
        | _ ->
          match immediate_migration (module To) direction with
          | No_migration -> assert false
          | Immediate_migration (g, to2) ->
            migrate (Immediate_migration (migration_compose f g, to2))
    in
    migrate (immediate_migration (module A) direction)

module Convert (A : OCaml_version) (B : OCaml_version) = struct
  let {
    (*$ foreach_type (fun _ s -> printf "copy_%s;\n" s) *)
    copy_structure;
    copy_signature;
    copy_toplevel_phrase;
    copy_core_type;
    copy_expression;
    copy_pattern;
    copy_case;
    copy_type_declaration;
    copy_type_extension;
    copy_extension_constructor;
    copy_out_value;
    copy_out_type;
    copy_out_class_type;
    copy_out_module_type;
    copy_out_sig_item;
    copy_out_type_extension;
    copy_out_phrase;
    copy_mapper;
    (*$*)
  } : (A.types, B.types) migration_functions =
    migrate (module A) (module B)
end

(*$ foreach_version (fun suffix version ->
      printf "module OCaml_%s = struct\n" suffix;
      printf "  module Ast = Ast_%s\n" suffix;
      printf "  include Make_witness(Ast_%s)\n" suffix;
      printf "  let version = %s\n" suffix;
      printf "  let string_version = %S\n" version;
      printf "end\n";
      printf "let ocaml_%s : OCaml_%s.types ocaml_version = (module OCaml_%s)\n"
        suffix suffix suffix;
    )
*)
module OCaml_402 = struct
  module Ast = Ast_402
  include Make_witness(Ast_402)
  let version = 402
  let string_version = "4.02"
end
let ocaml_402 : OCaml_402.types ocaml_version = (module OCaml_402)
module OCaml_403 = struct
  module Ast = Ast_403
  include Make_witness(Ast_403)
  let version = 403
  let string_version = "4.03"
end
let ocaml_403 : OCaml_403.types ocaml_version = (module OCaml_403)
module OCaml_404 = struct
  module Ast = Ast_404
  include Make_witness(Ast_404)
  let version = 404
  let string_version = "4.04"
end
let ocaml_404 : OCaml_404.types ocaml_version = (module OCaml_404)
module OCaml_405 = struct
  module Ast = Ast_405
  include Make_witness(Ast_405)
  let version = 405
  let string_version = "4.05"
end
let ocaml_405 : OCaml_405.types ocaml_version = (module OCaml_405)
(*$*)

let all_versions : (module OCaml_version) list = [
  (*$foreach_version (fun suffix _ ->
       printf "(module OCaml_%s : OCaml_version);\n" suffix)*)
  (module OCaml_402 : OCaml_version);
  (module OCaml_403 : OCaml_version);
  (module OCaml_404 : OCaml_version);
  (module OCaml_405 : OCaml_version);
  (*$*)
]

(*$foreach_version_pair (fun a b ->
   printf "include Register_migration(OCaml_%s)(OCaml_%s)\n" a b;
   printf "  (Migrate_parsetree_%s_%s)(Migrate_parsetree_%s_%s)\n" a b b a
  )
*)
include Register_migration(OCaml_402)(OCaml_403)
    (Migrate_parsetree_402_403)(Migrate_parsetree_403_402)
include Register_migration(OCaml_403)(OCaml_404)
    (Migrate_parsetree_403_404)(Migrate_parsetree_404_403)
include Register_migration(OCaml_404)(OCaml_405)
    (Migrate_parsetree_404_405)(Migrate_parsetree_405_404)
(*$*)

module OCaml_current = OCaml_402          
let ocaml_current : OCaml_current.types ocaml_version = (module OCaml_current)

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : OCaml_current.Ast.Parsetree.expression = x

end
module Migrate_parsetree_ast_io : sig 
#1 "migrate_parsetree_ast_io.mli"
# 1 "src/migrate_parsetree_ast_io.mli"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A marshalled ast packs the ast with the corresponding version of the
    frontend *)
type ast =
  | Impl : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.structure = 'concrete) * 'concrete -> ast
  | Intf : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.signature = 'concrete) * 'concrete -> ast

(** A simple alias used for the filename of the source that produced an AST *)
type filename = string

type read_error =
  | Not_a_binary_ast of string
  (** The input doesn't contain a binary AST. The argument corresponds
      to the bytes from the input that were consumed. *)
  | Unknown_version of string
  (** The input contains a binary AST for an unknown version of OCaml.
      The argument is the unknown magic number. *)

(** Load a marshalled AST from a channel

    Any exception raised during unmarshalling (see [Marshal]) can escape.  *)
val from_channel : in_channel -> (filename * ast, read_error) result

(** Load a marshalled AST from a byte string.

    See [from_channel] description for exception that can be raised. *)
val from_bytes : bytes -> int -> (filename * ast, read_error) result

(** Marshal an AST to a channel *)
val to_channel : out_channel -> filename -> ast -> unit

(** Marshal an AST to a byte string *)
val to_bytes : filename -> ast -> bytes

end = struct
#1 "migrate_parsetree_ast_io.ml"
# 1 "src/migrate_parsetree_ast_io.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ast =
  | Impl : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.structure = 'concrete) * 'concrete -> ast
  | Intf : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.signature = 'concrete) * 'concrete -> ast

type filename = string

let magic_length = String.length Ast_402.Config.ast_impl_magic_number

let read_magic ic =
  let buf = Bytes.create magic_length in
  let len = input ic buf 0 magic_length in
  let s = Bytes.sub_string buf 0 len in
  if len = magic_length then
    Ok s
  else
    Error s

type read_error =
  | Not_a_binary_ast of string
  | Unknown_version of string

let find_magic magic =
  let rec loop = function
    | [] ->
        let prefix = String.sub magic 0 9 in
        if prefix = String.sub Ast_402.Config.ast_impl_magic_number 0 9 ||
           prefix = String.sub Ast_402.Config.ast_intf_magic_number 0 9 then
          Error (Unknown_version magic)
        else
          Error (Not_a_binary_ast magic)
    | (module Frontend : Migrate_parsetree_versions.OCaml_version) :: tail ->
        if Frontend.Ast.Config.ast_impl_magic_number = magic then
          Ok (fun x -> Impl ((module Frontend), Obj.obj x))
        else if Frontend.Ast.Config.ast_intf_magic_number = magic then
          Ok (fun x -> Intf ((module Frontend), Obj.obj x))
        else
          loop tail
  in
  loop Migrate_parsetree_versions.all_versions

let from_channel ic =
  match read_magic ic with
  | Error s -> Error (Not_a_binary_ast s)
  | Ok s ->
    match find_magic s with
    | Ok inj ->
      let filename : filename = input_value ic in
      let payload = inj (input_value ic) in
      Ok (filename, payload)
    | Error _ as e  -> e

let from_bytes bytes pos =
  if Bytes.length bytes - pos < magic_length then
    Error (Not_a_binary_ast "")
  else
    let magic = Bytes.to_string (Bytes.sub bytes pos magic_length) in
    match find_magic magic with
    | Ok inj ->
      let filename_pos = pos + magic_length in
      let filename : filename = Marshal.from_bytes bytes filename_pos in
      let payload_pos = filename_pos + Marshal.total_size bytes filename_pos in
      let payload = inj (Marshal.from_bytes bytes payload_pos) in
      Ok (filename, payload)
    | Error _ as e -> e

let decompose_ast = function
  | Impl ((module Frontend), tree) ->
      (Frontend.Ast.Config.ast_impl_magic_number, Obj.repr tree)
  | Intf ((module Frontend), tree) ->
      (Frontend.Ast.Config.ast_intf_magic_number, Obj.repr tree)

let to_channel oc (filename : filename) x =
  let magic_number, payload = decompose_ast x in
  output_string oc magic_number;
  output_value oc filename;
  output_value oc payload

let to_bytes (filename : filename) x =
  let magic_number, payload = decompose_ast x in
  Bytes.cat (
    Bytes.cat
      (Bytes.of_string magic_number)
      (Marshal.to_bytes filename [])
  ) (Marshal.to_bytes payload [])

end
module Migrate_parsetree_driver : sig 
#1 "migrate_parsetree_driver.mli"
# 1 "src/migrate_parsetree_driver.mli"
open Migrate_parsetree_versions

(** {1 State a rewriter can access} *)

type config = {
  tool_name       : string;
  include_dirs    : string list;
  load_path       : string list;
  debug           : bool;
  for_package     : string option;
}

type cookies

val get_cookie
  : cookies
  -> string
  -> 'types ocaml_version -> 'types get_expression option

val set_cookie
  : cookies
  -> string
  -> 'types ocaml_version -> 'types get_expression
  -> unit

(** {1 Registering rewriters} *)

type 'types rewriter = config -> cookies -> 'types get_mapper

val register
  :  name:string
  -> ?reset_args:(unit -> unit) -> ?args:(Arg.key * Arg.spec * Arg.doc) list
  -> 'types ocaml_version -> 'types rewriter
  -> unit

(** {1 Running registered rewriters} *)

val run_as_ast_mapper : string list -> Ast_mapper.mapper

val run_as_ppx_rewriter : unit -> 'a

val run_main : unit -> 'a

end = struct
#1 "migrate_parsetree_driver.ml"
# 1 "src/migrate_parsetree_driver.ml"
open Migrate_parsetree_versions
module Ast_io = Migrate_parsetree_ast_io

(** {1 State a rewriter can access} *)

type config = {
  tool_name: string;
  include_dirs : string list;
  load_path : string list;
  debug : bool;
  for_package : string option;
}

type cookie = Cookie : 'types ocaml_version * 'types get_expression -> cookie

type cookies = (string, cookie) Hashtbl.t

let get_cookie table name version =
  match
    match Hashtbl.find table name with
    | result -> Some result
    | exception Not_found ->
        match Ast_mapper.get_cookie name with
        | None -> None
        | Some expr -> Some (Cookie ((module OCaml_current), expr))
  with
  | None -> None
  | Some (Cookie (version', expr)) ->
    Some ((migrate version' version).copy_expression expr)

let set_cookie table name version expr =
  Hashtbl.replace table name (Cookie (version, expr))

let apply_cookies table =
  Hashtbl.iter (fun name (Cookie (version, expr)) ->
      Ast_mapper.set_cookie name
        ((migrate version (module OCaml_current)).copy_expression expr)
    ) table

let initial_state () : config * cookies =
  {
    tool_name = Ast_mapper.tool_name ();
    include_dirs = !Clflags.include_dirs;
    load_path = !Config.load_path;
    debug = !Clflags.debug;
    for_package = !Clflags.for_package
  }, Hashtbl.create 3

(** {1 Registering rewriters} *)

type 'types rewriter = config -> cookies -> 'types get_mapper

type rewriter_group =
    Rewriters : 'types ocaml_version * (string * 'types rewriter) list -> rewriter_group

let uniq_rewriter = Hashtbl.create 7
let registered_rewriters = ref []

let uniq_arg = Hashtbl.create 7
let registered_args_reset = ref []
let registered_args = ref []

type ('types, 'version, 'rewriter) is_rewriter =
  | Is_rewriter : ('types, 'types ocaml_version, 'types rewriter) is_rewriter

let add_rewriter
    (type types) (type version) (type rewriter)
    (Is_rewriter : (types, version, rewriter) is_rewriter)
    (version : version) name (rewriter : rewriter) =
  let rec add_rewriter = function
  | [] -> [Rewriters (version, [name, rewriter])]
  | (Rewriters (version', rewriters) as x) :: xs ->
      match compare_ocaml_version version version' with
      | Eq -> Rewriters (version', (name, rewriter) :: rewriters) :: xs
      | Lt -> Rewriters (version, [name, rewriter]) :: x :: xs
      | Gt -> x :: add_rewriter xs
  in
  add_rewriter

let register ~name ?reset_args ?(args=[]) version rewriter =
  (* Validate name *)
  if name = "" then
    invalid_arg "Migrate_parsetree_driver.register: name is empty";
  if Hashtbl.mem uniq_rewriter name then
    invalid_arg ("Migrate_parsetree_driver.register: rewriter " ^ name ^ " has already been registered")
  else Hashtbl.add uniq_rewriter name ();
  (* Validate arguments *)
  List.iter (fun (arg_name, _, _) ->
      match Hashtbl.find uniq_arg arg_name with
      | other_rewriter ->
          invalid_arg (Printf.sprintf
                         "Migrate_parsetree_driver.register: argument %s is used by %s and %s" arg_name name other_rewriter)
      | exception Not_found ->
          Hashtbl.add uniq_arg arg_name name
    ) args;
  (* Register *)
  begin match reset_args with
  | None -> ()
  | Some f -> registered_args_reset := f :: !registered_args_reset
  end;
  registered_args := List.rev_append args !registered_args;
  registered_rewriters :=
    add_rewriter Is_rewriter version name rewriter !registered_rewriters

(** {1 Accessing or running registered rewriters} *)

type ('types, 'version, 'tree) is_signature =
    Signature : ('types, 'types ocaml_version, 'types get_signature) is_signature

type ('types, 'version, 'tree) is_structure =
    Structure : ('types, 'types ocaml_version, 'types get_structure) is_structure

let rec rewrite_signature
  : type types version tree.
    config -> cookies ->
    (types, version, tree) is_signature -> version -> tree ->
    rewriter_group list -> Parsetree.signature
  = fun (type types) (type version) (type tree)
    config cookies
    (Signature : (types, version, tree) is_signature)
    (version : version)
    (tree : tree)
    -> function
      | [] -> (migrate version (module OCaml_current)).copy_signature tree
      | Rewriters (version', rewriters) :: rest ->
          let rewrite (_name, rewriter) tree =
            let (module Version) = version' in
            Version.Ast.map_signature (rewriter config cookies) tree
          in
          let tree = (migrate version version').copy_signature tree in
          let tree = List.fold_right rewrite rewriters tree in
          rewrite_signature config cookies Signature version' tree rest

let rec rewrite_structure
  : type types version tree.
    config -> cookies ->
    (types, version, tree) is_structure -> version -> tree ->
    rewriter_group list -> Parsetree.structure
  = fun (type types) (type version) (type tree)
    config cookies
    (Structure : (types, version, tree) is_structure)
    (version : version)
    (tree : tree)
    -> function
      | [] -> (migrate version (module OCaml_current)).copy_structure tree
      | Rewriters (version', rewriters) :: rest ->
          let rewriter (_name, rewriter) tree =
            let (module Version) = version' in
            Version.Ast.map_structure (rewriter config cookies) tree
          in
          let tree = (migrate version version').copy_structure tree in
          let tree = List.fold_right rewriter rewriters tree in
          rewrite_structure config cookies Structure version' tree rest

let run_as_ast_mapper args =
  let spec = List.rev !registered_args in
  let args, usage =
    let me = Filename.basename Sys.executable_name in
    let args = match args with "--as-ppx" :: args -> args | args -> args in
    (Array.of_list (me :: args),
     Printf.sprintf "%s [options] <input ast file> <output ast file>" me)
  in
  match
    Arg.parse_argv args spec
      (fun arg -> raise (Arg.Bad (Printf.sprintf "invalid argument %S" arg)))
      usage
  with
  | exception (Arg.Help msg) ->
      prerr_endline msg;
      exit 1
  | () ->
      OCaml_current.Ast.make_top_mapper
        ~signature:(fun sg ->
            let config, cookies = initial_state () in
            let sg = rewrite_signature config cookies
                Signature (module OCaml_current) sg !registered_rewriters in
            apply_cookies cookies;
            sg
          )
        ~structure:(fun str ->
            let config, cookies = initial_state () in
            let str = rewrite_structure config cookies
                Structure (module OCaml_current) str !registered_rewriters in
            apply_cookies cookies;
            str
          )

let protectx x ~finally ~f =
  match f x with
  | y -> finally x; y
  | exception e -> finally x; raise e

let with_file_in fn ~f =
  protectx (open_in_bin fn) ~finally:close_in ~f

let with_file_out fn ~f =
  protectx (open_out_bin fn) ~finally:close_out ~f

type ('a, 'b) intf_or_impl =
  | Intf of 'a
  | Impl of 'b

let guess_file_kind fn =
  if Filename.check_suffix fn ".ml" then
    Impl fn
  else if Filename.check_suffix fn ".mli" then
    Intf fn
  else
    Location.raise_errorf ~loc:(Location.in_file fn)
      "I can't decide whether %s is an implementation or interface file"
      fn

let check_kind fn ~expected ~got =
  let describe = function
    | Intf _ -> "interface"
    | Impl _ -> "implementation"
  in
  match expected, got with
  | Impl _, Impl _
  | Intf _, Intf _ -> ()
  | _ ->
    Location.raise_errorf ~loc:(Location.in_file fn)
      "Expected an %s got an %s instead"
      (describe expected)
      (describe got)

let load_file file =
  let fn =
    match file with
    | Intf fn -> fn
    | Impl fn -> fn
  in
  with_file_in fn ~f:(fun ic ->
    match Ast_io.from_channel ic with
    | Ok (fn, Ast_io.Intf ((module V), sg)) ->
      check_kind fn ~expected:file ~got:(Intf ());
      (* We need to convert to the current version in order to interpret the cookies using
         [Ast_mapper.drop_ppx_context_*] from the compiler *)
      (fn, Intf ((migrate (module V) (module OCaml_current)).copy_signature sg))
    | Ok (fn, Ast_io.Impl ((module V), st)) ->
      check_kind fn ~expected:file ~got:(Impl ());
      (fn, Impl ((migrate (module V) (module OCaml_current)).copy_structure st))
    | Error (Ast_io.Unknown_version _) ->
      Location.raise_errorf ~loc:(Location.in_file fn)
        "File is a binary ast for an unknown version of OCaml"
    | Error (Ast_io.Not_a_binary_ast prefix_read_from_file) ->
      (* To test if a file is a binary AST file, we have to read the first few bytes of
         the file.

         If it is not a binary AST, we have to parse these bytes and the rest of the file
         as source code. To do that, we prefill the lexbuf buffer with what we read from
         the file to do the test. *)
      let lexbuf = Lexing.from_channel ic in
      let len = String.length prefix_read_from_file in
      String.blit prefix_read_from_file 0 lexbuf.Lexing.lex_buffer 0 len;
      lexbuf.Lexing.lex_buffer_len <- len;
      lexbuf.Lexing.lex_curr_p <-
        { Lexing.
          pos_fname = fn
        ; pos_lnum  = 1
        ; pos_bol   = 0
        ; pos_cnum  = 0
        };
      Location.input_name := fn;
      if Filename.check_suffix fn ".ml" then
        (fn, Impl (Parse.implementation lexbuf))
      else if Filename.check_suffix fn ".mli" then
        (fn, Intf (Parse.interface lexbuf))
      else
        (* TODO: add support for -intf and -impl *)
        Location.raise_errorf ~loc:(Location.in_file fn)
          "I can't decide whether %s is an implementation or interface file"
          fn)

let with_output output ~f =
  match output with
  | None -> f stdout
  | Some fn -> with_file_out fn ~f

let process_file ~config ~output ~dump_ast file =
  let cookies = Hashtbl.create 3 in
  let fn, ast = load_file file in
  let ast =
    match ast with
    | Intf sg ->
      let sg = Ast_mapper.drop_ppx_context_sig ~restore:true sg in
      let sg =
        rewrite_signature config cookies Signature
          (module OCaml_current) sg !registered_rewriters
      in
      apply_cookies cookies;
      Intf (sg, Ast_mapper.add_ppx_context_sig ~tool_name:config.tool_name sg)
    | Impl st ->
      let st = Ast_mapper.drop_ppx_context_str ~restore:true st in
      let st =
        rewrite_structure config cookies Structure
          (module OCaml_current) st !registered_rewriters
      in
      apply_cookies cookies;
      Impl (st, Ast_mapper.add_ppx_context_str ~tool_name:config.tool_name st)
  in
  with_output output ~f:(fun oc ->
    if dump_ast then begin
      let ast =
        match ast with
        | Intf (_, sg) -> Ast_io.Intf ((module OCaml_current), sg)
        | Impl (_, st) -> Ast_io.Impl ((module OCaml_current), st)
      in
      Ast_io.to_channel oc fn ast
    end else begin
      let ppf = Format.formatter_of_out_channel oc in
      (match ast with
       | Intf (sg, _) -> Pprintast.signature ppf sg
       | Impl (st, _) -> Pprintast.structure ppf st);
      Format.pp_print_newline ppf ()
    end)

let run_as_standalone_driver () =
  let output = ref None in
  let dump_ast = ref false in
  let files = ref [] in
  let set_cookie s =
    match String.index s '=' with
    | exception _ ->
      raise (Arg.Bad "invalid cookie, must be of the form \"<name>=<expr>\"")
    | i ->
      let name = String.sub s 0 i in
      let value = String.sub s (i + 1) (String.length s - i - 1) in
      let input_name = "<command-line>" in
      Location.input_name := input_name;
      let lexbuf = Lexing.from_string value in
      lexbuf.Lexing.lex_curr_p <-
        { Lexing.
          pos_fname = input_name
        ; pos_lnum  = 1
        ; pos_bol   = 0
        ; pos_cnum  = 0
        };
      let expr = Parse.expression lexbuf in
      Ast_mapper.set_cookie name expr
  in
  let spec =
    let as_ppx () =
      raise (Arg.Bad "--as-ppx must be passed as first argument")
    in
    [ "--as-ppx", Arg.Unit as_ppx,
      " Act as a -ppx rewriter"
    ; "--dump-ast", Arg.Set dump_ast,
      " Output a binary AST instead of source code"
    ; "-o", Arg.String (fun o -> output := Some o),
      "FILE Output to this file instead of the standard output"
    ; "--intf", Arg.String (fun fn -> files := Intf fn :: !files),
      "FILE Treat FILE as a .mli file"
    ; "--impl", Arg.String (fun fn -> files := Impl fn :: !files),
      "FILE Treat FILE as a .ml file"
    ; "--cookie", Arg.String set_cookie,
      "NAME=EXPR Set the cookie NAME to EXPR"
    ]
  in
  let spec = Arg.align (spec @ List.rev !registered_args) in
  let me = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "%s [options] [<files>]" me in
  try
    Arg.parse spec (fun anon -> files := guess_file_kind anon :: !files) usage;
    let output = !output in
    let dump_ast = !dump_ast in
    let config =
      (* TODO: we could add -I, -L and -g options to populate these fields. *)
      { tool_name    = "migrate_driver"
      ; include_dirs = []
      ; load_path    = []
      ; debug        = false
      ; for_package  = None
      }
    in
    List.iter (process_file ~config ~output ~dump_ast) (List.rev !files)
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1

let run_as_ppx_rewriter () =
  let a = Sys.argv in
  let n = Array.length a in
  if n <= 2 then begin
    let me = Filename.basename Sys.executable_name in
    Arg.usage (List.rev !registered_args)
      (Printf.sprintf "%s [options] <input ast file> <output ast file>" me);
    exit 2
  end;
  match
    Ast_mapper.apply ~source:a.(n - 2) ~target:a.(n - 1)
      (run_as_ast_mapper (Array.to_list (Array.sub a 1 (n - 3))))
  with
  | () -> exit 0
  | exception (Arg.Bad help) ->
      prerr_endline help;
      exit 1
  | exception exn ->
      Location.report_exception Format.err_formatter exn;
      exit 1

let run_main () =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "--as-ppx" then
    run_as_ppx_rewriter ()
  else
    run_as_standalone_driver ();
  exit 0

end
module Migrate_parsetree
= struct
#1 "migrate_parsetree.ml"
# 1 "src/migrate_parsetree.ml"
(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Shared definitions.
   Mostly errors about features missing in older versions. *)
module Def = Migrate_parsetree_def

(* Copy of OCaml parsetrees *)
module Ast_402 = Ast_402
module Ast_403 = Ast_403
module Ast_404 = Ast_404
module Ast_405 = Ast_405

(* A module for marshalling/unmarshalling arbitrary versions of Asts *)
module Ast_io = Migrate_parsetree_ast_io

(* Manual migration between versions *)
module Migrate_402_403 = Migrate_parsetree_402_403
module Migrate_403_402 = Migrate_parsetree_403_402
module Migrate_403_404 = Migrate_parsetree_403_404
module Migrate_404_403 = Migrate_parsetree_404_403
module Migrate_404_405 = Migrate_parsetree_404_405
module Migrate_405_404 = Migrate_parsetree_405_404

(* An abstraction of OCaml compiler versions *)
module Versions = Migrate_parsetree_versions

(* All versions are compatible with this signature *)
module type OCaml_version = Versions.OCaml_version

module OCaml_402 = Versions.OCaml_402
module OCaml_403 = Versions.OCaml_403
module OCaml_404 = Versions.OCaml_404
module OCaml_405 = Versions.OCaml_405
module OCaml_current = Versions.OCaml_current

(* A Functor taking two OCaml versions and producing a module of functions
   migrating from one to the other. *)
module Convert = Versions.Convert

(* Entrypoints for registering rewriters and making a ppx binary *)
module Driver = Migrate_parsetree_driver

(* Aliases for compiler-libs modules that might be shadowed *)
module Compiler_libs = struct
  module Location = Location
  module Longident = Longident

  module type Asttypes = module type of struct include Asttypes end
  module rec Asttypes : Asttypes = Asttypes

  module type Parsetree = module type of struct include Parsetree end
  module rec Parsetree : Parsetree = Parsetree

  module Docstrings = Docstrings
  module Ast_helper = Ast_helper
  module Ast_mapper = Ast_mapper
end

end
module Jsx_ppx_to_current
= struct
#1 "jsx_ppx_to_current.ml"
(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

(* isolated module for use by reactjs_jsx_ppx and reactjs_jsx_ppx_2, so that when bspacking, we don't have to drag in all the deps *)

open Migrate_parsetree

module To_current = Convert(OCaml_404)(OCaml_current)

end
module Reactjs_jsx_ppx_2
= struct
#1 "reactjs_jsx_ppx_2.ml"
(* transform `div props1::a props2::b children::[foo, bar] () [@JSX]` into
   `ReactDOMRe.createElement "div" props::[%bs.obj {props1: 1, props2: b}] [|foo, bar|]`.

   For the old behavior:
   Don't transform the upper-cased case: `Foo.createElement foo::bar children::[] () [@JSX]`.

   For the new behavior:
   transform the upper-cased case `Foo.createElement key::a ref::b foo::bar children::[] () [@JSX]` into
  `ReasonReact.element key::a ref::b (Foo.make foo::bar [||] [@JSX])`
*)

(* Why do we need a transform, instead of just using the original format?
   Because that one currently doesn't work well for the existing React.js *)
open Migrate_parsetree
open Ast_404

open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let listToArray lst =
  let rec listToArray' lst accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match lst with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> accum
    | {
        pexp_desc = Pexp_construct (
          {txt = Lident "::"},
          Some {pexp_desc = Pexp_tuple (v::acc::[])}
        )
    } -> listToArray' acc (v::accum)
    | _ -> raise (
        Invalid_argument "JSX: the `children` prop must be a literal list (of react elements)."
      ) in
  listToArray' lst [] |> List.rev

let extractChildrenForDOMElements ?(removeLastPositionUnit=false) ~loc propsAndChildren =
  let rec allButLast_ lst acc = match lst with
    | [] -> []
    | (Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})::[] -> acc
    | (Nolabel, _)::rest -> raise (Invalid_argument "JSX: found non-labelled argument before the last position")
    | arg::rest -> allButLast_ rest (arg::acc)
  in
  let allButLast lst = allButLast_ lst [] |> List.rev in
  match (List.partition (fun (label, expr) -> label = Labelled "children") propsAndChildren) with
  | ((label, childrenExpr)::[], props) ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | ([], props) ->
    (* no children provided? Place a placeholder list (don't forgot we're talking about DOM element conversion here only) *)
    (Exp.construct ~loc {loc; txt = Lident "[]"} None, if removeLastPositionUnit then allButLast props else props)
  | (moreThanOneChild, props) -> raise (Invalid_argument "JSX: somehow there's more than one `children` label")

(* TODO: some line number might still be wrong *)
let jsxMapper () =

  let oldJSX mapper loc attrs callExpression callArguments =
    Exp.apply
      ~loc
      ~attrs
      callExpression
      (
        callArguments |> List.map (fun (label, expr) -> (label, mapper.expr mapper expr))
      ) in

  let newJSX modulePath mapper loc attrs callExpression callArguments =
    let (children, argsWithLabels) =
      extractChildrenForDOMElements ~loc ~removeLastPositionUnit:true callArguments in
    let argIsKeyRef = function
      | (Labelled ("key" | "ref") , _) -> true
      | _ -> false in
    let (argsKeyRef, argsForMake) = List.partition argIsKeyRef argsWithLabels in
    let childrenExpr =
      Exp.array (
        listToArray children |> List.map (fun a -> mapper.expr mapper a)
      ) in
    let args = argsForMake @ [ (Nolabel, childrenExpr) ] in
    let wrapWithReasonReactElement e = (* ReasonReact.element ::key ::ref (...) *)
      Exp.apply
        ~loc
        (Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "element")})
        (argsKeyRef @ [(Nolabel, e)]) in
    Exp.apply
      ~loc
      ~attrs
      (* Foo.make *)
      (Exp.ident ~loc {loc; txt = Ldot (modulePath, "make")})
      args
    |> wrapWithReasonReactElement in

  let lowercaseCaller mapper loc attrs callArguments id  =
    let (children, propsWithLabels) =
      extractChildrenForDOMElements ~loc callArguments in
    let componentNameExpr =
      Exp.constant ~loc (Pconst_string (id, None)) in
    let childrenExpr =
      Exp.array (
        listToArray children |> List.map (fun a -> mapper.expr mapper a)
      ) in
    let args = match propsWithLabels with
      | [theUnitArgumentAtEnd] ->
        [
          (* "div" *)
          (Nolabel, componentNameExpr);
          (* [|moreCreateElementCallsHere|] *)
          (Nolabel, childrenExpr)
        ]
      | nonEmptyProps ->
        let propsCall =
          Exp.apply
            ~loc
            (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "props")})
            (nonEmptyProps |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)))
        in
        [
          (* "div" *)
          (Nolabel, componentNameExpr);
          (* ReactDOMRe.props className:blabla foo::bar () *)
          (Labelled "props", propsCall);
          (* [|moreCreateElementCallsHere|] *)
          (Nolabel, childrenExpr)
        ] in
    Exp.apply
      ~loc
      (* throw away the [@JSX] attribute and keep the others, if any *)
      ~attrs
      (* ReactDOMRe.createDOMElement *)
      (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
      args in

  let useNewJsxBehavior = ref None in

  let structure =
    (fun mapper structure -> match structure with
      (* match against [@@@bs.config {foo, jsx: ...}] *)
      | {
            pstr_loc;
            pstr_desc = Pstr_attribute (
              ({txt = "bs.config"} as bsConfigLabel),
              PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (recordFields, b)} as innerConfigRecord, a)} as configRecord]
            )
          }::restOfStructure -> begin
            let (jsxField, recordFieldsWithoutJsx) = recordFields |> List.partition (fun ({txt}, _) -> txt = Lident "jsx") in
            match (jsxField, recordFieldsWithoutJsx) with
            (* no jsx config found *)
            | ([], _) -> default_mapper.structure mapper structure
            (* {jsx: 1 | 2} *)
            | ((_, {pexp_desc = Pexp_constant (Pconst_integer (version, _))})::rest, recordFieldsWithoutJsx) -> begin
                (match version with
                | "1" -> useNewJsxBehavior := Some 1
                | "2" -> useNewJsxBehavior := Some 2
                | _ -> raise (Invalid_argument "JSX: the file-level bs.config's jsx version must be either 1 or 2"));
                match recordFieldsWithoutJsx with
                (* record empty now, remove the whole bs.config attribute *)
                | [] -> default_mapper.structure mapper restOfStructure
                | fields -> default_mapper.structure mapper ({
                  pstr_loc;
                  pstr_desc = Pstr_attribute (
                    bsConfigLabel,
                    PStr [{configRecord with pstr_desc = Pstr_eval ({innerConfigRecord with pexp_desc = Pexp_record (fields, b)}, a)}]
                  )
                }::restOfStructure)
              end
          | (_, recordFieldsWithoutJsx) -> raise (Invalid_argument "JSX: the file-level bs.config's {jsx: ...} config accepts only a version number")
        end
      | _ -> default_mapper.structure mapper structure
    ) in

  let handleJsxCall mapper callExpression callArguments attrs =
    (match callExpression.pexp_desc with
     | Pexp_ident caller ->
       (match caller with
        | {txt = Lident "createElement"} ->
          raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")
        (* Foo.createElement prop1::foo prop2:bar children::[] () *)
        | {loc; txt = Ldot (modulePath, "createElement")} ->
          let f = match !useNewJsxBehavior with
            | Some 1 -> oldJSX
            | Some 2 -> newJSX modulePath
            | Some _ -> assert false
            | None -> newJSX modulePath
          in f mapper loc attrs callExpression callArguments
        (* div prop1::foo prop2:bar children::[bla] () *)
        (* turn that into ReactDOMRe.createElement props::(ReactDOMRe.props props1::foo props2::bar ()) [|bla|] *)
        | {loc; txt = Lident id} ->
          lowercaseCaller mapper loc attrs callArguments id
        | {txt = Ldot (_, anythingNotCreateElement)} ->
          raise (
            Invalid_argument
              ("JSX: the JSX attribute should be attached to a `YourModuleName.createElement` call. We saw `"
               ^ anythingNotCreateElement
               ^ "` instead"
              )
          )
        | {txt = Lapply _} ->
          (* don't think there's ever a case where this is reached *)
          raise (
            Invalid_argument "JSX: encountered a weird case while processing the code. Please report this!"
          )
       )
     | anythingElseThanIdent ->
       raise (
         Invalid_argument "JSX: `createElement` should be preceeded by a simple, direct module name."
       )
    ) in

  let expr =
    (fun mapper expression -> match expression with
       (* Function application with the @JSX attribute? *)
       |
         {
           pexp_desc = Pexp_apply (callExpression, callArguments);
           pexp_attributes
         } ->
         let (jsxAttribute, attributesWithoutJSX) = List.partition (fun (attribute, _) -> attribute.txt = "JSX") pexp_attributes in
         (match (jsxAttribute, attributesWithoutJSX) with
         (* no JSX attribute *)
         | ([], _) -> default_mapper.expr mapper expression
         | (_, attributesWithoutJSX) -> handleJsxCall mapper callExpression callArguments attributesWithoutJSX)
       (* Delegate to the default mapper, a deep identity traversal *)
       | e -> default_mapper.expr mapper e) in

  Jsx_ppx_to_current.To_current.copy_mapper { default_mapper with structure; expr }

let () = Compiler_libs.Ast_mapper.register "JSX" (fun _argv -> jsxMapper ())

end
