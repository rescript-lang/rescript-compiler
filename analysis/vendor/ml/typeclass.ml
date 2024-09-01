(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Parsetree
open Asttypes
open Types
open Typetexp
open Format

type 'a class_info = {
  cls_id : Ident.t;
  cls_id_loc : string loc;
  cls_decl : class_declaration;
  cls_ty_id : Ident.t;
  cls_ty_decl : class_type_declaration;
  cls_obj_id : Ident.t;
  cls_obj_abbr : type_declaration;
  cls_typesharp_id : Ident.t;
  cls_abbr : type_declaration;
  cls_arity : int;
  cls_pub_methods : string list;
  cls_info : 'a;
}

type class_type_info = {
  clsty_ty_id : Ident.t;
  clsty_id_loc : string loc;
  clsty_ty_decl : class_type_declaration;
  clsty_obj_id : Ident.t;
  clsty_obj_abbr : type_declaration;
  clsty_typesharp_id : Ident.t;
  clsty_abbr : type_declaration;
  clsty_info : Typedtree.class_type_declaration;
}

type error =
    Unconsistent_constraint of (type_expr * type_expr) list
  | Field_type_mismatch of string * string * (type_expr * type_expr) list
  | Structure_expected of class_type
  | Pattern_type_clash of type_expr
  | Repeated_parameter
  | Unbound_class_type_2 of Longident.t
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Constructor_type_mismatch of string * (type_expr * type_expr) list
  | Virtual_class of bool * bool * string list * string list
  | Parameter_arity_mismatch of Longident.t * int * int
  | Parameter_mismatch of (type_expr * type_expr) list
  | Bad_parameters of Ident.t * type_expr * type_expr
 
  | Unbound_type_var of (formatter -> unit) * Ctype.closed_class_failure
  | Non_generalizable_class of Ident.t * Types.class_declaration
  | Cannot_coerce_self of type_expr
  | Non_collapsable_conjunction of
      Ident.t * Types.class_declaration * (type_expr * type_expr) list
  | No_overriding of string * string
 

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

open Typedtree

let ctyp desc typ env loc =
  { ctyp_desc = desc; ctyp_type = typ; ctyp_loc = loc; ctyp_env = env;
    ctyp_attributes = [] }

                       (**********************)
                       (*  Useful constants  *)
                       (**********************)


(*
   Self type have a dummy private method, thus preventing it to become
   closed.
*)
let dummy_method = Btype.dummy_method

(*
   Path associated to the temporary class type of a class being typed
   (its constructor is not available).
*)
let unbound_class = Path.Pident (Ident.create "*undef*")


                (************************************)
                (*  Some operations on class types  *)
                (************************************)


(* Fully expand the head of a class type *)
let rec scrape_class_type =
  function
    Cty_constr (_, _, cty) -> scrape_class_type cty
  | cty                     -> cty

(* Generalize a class type *)
let rec generalize_class_type gen =
  function
    Cty_constr (_, params, cty) ->
      List.iter gen params;
      generalize_class_type gen cty
  | Cty_signature {csig_self = sty; csig_vars = vars; csig_inher = inher} ->
      gen sty;
      Vars.iter (fun _ (_, _, ty) -> gen ty) vars;
      List.iter (fun (_,tl) -> List.iter gen tl) inher
  | Cty_arrow (_, ty, cty) ->
      gen ty;
      generalize_class_type gen cty

let generalize_class_type vars =
  let gen = if vars then Ctype.generalize else Ctype.generalize_structure in
  generalize_class_type gen

(* Return the virtual methods of a class type *)
let virtual_methods sign =
  let (fields, _) =
    Ctype.flatten_fields (Ctype.object_fields sign.Types.csig_self)
  in
  List.fold_left
    (fun virt (lab, _, _) ->
       if lab = dummy_method then virt else
       if Concr.mem lab sign.csig_concr then virt else
       lab::virt)
    [] fields

(* Return the constructor type associated to a class type *)
let rec constructor_type constr cty =
  match cty with
    Cty_constr (_, _, cty) ->
      constructor_type constr cty
  | Cty_signature _ ->
      constr
  | Cty_arrow (l, ty, cty) ->
      Ctype.newty (Tarrow (l, ty, constructor_type constr cty, Cok))

let rec class_body cty =
  match cty with
    Cty_constr _ ->
      cty (* Only class bodies can be abbreviated *)
  | Cty_signature _ ->
      cty
  | Cty_arrow (_, _, cty) ->
      class_body cty


(* Check that all type variables are generalizable *)
(* Use Env.empty to prevent expansion of recursively defined object types;
   cf. typing-poly/poly.ml *)
let rec closed_class_type =
  function
    Cty_constr (_, params, _) ->
      List.for_all (Ctype.closed_schema Env.empty) params
  | Cty_signature sign ->
      Ctype.closed_schema Env.empty sign.csig_self
        &&
      Vars.fold (fun _ (_, _, ty) cc -> Ctype.closed_schema Env.empty ty && cc)
        sign.csig_vars
        true
  | Cty_arrow (_, ty, cty) ->
      Ctype.closed_schema Env.empty ty
        &&
      closed_class_type cty

let closed_class cty =
  List.for_all (Ctype.closed_schema Env.empty) cty.cty_params
    &&
  closed_class_type cty.cty_type

let rec limited_generalize rv =
  function
    Cty_constr (_path, params, cty) ->
      List.iter (Ctype.limited_generalize rv) params;
      limited_generalize rv cty
  | Cty_signature sign ->
      Ctype.limited_generalize rv sign.csig_self;
      Vars.iter (fun _ (_, _, ty) -> Ctype.limited_generalize rv ty)
        sign.csig_vars;
      List.iter (fun (_, tl) -> List.iter (Ctype.limited_generalize rv) tl)
        sign.csig_inher
  | Cty_arrow (_, ty, cty) ->
      Ctype.limited_generalize rv ty;
      limited_generalize rv cty



                (***********************************)
                (*  Primitives for typing classes  *)
                (***********************************)



(* Enter an instance variable in the environment *)
let concr_vals vars =
  Vars.fold
    (fun id (_, vf, _) s -> if vf = Virtual then s else Concr.add id s)
    vars Concr.empty

let inheritance self_type env ovf concr_meths warn_vals loc parent =
  match scrape_class_type parent with
    Cty_signature cl_sig ->

      (* Methods *)
      begin try
        Ctype.unify env self_type cl_sig.csig_self
      with Ctype.Unify trace ->
        match trace with
          _::_::_::({desc = Tfield(n, _, _, _)}, _)::rem ->
            raise(Error(loc, env, Field_type_mismatch ("method", n, rem)))
        | _ ->
            assert false
      end;

      (* Overriding *)
      let over_meths = Concr.inter cl_sig.csig_concr concr_meths in
      let concr_vals = concr_vals cl_sig.csig_vars in
      let over_vals = Concr.inter concr_vals warn_vals in
      begin match ovf with
        Some Fresh ->
          let cname =
            match parent with
              Cty_constr (p, _, _) -> Path.name p
            | _ -> "inherited"
          in
          if not (Concr.is_empty over_meths) then
            Location.prerr_warning loc
              (Warnings.Method_override (cname :: Concr.elements over_meths));
          if not (Concr.is_empty over_vals) then
            Location.prerr_warning loc
              (Warnings.Instance_variable_override
                 (cname :: Concr.elements over_vals));
      | Some Override
        when Concr.is_empty over_meths && Concr.is_empty over_vals ->
        raise (Error(loc, env, No_overriding ("","")))
      | _ -> ()
      end;

      let concr_meths = Concr.union cl_sig.csig_concr concr_meths
      and warn_vals = Concr.union concr_vals warn_vals in

      (cl_sig, concr_meths, warn_vals)

  | _ ->
      raise(Error(loc, env, Structure_expected parent))


let delayed_meth_specs = ref []

let declare_method val_env meths self_type lab priv sty loc =
  let (_, ty') =
     Ctype.filter_self_method val_env lab priv meths self_type
  in
  let unif ty =
    try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
      raise(Error(loc, val_env, Field_type_mismatch ("method", lab, trace)))
  in
  let sty = Ast_helper.Typ.force_poly sty in
  match sty.ptyp_desc, priv with
    Ptyp_poly ([],sty'), Public ->
(* TODO: we moved the [transl_simple_type_univars] outside of the lazy,
so that we can get an immediate value. Is that correct ? Ask Jacques. *)
      let returned_cty = ctyp Ttyp_any (Ctype.newty Tnil) val_env loc in
      delayed_meth_specs :=
      Warnings.mk_lazy (fun () ->
            let cty = transl_simple_type_univars val_env sty' in
            let ty = cty.ctyp_type in
            unif ty;
            returned_cty.ctyp_desc <- Ttyp_poly ([], cty);
            returned_cty.ctyp_type <- ty;
          ) ::
      !delayed_meth_specs;
      returned_cty
  | _ ->
      let cty = transl_simple_type val_env false sty in
      let ty = cty.ctyp_type in
      unif ty;
      cty

let type_constraint val_env sty sty' loc =
  let cty  = transl_simple_type val_env false sty in
  let ty = cty.ctyp_type in
  let cty' = transl_simple_type val_env false sty' in
  let ty' = cty'.ctyp_type in
  begin
    try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
        raise(Error(loc, val_env, Unconsistent_constraint trace));
  end;
  (cty, cty')


(*******************************)

let add_val lab (mut, virt, ty) val_sig =
  let virt =
    try
      let (_mut', virt', _ty') = Vars.find lab val_sig in
      if virt' = Concrete then virt' else virt
    with Not_found -> virt
  in
  Vars.add lab (mut, virt, ty) val_sig

let rec class_type_field env self_type meths arg ctf =
  Builtin_attributes.warning_scope ctf.pctf_attributes
    (fun () -> class_type_field_aux env self_type meths arg ctf)

and class_type_field_aux env self_type meths
    (fields, val_sig, concr_meths, inher) ctf =

  let loc = ctf.pctf_loc in
  let mkctf desc =
    { ctf_desc = desc; ctf_loc = loc; ctf_attributes = ctf.pctf_attributes }
  in
  match ctf.pctf_desc with
    Pctf_inherit sparent ->
      let parent = class_type env sparent in
      let inher =
        match parent.cltyp_type with
          Cty_constr (p, tl, _) -> (p, tl) :: inher
        | _ -> inher
      in
      let (cl_sig, concr_meths, _) =
        inheritance self_type env None concr_meths Concr.empty sparent.pcty_loc
          parent.cltyp_type
      in
      let val_sig =
        Vars.fold add_val cl_sig.csig_vars val_sig in
      (mkctf (Tctf_inherit parent) :: fields,
       val_sig, concr_meths, inher)

  | Pctf_val ({txt=lab}, mut, virt, sty) ->
      let cty = transl_simple_type env false sty in
      let ty = cty.ctyp_type in
      (mkctf (Tctf_val (lab, mut, virt, cty)) :: fields,
      add_val lab (mut, virt, ty) val_sig, concr_meths, inher)

  | Pctf_method ({txt=lab}, priv, virt, sty)  ->
      let cty =
        declare_method env meths self_type lab priv sty  ctf.pctf_loc in
      let concr_meths =
        match virt with
        | Concrete -> Concr.add lab concr_meths
        | Virtual -> concr_meths
      in
      (mkctf (Tctf_method (lab, priv, virt, cty)) :: fields,
        val_sig, concr_meths, inher)

  | Pctf_constraint (sty, sty') ->
      let (cty, cty') = type_constraint env sty sty'  ctf.pctf_loc in
      (mkctf (Tctf_constraint (cty, cty')) :: fields,
        val_sig, concr_meths, inher)

  | Pctf_attribute x ->
      Builtin_attributes.warning_attribute x;
      (mkctf (Tctf_attribute x) :: fields,
        val_sig, concr_meths, inher)

  | Pctf_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and class_signature env {pcsig_self=sty; pcsig_fields=sign} =
  let meths = ref Meths.empty in
  let self_cty = transl_simple_type env false sty in
  let self_cty = { self_cty with
    ctyp_type = Ctype.expand_head env self_cty.ctyp_type } in
  let self_type =  self_cty.ctyp_type in

  (* Check that the binder is a correct type, and introduce a dummy
     method preventing self type from being closed. *)
  let dummy_obj = Ctype.newvar () in
  Ctype.unify env (Ctype.filter_method env dummy_method Private dummy_obj)
    (Ctype.newty (Ttuple []));
  begin try
    Ctype.unify env self_type dummy_obj
  with Ctype.Unify _ ->
    raise(Error(sty.ptyp_loc, env, Pattern_type_clash self_type))
  end;

  (* Class type fields *)
  let (rev_fields, val_sig, concr_meths, inher) =
    Builtin_attributes.warning_scope []
      (fun () ->
         List.fold_left (class_type_field env self_type meths)
           ([], Vars.empty, Concr.empty, [])
           sign
      )
  in
  let cty =   {csig_self = self_type;
   csig_vars = val_sig;
   csig_concr = concr_meths;
   csig_inher = inher}
  in
  { csig_self = self_cty;
    csig_fields = List.rev rev_fields;
    csig_type = cty;
  }

and class_type env scty =
  Builtin_attributes.warning_scope scty.pcty_attributes
    (fun () -> class_type_aux env scty)

and class_type_aux env scty =
  let cltyp desc typ =
    {
     cltyp_desc = desc;
     cltyp_type = typ;
     cltyp_loc = scty.pcty_loc;
     cltyp_env = env;
     cltyp_attributes = scty.pcty_attributes;
    }
  in
  match scty.pcty_desc with
    Pcty_constr (lid, styl) ->
      let (path, decl) = Typetexp.find_class_type env scty.pcty_loc lid.txt in
      if Path.same decl.clty_path unbound_class then
        raise(Error(scty.pcty_loc, env, Unbound_class_type_2 lid.txt));
      let (params, clty) =
        Ctype.instance_class decl.clty_params decl.clty_type
      in
      if List.length params <> List.length styl then
        raise(Error(scty.pcty_loc, env,
                    Parameter_arity_mismatch (lid.txt, List.length params,
                                                   List.length styl)));
      let ctys = List.map2
        (fun sty ty ->
          let cty' = transl_simple_type env false sty in
          let ty' = cty'.ctyp_type in
          begin
           try Ctype.unify env ty' ty with Ctype.Unify trace ->
                  raise(Error(sty.ptyp_loc, env, Parameter_mismatch trace))
            end;
            cty'
        )       styl params
      in
      let typ = Cty_constr (path, params, clty) in
      cltyp (Tcty_constr ( path, lid , ctys)) typ

  | Pcty_signature pcsig ->
      let clsig = class_signature env pcsig in
      let typ = Cty_signature clsig.csig_type in
      cltyp (Tcty_signature clsig) typ

  | Pcty_arrow (l, sty, scty) ->
      let cty = transl_simple_type env false sty in
      let ty = cty.ctyp_type in
      let ty =
        if Btype.is_optional l
        then Ctype.newty (Tconstr(Predef.path_option,[ty], ref Mnil))
        else ty in
      let clty = class_type env scty in
      let typ = Cty_arrow (l, ty, clty.cltyp_type) in
      cltyp (Tcty_arrow (l, cty, clty)) typ

  | Pcty_open (ovf, lid, e) ->
      let (path, newenv) = !Typecore.type_open ovf env scty.pcty_loc lid in
      let clty = class_type newenv e in
      cltyp (Tcty_open (ovf, path, lid, newenv, clty)) clty.cltyp_type

  | Pcty_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

let class_type env scty =
  delayed_meth_specs := [];
  let cty = class_type env scty in
  List.iter Lazy.force (List.rev !delayed_meth_specs);
  delayed_meth_specs := [];
  cty

(*******************************)


(*******************************)

(* Approximate the type of the constructor to allow recursive use *)
(* of optional parameters                                         *)

let var_option = Predef.type_option (Btype.newgenvar ())


let rec approx_description ct =
  match ct.pcty_desc with
    Pcty_arrow (l, _, ct) ->
      let arg =
        if Btype.is_optional l then Ctype.instance_def var_option
        else Ctype.newvar () in
      Ctype.newty (Tarrow (l, arg, approx_description ct, Cok))
  | _ -> Ctype.newvar ()

(*******************************)

let temp_abbrev loc env id arity =
  let params = ref [] in
  for _i = 1 to arity do
    params := Ctype.newvar () :: !params
  done;
  let ty = Ctype.newobj (Ctype.newvar ()) in
  let env =
    Env.add_type ~check:true id
      {type_params = !params;
       type_arity = arity;
       type_kind = Type_abstract;
       type_private = Public;
       type_manifest = Some ty;
       type_variance = Misc.replicate_list Variance.full arity;
       type_newtype_level = None;
       type_loc = loc;
       type_attributes = []; (* or keep attrs from the class decl? *)
       type_immediate = false;
       type_unboxed = unboxed_false_default_false;
      }
      env
  in
  (!params, ty, env)

let initial_env  approx
    (res, env) (cl, id, ty_id, obj_id, cl_id) =
  (* Temporary abbreviations *)
  let arity = List.length cl.pci_params in
  let (obj_params, obj_ty, env) = temp_abbrev cl.pci_loc env obj_id arity in
  let (cl_params, cl_ty, env) = temp_abbrev cl.pci_loc env cl_id arity in

  (* Temporary type for the class constructor *)
  let constr_type = approx cl.pci_expr in
  let dummy_cty =
    Cty_signature
      { csig_self = Ctype.newvar ();
        csig_vars = Vars.empty;
        csig_concr = Concr.empty;
        csig_inher = [] }
  in
  let dummy_class =
    {Types.cty_params = [];             (* Dummy value *)
     cty_variance = [];
     cty_type = dummy_cty;        (* Dummy value *)
     cty_path = unbound_class;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some constr_type
       end;
     cty_loc = Location.none;
     cty_attributes = [];
    }
  in
  let env =
    Env.add_cltype ty_id
      {clty_params = [];            (* Dummy value *)
       clty_variance = [];
       clty_type = dummy_cty;       (* Dummy value *)
       clty_path = unbound_class;
       clty_loc = Location.none;
       clty_attributes = [];
      } env
  in
  ((cl, id, ty_id,
    obj_id, obj_params, obj_ty,
    cl_id, cl_params, cl_ty,
    constr_type, dummy_class)::res,
   env)

let class_infos  kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_id, cl_params, cl_ty,
     constr_type, dummy_class)
    (res, env) =

  reset_type_variables ();
  Ctype.begin_class_def ();

  (* Introduce class parameters *)
  let ci_params =
    let make_param (sty, v) =
      try
          (transl_type_param env sty, v)
      with Already_bound ->
        raise(Error(sty.ptyp_loc, env, Repeated_parameter))
    in
      List.map make_param cl.pci_params
  in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) ci_params in

  (* Allow self coercions (only for class declarations) *)
  let coercion_locs = ref [] in

  (* Type the class expression *)
  let (expr, typ) =
    try
      Typecore.self_coercion :=
        (Path.Pident obj_id, coercion_locs) :: !Typecore.self_coercion;
      let res = kind env cl.pci_expr in
      Typecore.self_coercion := List.tl !Typecore.self_coercion;
      res
    with exn ->
      Typecore.self_coercion := []; raise exn
  in

  Ctype.end_def ();

  let sty = Ctype.self_type typ in

  (* First generalize the type of the dummy method (cf PR#6123) *)
  let (fields, _) = Ctype.flatten_fields (Ctype.object_fields sty) in
  List.iter (fun (met, _, ty) -> if met = dummy_method then Ctype.generalize ty)
    fields;
  (* Generalize the row variable *)
  let rv = Ctype.row_variable sty in
  List.iter (Ctype.limited_generalize rv) params;
  limited_generalize rv typ;

  (* Check the abbreviation for the object type *)
  let (obj_params', obj_type) = Ctype.instance_class params typ in
  let constr = Ctype.newconstr (Path.Pident obj_id) obj_params in
  begin
    let ty = Ctype.self_type obj_type in
    Ctype.hide_private_methods ty;
    Ctype.close_object ty;
    begin try
      List.iter2 (Ctype.unify env) obj_params obj_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
            Bad_parameters (obj_id, constr,
                            Ctype.newconstr (Path.Pident obj_id)
                                            obj_params')))
    end;
    begin try
      Ctype.unify env ty constr
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
        Abbrev_type_clash (constr, ty, Ctype.expand_head env constr)))
    end
  end;

  (* Check the other temporary abbreviation (#-type) *)
  begin
    let (cl_params', cl_type) = Ctype.instance_class params typ in
    let ty = Ctype.self_type cl_type in
    Ctype.hide_private_methods ty;
    Ctype.set_object_name obj_id (Ctype.row_variable ty) cl_params ty;
    begin try
      List.iter2 (Ctype.unify env) cl_params cl_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
            Bad_parameters (cl_id,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params')))
    end;
    begin try
      Ctype.unify env ty cl_ty
    with Ctype.Unify _ ->
      let constr = Ctype.newconstr (Path.Pident cl_id) params in
      raise(Error(cl.pci_loc, env, Abbrev_type_clash (constr, ty, cl_ty)))
    end
  end;

  (* Type of the class constructor *)
  begin try
    Ctype.unify env
      (constructor_type constr obj_type)
      (Ctype.instance env constr_type)
  with Ctype.Unify trace ->
    raise(Error(cl.pci_loc, env,
                Constructor_type_mismatch (cl.pci_name.txt, trace)))
  end;

  (* Class and class type temporary definitions *)
  let cty_variance = List.map (fun _ -> Variance.full) params in
  let cltydef =
    {clty_params = params; clty_type = class_body typ;
     clty_variance = cty_variance;
     clty_path = Path.Pident obj_id;
     clty_loc = cl.pci_loc;
     clty_attributes = cl.pci_attributes;
    }
  in
  dummy_class.cty_type <- typ;
  let env =
    Env.add_cltype ty_id cltydef (
     env)
  in

  if cl.pci_virt = Concrete then begin
    let sign = Ctype.signature_of_class_type typ in
    let mets = virtual_methods sign in
    let vals =
      Vars.fold
        (fun name (_mut, vr, _ty) l -> if vr = Virtual then name :: l else l)
        sign.csig_vars [] in
    if mets <> []  || vals <> [] then
      raise(Error(cl.pci_loc, env, Virtual_class(false, false, mets,
                                                 vals)));
  end;

  (* Misc. *)
  let arity = Ctype.class_type_arity typ in
  let pub_meths =
    let (fields, _) =
      Ctype.flatten_fields (Ctype.object_fields (Ctype.expand_head env obj_ty))
    in
    List.map (function (lab, _, _) -> lab) fields
  in

  (* Final definitions *)
  let (params', typ') = Ctype.instance_class params typ in
  let cltydef =
    {clty_params = params'; clty_type = class_body typ';
     clty_variance = cty_variance;
     clty_path = Path.Pident obj_id;
     clty_loc = cl.pci_loc;
     clty_attributes = cl.pci_attributes;
    }
  and clty =
    {cty_params = params'; cty_type = typ';
     cty_variance = cty_variance;
     cty_path = Path.Pident obj_id;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some (Ctype.instance env constr_type)
       end;
     cty_loc = cl.pci_loc;
     cty_attributes = cl.pci_attributes;
    }
  in
  let obj_abbr =
    {type_params = obj_params;
     type_arity = List.length obj_params;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = Some obj_ty;
     type_variance = List.map (fun _ -> Variance.full) obj_params;
     type_newtype_level = None;
     type_loc = cl.pci_loc;
     type_attributes = []; (* or keep attrs from cl? *)
     type_immediate = false;
     type_unboxed = unboxed_false_default_false;
    }
  in
  let (cl_params, cl_ty) =
    Ctype.instance_parameterized_type params (Ctype.self_type typ)
  in
  Ctype.hide_private_methods cl_ty;
  Ctype.set_object_name obj_id (Ctype.row_variable cl_ty) cl_params cl_ty;
  let cl_abbr =
    {type_params = cl_params;
     type_arity = List.length cl_params;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = Some cl_ty;
     type_variance = List.map (fun _ -> Variance.full) cl_params;
     type_newtype_level = None;
     type_loc = cl.pci_loc;
     type_attributes = []; (* or keep attrs from cl? *)
     type_immediate = false;
     type_unboxed = unboxed_false_default_false;
    }
  in
  ((cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr, ci_params,
    arity, pub_meths, List.rev !coercion_locs, expr) :: res,
   env)

let final_decl env 
    (cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr, ci_params,
     arity, pub_meths, coe, expr) =

  begin try Ctype.collapse_conj_params env clty.cty_params
  with Ctype.Unify trace ->
    raise(Error(cl.pci_loc, env, Non_collapsable_conjunction (id, clty, trace)))
  end;

  List.iter Ctype.generalize clty.cty_params;
  generalize_class_type true clty.cty_type;
  Misc.may  Ctype.generalize clty.cty_new;
  List.iter Ctype.generalize obj_abbr.type_params;
  Misc.may  Ctype.generalize obj_abbr.type_manifest;
  List.iter Ctype.generalize cl_abbr.type_params;
  Misc.may  Ctype.generalize cl_abbr.type_manifest;

  if not (closed_class clty) then
    raise(Error(cl.pci_loc, env, Non_generalizable_class (id, clty)));

  begin match
    Ctype.closed_class clty.cty_params
      (Ctype.signature_of_class_type clty.cty_type)
  with
    None        -> ()
  | Some reason ->
      let printer =
        function ppf -> Printtyp.cltype_declaration id ppf cltydef
      in
      raise(Error(cl.pci_loc, env, Unbound_type_var(printer, reason)))
  end;

  (id, cl.pci_name, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
   arity, pub_meths, coe, expr,
   { ci_loc = cl.pci_loc;
     ci_virt = cl.pci_virt;
     ci_params = ci_params;
(* TODO : check that we have the correct use of identifiers *)
     ci_id_name = cl.pci_name;
     ci_id_class = id;
     ci_id_class_type = ty_id;
     ci_id_object = obj_id;
     ci_id_typehash = cl_id;
     ci_expr = expr;
     ci_decl = clty;
     ci_type_decl = cltydef;
     ci_attributes = cl.pci_attributes;
 })
(*   (cl.pci_variance, cl.pci_loc)) *)

let class_infos  kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_id, cl_params, cl_ty,
     constr_type, dummy_class)
    (res, env) =
  Builtin_attributes.warning_scope cl.pci_attributes
    (fun () ->
       class_infos  kind
         (cl, id, ty_id,
          obj_id, obj_params, obj_ty,
          cl_id, cl_params, cl_ty,
          constr_type, dummy_class)
         (res, env)
    )

let extract_type_decls
    (_id, _id_loc, clty, _ty_id, cltydef, obj_id, obj_abbr, _cl_id, cl_abbr,
     _arity, _pub_meths, _coe, _expr, required) decls =
  (obj_id, obj_abbr, cl_abbr, clty, cltydef, required) :: decls

let merge_type_decls
    (id, id_loc, _clty, ty_id, _cltydef, obj_id, _obj_abbr, cl_id, _cl_abbr,
     arity, pub_meths, coe, expr, req) (obj_abbr, cl_abbr, clty, cltydef) =
  (id, id_loc, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
   arity, pub_meths, coe, expr, req)

let final_env env
    (_id, _id_loc, _clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
     _arity, _pub_meths, _coe, _expr, _req) =
  (* Add definitions after cleaning them *)
  Env.add_type ~check:true obj_id
    (Subst.type_declaration Subst.identity obj_abbr) (
  Env.add_type ~check:true cl_id
    (Subst.type_declaration Subst.identity cl_abbr) (
  Env.add_cltype ty_id (Subst.cltype_declaration Subst.identity cltydef)   
   env))

(* Check that #c is coercible to c if there is a self-coercion *)
let check_coercions env
    (id, id_loc, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
     arity, pub_meths, coercion_locs, _expr, req) =
  begin match coercion_locs with [] -> ()
  | loc :: _ ->
      let cl_ty, obj_ty =
        match cl_abbr.type_manifest, obj_abbr.type_manifest with
          Some cl_ab, Some obj_ab ->
            let cl_params, cl_ty =
              Ctype.instance_parameterized_type cl_abbr.type_params cl_ab
            and obj_params, obj_ty =
              Ctype.instance_parameterized_type obj_abbr.type_params obj_ab
            in
            List.iter2 (Ctype.unify env) cl_params obj_params;
            cl_ty, obj_ty
        | _ -> assert false
      in
      begin try Ctype.subtype env cl_ty obj_ty ()
      with Ctype.Subtype (tr1, tr2) ->
        raise(Typecore.Error(loc, env, Typecore.Not_subtype(tr1, tr2)))
      end;
      if not (Ctype.opened_object cl_ty) then
        raise(Error(loc, env, Cannot_coerce_self obj_ty))
  end;
  {cls_id = id;
   cls_id_loc = id_loc;
   cls_decl = clty;
   cls_ty_id = ty_id;
   cls_ty_decl = cltydef;
   cls_obj_id = obj_id;
   cls_obj_abbr = obj_abbr;
   cls_typesharp_id = cl_id;
   cls_abbr = cl_abbr;
   cls_arity = arity;
   cls_pub_methods = pub_meths;
   cls_info=req}

(*******************************)
(* FIXME: [define_class] is always [false] here *)
let type_classes  approx kind env cls =
  let cls =
    List.map
      (function cl ->
         (cl,
          Ident.create cl.pci_name.txt, Ident.create cl.pci_name.txt,
          Ident.create cl.pci_name.txt, Ident.create ("#" ^ cl.pci_name.txt)))
      cls
  in
  Ctype.init_def (Ident.current_time ());
  Ctype.begin_class_def ();
  let (res, env) =
    List.fold_left (initial_env  approx) ([], env) cls
  in
  let (res, env) =
    List.fold_right (class_infos  kind) res ([], env)
  in
  Ctype.end_def ();
  let res = List.rev_map (final_decl env ) res in
  let decls = List.fold_right extract_type_decls res [] in
  let decls = Typedecl.compute_variance_decls env decls in
  let res = List.map2 merge_type_decls res decls in
  let env = List.fold_left final_env env res in
  let res = List.map (check_coercions env) res in
  (res, env)


let class_description env sexpr =
  let expr = class_type env sexpr in
  (expr, expr.cltyp_type)



let class_type_declarations env cls =
  let (decls, env) =
    type_classes  approx_description class_description env cls
  in
  (List.map
     (fun decl ->
        {clsty_ty_id = decl.cls_ty_id;
         clsty_id_loc = decl.cls_id_loc;
         clsty_ty_decl = decl.cls_ty_decl;
         clsty_obj_id = decl.cls_obj_id;
         clsty_obj_abbr = decl.cls_obj_abbr;
         clsty_typesharp_id = decl.cls_typesharp_id;
         clsty_abbr = decl.cls_abbr;
         clsty_info = decl.cls_info})
     decls,
   env)



(*******************************)

(* Approximate the class declaration as class ['params] id = object end *)
let approx_class sdecl =
  let open Ast_helper in
  let self' = Typ.any () in
  let clty' = Cty.signature ~loc:sdecl.pci_expr.pcty_loc (Csig.mk self' []) in
  { sdecl with pci_expr = clty' }

let approx_class_declarations env sdecls =
  fst (class_type_declarations env (List.map approx_class sdecls))

(*******************************)

(* Error report *)

open Format

let report_error env ppf = function
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Unconsistent_constraint trace ->
      fprintf ppf "The class constraints are not consistent.@.";
      Printtyp.report_unification_error ppf env trace
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "is not compatible with type")
  | Field_type_mismatch (k, m, trace) ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The %s %s@ has type" k m)
        (function ppf ->
           fprintf ppf "but is expected to have type")
  | Structure_expected clty ->
      fprintf ppf
        "@[This class expression is not a class structure; it has type@ %a@]"
        Printtyp.class_type clty
  | Pattern_type_clash ty ->
      (* XXX Trace *)
      (* XXX Revoir message d'erreur | Improve error message *)
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[%s@ %a@]"
        "This pattern cannot match self: it only matches values of type"
        Printtyp.type_expr ty
  | Unbound_class_type_2 cl ->
      fprintf ppf "@[The class type@ %a@ is not yet completely defined@]"
      Printtyp.longident cl
  | Abbrev_type_clash (abbrev, actual, expected) ->
      (* XXX Afficher une trace ? | Print a trace? *)
      Printtyp.reset_and_mark_loops_list [abbrev; actual; expected];
      fprintf ppf "@[The abbreviation@ %a@ expands to type@ %a@ \
       but is used with type@ %a@]"
       Printtyp.type_expr abbrev
       Printtyp.type_expr actual
       Printtyp.type_expr expected
  | Constructor_type_mismatch (c, trace) ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The expression \"new %s\" has type" c)
        (function ppf ->
           fprintf ppf "but is used with type")
  | Virtual_class (cl, imm, mets, vals) ->
      let print_mets ppf mets =
        List.iter (function met -> fprintf ppf "@ %s" met) mets in
      let missings =
        match mets, vals with
          [], _ -> "variables"
        | _, [] -> "methods"
        | _ -> "methods and variables"
      in
      let print_msg ppf =
        if imm then fprintf ppf "This object has virtual %s" missings
        else if cl then fprintf ppf "This class should be virtual"
        else fprintf ppf "This class type should be virtual"
      in
      fprintf ppf
        "@[%t.@ @[<2>The following %s are undefined :%a@]@]"
        print_msg missings print_mets (mets @ vals)
  | Parameter_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
        "@[The class constructor %a@ expects %i type argument(s),@ \
           but is here applied to %i type argument(s)@]"
        Printtyp.longident lid expected provided
  | Parameter_mismatch trace ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The type parameter")
        (function ppf ->
           fprintf ppf "does not meet its constraint: it should be")
  | Bad_parameters (id, params, cstrs) ->
      Printtyp.reset_and_mark_loops_list [params; cstrs];
      fprintf ppf
        "@[The abbreviation %a@ is used with parameters@ %a@ \
           which are incompatible with constraints@ %a@]"
        Printtyp.ident id Printtyp.type_expr params Printtyp.type_expr cstrs
  | Unbound_type_var (printer, reason) ->
      let print_common ppf kind ty0 real lab ty =
        let ty1 =
          if real then ty0 else Btype.newgenty(Tobject(ty0, ref None)) in
        List.iter Printtyp.mark_loops [ty; ty1];
        fprintf ppf
          "The %s %s@ has type@;<1 2>%a@ where@ %a@ is unbound"
            kind lab Printtyp.type_expr ty Printtyp.type_expr ty0
      in
      let print_reason ppf = function
      | Ctype.CC_Method (ty0, real, lab, ty) ->
          print_common ppf "method" ty0 real lab ty
      | Ctype.CC_Value (ty0, real, lab, ty) ->
          print_common ppf "instance variable" ty0 real lab ty
      in
      Printtyp.reset ();
      fprintf ppf
        "@[<v>@[Some type variables are unbound in this type:@;<1 2>%t@]@ \
              @[%a@]@]"
       printer print_reason reason
  | Non_generalizable_class (id, clty) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (Printtyp.class_declaration id) clty
  | Cannot_coerce_self ty ->
      fprintf ppf
        "@[The type of self cannot be coerced to@ \
           the type of the current class:@ %a.@.\
           Some occurrences are contravariant@]"
        Printtyp.type_scheme ty
  | Non_collapsable_conjunction (id, clty, trace) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains non-collapsible conjunctive types in constraints@]"
        (Printtyp.class_declaration id) clty;
      Printtyp.report_unification_error ppf env trace
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "is not compatible with type")
  | No_overriding (_, "") ->
      fprintf ppf "@[This inheritance does not override any method@ %s@]"
        "instance variable"
  | No_overriding (kind, name) ->
      fprintf ppf "@[The %s `%s'@ has no previous definition@]" kind name

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error env ppf err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (Location.error_of_printer loc (report_error env) err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )
