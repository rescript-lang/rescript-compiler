(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








let string_of_lambda = Format.asprintf "%a" Lam_print.lambda 

let string_of_primitive = Format.asprintf "%a" Lam_print.primitive






(*
let add_required_modules ( x : Ident.t list) (meta : Lam_stats.t) = 
  let meta_require_modules = meta.required_modules in
  List.iter (fun x -> add meta_require_modules (Lam_module_ident.of_ml x)) x 
*)
  
(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda (s : Lam.t Ident_map.t) lam =
  let rec subst (x : Lam.t) : Lam.t =
    match x with 
    | Lvar id as l ->
      Ident_map.find_default id s l
    | Lconst sc as l -> l
    | Lapply{fn; args; loc; status} -> 
      Lam.apply (subst fn) (List.map subst args) loc status
    | Lfunction {arity; function_kind; params; body} -> 
      Lam.function_ ~arity ~function_kind  ~params ~body:(subst body)
    | Llet(str, id, arg, body) -> 
      Lam.let_ str id (subst arg) (subst body)
    | Lletrec(decl, body) -> 
      Lam.letrec (List.map subst_decl decl) (subst body)
    | Lprim { primitive ; args; loc} -> 
      Lam.prim ~primitive ~args:(List.map subst args) loc
    | Lam.Lglobal_module _ -> x  
    | Lswitch(arg, sw) ->
      Lam.switch (subst arg)
        {sw with sw_consts = List.map subst_case sw.sw_consts;
                 sw_blocks = List.map subst_case sw.sw_blocks;
                 sw_failaction = subst_opt  sw.sw_failaction; }
    | Lstringswitch (arg,cases,default) ->
      Lam.stringswitch
        (subst arg) (List.map subst_strcase cases) (subst_opt default)
    | Lstaticraise (i,args)
      ->  Lam.staticraise i (List.map subst args)
    | Lstaticcatch(e1, io, e2)
      -> Lam.staticcatch (subst e1) io (subst e2)
    | Ltrywith(e1, exn, e2)
      -> Lam.try_ (subst e1) exn (subst e2)
    | Lifthenelse(e1, e2, e3)
      -> Lam.if_ (subst e1) (subst e2) (subst e3)
    | Lsequence(e1, e2)
      -> Lam.seq (subst e1) (subst e2)
    | Lwhile(e1, e2) 
      -> Lam.while_ (subst e1) (subst e2)
    | Lfor(v, e1, e2, dir, e3) 
      -> Lam.for_ v (subst e1) (subst e2) dir (subst e3)
    | Lassign(id, e) -> 
      Lam.assign id (subst e)
    | Lsend (k, met, obj, args, loc) ->
      Lam.send k (subst met) (subst obj) (List.map subst args) loc
    | Lifused (v, e) -> Lam.ifused v (subst e)
  and subst_decl (id, exp) = (id, subst exp)
  and subst_case (key, case) = (key, subst case)
  and subst_strcase (key, case) = (key, subst case)
  and subst_opt = function
    | None -> None
    | Some e -> Some (subst e)
  in subst lam

(* 
    It's impossible to have a case like below:
   {[
     (let export_f = ... in export_f)
   ]}
    Even so, it's still correct
*)
let refine_let
    ~kind param
    (arg : Lam.t) (l : Lam.t)  : Lam.t =

  match (kind : Lam.let_kind ), arg, l  with 
  | _, _, Lvar w when Ident.same w param 
    (* let k = xx in k
      there is no [rec] so [k] would not appear in [xx]
     *)
    -> arg (* TODO: optimize here -- it's safe to do substitution here *)
  | _, _, Lprim {primitive ; args =  [Lvar w]; loc ; _} when Ident.same w param 
                                                          &&  (function | Lam.Pmakeblock _ -> false | _ ->  true) primitive
    (* don't inline inside a block *)
    ->  Lam.prim ~primitive ~args:[arg]  loc 
  (* we can not do this substitution when capttured *)
  (* | _, Lvar _, _ -> (\** let u = h in xxx*\) *)
  (*     (\* assert false *\) *)
  (*     Ext_log.err "@[substitution >> @]@."; *)
  (*     let v= subst_lambda (Ident_map.singleton param arg ) l in *)
  (*     Ext_log.err "@[substitution << @]@."; *)
  (* v *)
  | _, _, Lapply {fn; args = [Lvar w]; loc; status} when
   Ident.same w param &&
    (not (Lam.hit_any_variables (Ident_set.singleton param) fn ))
   -> 
    (** does not work for multiple args since 
        evaluation order unspecified, does not apply 
        for [js] in general, since the scope of js ir is loosen

        here we remove the definition of [param]
        {[ let k = v in (body) k 
        ]}
        #1667 make sure body does not hit k 
    *)
    Lam.apply fn [arg] loc status
  | (Strict | StrictOpt ),
    ( Lvar _    | Lconst  _ | 
      Lprim {primitive = Pfield _ ;  
             args = [ Lglobal_module _ ]; _}) , _ ->
    (* (match arg with  *)
    (* | Lconst _ ->  *)
    (*     Ext_log.err "@[%a %s@]@."  *)
    (*       Ident.print param (string_of_lambda arg) *)
    (* | _ -> ()); *)
    (* No side effect and does not depend on store,
        since function evaluation is always delayed
    *)
    Lam.let_ Alias param arg l
  | ( (Strict | StrictOpt ) ), (Lfunction _ ), _ ->
    (*It can be promoted to [Alias], however, 
        we don't want to do this, since we don't want the 
        function to be inlined to a block, for example
      {[
        let f = fun _ -> 1 in
        [0, f]
      ]}
        TODO: punish inliner to inline functions 
        into a block 
    *)
    Lam.let_ StrictOpt  param arg l
  (* Not the case, the block itself can have side effects 
      we can apply [no_side_effects] pass 
      | Some Strict, Lprim(Pmakeblock (_,_,Immutable),_) ->  
        Llet(StrictOpt, param, arg, l) 
  *)      
  | Strict, _ ,_  when Lam_analysis.no_side_effects arg ->
    Lam.let_ StrictOpt param arg l
  | Variable, _, _ -> 
    Lam.let_ Variable  param arg l
  | kind, _, _ -> 
    Lam.let_ kind  param arg l
  (* | None , _, _ -> 
    Lam.let_ Strict param arg  l *)

let alias_ident_or_global (meta : Lam_stats.t) (k:Ident.t) (v:Ident.t) 
    (v_kind : Lam_id_kind.t) (let_kind : Lam.let_kind) =
  (** treat rec as Strict, k is assigned to v 
      {[ let k = v ]}
  *)
  begin 
    match v_kind with 
    | NA ->
      begin 
        match Ident_hashtbl.find_opt meta.ident_tbl v  with 
        | None -> ()
        | Some ident_info -> Ident_hashtbl.add meta.ident_tbl k ident_info
      end
    | ident_info -> Ident_hashtbl.add meta.ident_tbl k ident_info
  end ;
  (* share -- it is safe to share most properties,
      for arity, we might be careful, only [Alias] can share,
      since two values have same type, can have different arities
      TODO: check with reference pass, it might break 
      since it will create new identifier, we can avoid such issue??

      actually arity is a dynamic property, for a reference, it can 
      be changed across 
      we should treat
      reference specially. or maybe we should track any 
      mutable reference
  *)
  begin match let_kind with 
    | Alias -> 
      if not @@ Ident_set.mem k meta.export_idents 
      then
        Ident_hashtbl.add meta.alias_tbl k v 
    (** For [export_idents], we don't want to do such simplification
        if we do substitution, then it will affect exports...
    *)
    | Strict | StrictOpt(*can discard but not be substitued *) | Variable  -> ()
  end




(* How we destruct the immutable block 
   depend on the block name itself, 
   good hints to do aggressive destructing
   1. the variable is not exported
      like [matched] -- these are blocks constructed temporary
   2. how the variable is used 
      if it is guarateed to be 
      - non export 
      - and non escaped (there is no place it is used as a whole)
      then we can always destruct it 
      if some fields are used in multiple places, we can create 
      a temporary field 

   3. It would be nice that when the block is mutable, its 
       mutable fields are explicit, since wen can not inline an mutable block access
*)

let element_of_lambda (lam : Lam.t) : Lam_id_kind.element = 
  match lam with 
  | Lvar _ 
  | Lconst _ 
  | Lprim {primitive = Pfield _ ; 
           args =  [ Lglobal_module _ ];
           _} -> SimpleForm lam
  (* | Lfunction _  *)
  | _ -> NA 

let kind_of_lambda_block kind (xs : Lam.t list) : Lam_id_kind.t = 
  ImmutableBlock( Ext_array.of_list_map (fun x -> 
  element_of_lambda x ) xs , kind)

let field_flatten_get
   lam v i (tbl : Lam_id_kind.t Ident_hashtbl.t) : Lam.t =
  match Ident_hashtbl.find_opt tbl v  with 
  | Some (Module g) -> 
    Lam.prim ~primitive:(Pfield (i, Lambda.Fld_na)) 
      ~args:[ Lam.global_module g ] Location.none
  | Some (ImmutableBlock (arr, _)) -> 
    begin match arr.(i) with 
      | NA -> lam ()
      | SimpleForm l -> l
      | exception _ -> lam ()
    end
  | Some (Constant (Const_block (_,_,ls))) -> 
    begin match Ext_list.nth_opt ls i with 
    | None -> lam  ()
    | Some x -> Lam.const x
    end
  | Some _
  | None -> lam ()


(* TODO: check that if label belongs to a different 
    namesape
*)
let count = ref 0 

let generate_label ?(name="") ()  = 
  incr count; 
  Printf.sprintf "%s_tailcall_%04d" name !count

let log_counter = ref 0


let dump env ext  lam = 
#if BS_COMPILER_IN_BROWSER || (undefined BS_DEBUG) then
      lam
#else
   if Js_config.is_same_file ()
    then 
      (* ATTENTION: easy to introduce a bug during refactoring when forgeting `begin` `end`*)
      begin 
        incr log_counter;
        Ext_log.dwarn __LOC__ "\n@[[TIME:]%s: %f@]@." ext (Sys.time () *. 1000.);
        Lam_print.seriaize env 
          (Ext_path.chop_extension 
             ~loc:__LOC__ 
             (Js_config.get_current_file ()) ^ 
           (Printf.sprintf ".%02d%s.lam" !log_counter ext)
          ) lam;
      end;
  lam
#end      
  




let print_ident_set fmt s = 
  Format.fprintf fmt   "@[<v>{%a}@]@."
    (fun fmt s   -> 
       Ident_set.iter 
         (fun e -> Format.fprintf fmt "@[<v>%a@],@ " Ident.print e) s
    )
    s     




let is_function (lam : Lam.t) = 
  match lam with 
  | Lfunction _ -> true | _ -> false

let not_function (lam : Lam.t) = 
  match lam with 
  | Lfunction _ -> false | _ -> true 

(* TODO: we need create 
   1. a smart [let] combinator, reusable beta-reduction 
   2. [lapply fn args info] 
   here [fn] should get the last tail
   for example 
   {[
     lapply (let a = 3 in let b = 4 in fun x y -> x + y) 2 3 
   ]}   
*)








