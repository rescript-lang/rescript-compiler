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








let string_of_lambda = Format.asprintf "%a" Printlambda.lambda 

let string_of_primitive = Format.asprintf "%a" Printlambda.primitive


(* TODO: not very efficient .. *)
exception Cyclic 
      
let toplogical (get_deps : Ident.t -> Ident_set.t) (libs : Ident.t list) : Ident.t list =
  let rec aux acc later todo round_progress =
    match todo, later with
    | [], [] ->  acc
    | [], _ ->
        if round_progress
        then aux acc todo later false
        else raise Cyclic
    | x::xs, _ ->
        if Ident_set.for_all (fun dep -> x == dep || List.mem dep acc) (get_deps x)
        then aux (x::acc) later xs true
        else aux acc (x::later) xs round_progress
  in
  let starts, todo = List.partition (fun lib -> Ident_set.is_empty @@ get_deps lib) libs in
  aux starts [] todo false

let sort_dag_args  param_args =
  let todos = Ident_map.keys param_args  in
  let idents = Ident_set.of_list  todos in
  let dependencies  : Ident_set.t Ident_map.t = 
    Ident_map.mapi (fun param arg -> Js_fold_basic.depends_j arg idents) param_args in
  try  
    Some (toplogical (fun k -> Ident_map.find k dependencies) todos)
  with Cyclic -> None 



let add_required_module (x : Ident.t) (meta : Lam_stats.meta) = 
  if not @@ Ident.is_predef_exn x then   
    meta.required_modules <- Lam_module_ident.of_ml x :: meta.required_modules 

let add_required_modules ( x : Ident.t list) (meta : Lam_stats.meta) = 
  let required_modules = 
    Ext_list.filter_map 
      (fun x ->
         if Ident.is_predef_exn x then
           None 
         else Some ( Lam_module_ident.of_ml x))  x
    @ meta.required_modules in
  meta.required_modules <- required_modules

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda s lam =
  let rec subst (x : Lambda.lambda) =
    match x with 
    | Lvar id as l ->
      begin 
        try Ident_map.find id s with Not_found -> l 
      end
    | Lconst sc as l -> l
    | Lapply(fn, args, loc) -> Lapply(subst fn, List.map subst args, loc)
    | Lfunction(kind, params, body) -> Lfunction(kind, params, subst body)
    | Llet(str, id, arg, body) -> Llet(str, id, subst arg, subst body)
    | Lletrec(decl, body) -> Lletrec(List.map subst_decl decl, subst body)
    | Lprim(p, args) -> Lam_comb.prim p (List.map subst args)
    | Lswitch(arg, sw) ->
      Lam_comb.switch (subst arg)
        {sw with sw_consts = List.map subst_case sw.sw_consts;
                 sw_blocks = List.map subst_case sw.sw_blocks;
                 sw_failaction = subst_opt  sw.sw_failaction; }
    | Lstringswitch (arg,cases,default) ->
      Lam_comb.stringswitch
        (subst arg) (List.map subst_strcase cases) (subst_opt default)
    | Lstaticraise (i,args)
      ->  Lam_comb.staticraise i (List.map subst args)
    | Lstaticcatch(e1, io, e2)
      -> Lam_comb.staticcatch (subst e1) io (subst e2)
    | Ltrywith(e1, exn, e2)
      -> Lam_comb.try_ (subst e1) exn (subst e2)
    | Lifthenelse(e1, e2, e3)
      -> Lam_comb.if_ (subst e1) (subst e2) (subst e3)
    | Lsequence(e1, e2)
      -> Lam_comb.seq (subst e1) (subst e2)
    | Lwhile(e1, e2) 
      -> Lam_comb.while_ (subst e1) (subst e2)
    | Lfor(v, e1, e2, dir, e3) 
      -> Lam_comb.for_ v (subst e1) (subst e2) dir (subst e3)
    | Lassign(id, e) -> 
      Lam_comb.assign id (subst e)
    | Lsend (k, met, obj, args, loc) ->
      Lam_comb.send k (subst met) (subst obj) (List.map subst args) loc
    | Levent (lam, evt)
      -> Lam_comb.event (subst lam) evt
    | Lifused (v, e) -> Lam_comb.ifused v (subst e)
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
    ?kind param
    (arg : Lambda.lambda) (l : Lambda.lambda)  : Lambda.lambda =

  match (kind : Lambda.let_kind option), arg, l  with 
  | _, _, Lvar w when Ident.same w param (* let k = xx in k *)
    -> arg (* TODO: optimize here -- it's safe to do substitution here *)
  | _, _, Lprim (fn, [Lvar w]) when Ident.same w param 
                                 &&  (function | Lambda.Pmakeblock _ -> false | _ ->  true) fn
    (* don't inline inside a block *)
    ->  Lam_comb.prim fn [arg]
  (* we can not do this substitution when capttured *)
  (* | _, Lvar _, _ -> (\** let u = h in xxx*\) *)
  (*     (\* assert false *\) *)
  (*     Ext_log.err "@[substitution >> @]@."; *)
  (*     let v= subst_lambda (Ident_map.singleton param arg ) l in *)
  (*     Ext_log.err "@[substitution << @]@."; *)
  (* v *)
  | _, _, Lapply (fn, [Lvar w],info) when Ident.same w param -> 
    (** does not work for multiple args since 
        evaluation order unspecified, does not apply 
        for [js] in general, since the scope of js ir is loosen

        here we remove the definition of [param]
    *)
    Lapply(fn, [arg], info)
  | (Some (Strict | StrictOpt ) | None ),
    ( Lvar _    | Lconst  _ | Lprim (Pfield _ , [Lprim (Pgetglobal _ , [])])) , _ ->
    (* (match arg with  *)
    (* | Lconst _ ->  *)
    (*     Ext_log.err "@[%a %s@]@."  *)
    (*       Ident.print param (string_of_lambda arg) *)
    (* | _ -> ()); *)
    (* No side effect and does not depend on store,
        since function evaluation is always delayed
    *)
    Llet(Alias, param,arg, l)
  | (Some (Strict | StrictOpt ) | None ), (Lfunction _ ), _ ->
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
    Llet(StrictOpt, param,arg, l)
  (* Not the case, the block itself can have side effects 
      we can apply [no_side_effects] pass 
      | Some Strict, Lprim(Pmakeblock (_,_,Immutable),_) ->  
        Llet(StrictOpt, param, arg, l) 
  *)      
  | Some Strict, _ ,_  when Lam_analysis.no_side_effects arg ->
    Llet(StrictOpt, param, arg,l)
  | Some Variable, _, _ -> 
    Llet(Variable, param,arg,l) 
  | Some kind, _, _ -> 
    Llet(kind, param,arg,l) 
  | None , _, _ -> 
    Llet(Strict, param, arg , l)

let alias (meta : Lam_stats.meta) (k:Ident.t) (v:Ident.t) 
    (v_kind : Lam_stats.kind) (let_kind : Lambda.let_kind) =
  (** treat rec as Strict, k is assigned to v 
      {[ let k = v ]}
  *)
  begin 
    match v_kind with 
    | NA ->
      begin 
        match Hashtbl.find meta.ident_tbl v  with 
        | exception Not_found -> ()
        | ident_info -> Hashtbl.add meta.ident_tbl k ident_info
      end
    | ident_info -> Hashtbl.add meta.ident_tbl k ident_info
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
        Hashtbl.add meta.alias_tbl k v 
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
       mutable fields are explicit
*)

let element_of_lambda (lam : Lambda.lambda) : Lam_stats.element = 
  match lam with 
  | Lvar _ 
  | Lconst _ 
  | Lprim (Pfield _ , [ Lprim (Pgetglobal _, [])]) -> SimpleForm lam
  (* | Lfunction _  *)
  | _ -> NA 

let kind_of_lambda_block kind (xs : Lambda.lambda list) : Lam_stats.kind = 
  xs 
  |> List.map element_of_lambda 
  |> (fun ls -> Lam_stats.ImmutableBlock (Array.of_list  ls, kind))

let get lam v i tbl : Lambda.lambda =
  match (Hashtbl.find tbl v  : Lam_stats.kind) with 
  | Module g -> 
    Lam_comb.prim (Pfield (i, Lambda.Fld_na)) 
      [Lam_comb.prim (Pgetglobal g) []]
  | ImmutableBlock (arr, _) -> 
    begin match arr.(i) with 
      | NA -> lam 
      | SimpleForm l -> l
    end
  | Constant (Const_block (_,_,ls)) -> 
    Lconst (List.nth  ls i)
  | _ -> lam
  | exception Not_found -> lam 


(* TODO: check that if label belongs to a different 
    namesape
*)
let count = ref 0 

let generate_label ?(name="") ()  = 
  incr count; 
  Printf.sprintf "%s_tailcall_%04d" name !count

let log_counter = ref 0

let dump env ext  lam = 
  incr log_counter ; 
  if Js_config.get_env () <> Browser 
  (* TODO: when no [Browser] detection, it will go through.. bug in js_of_ocaml? *)
  && Js_config.is_same_file ()
  then 
    Printlambda.seriaize env 
      (Ext_filename.chop_extension 
         ~loc:__LOC__ 
         (Js_config.get_current_file ()) ^ 
       (Printf.sprintf ".%02d%s.lam" !log_counter ext)
      ) lam;
  lam



let ident_set_of_list ls = 
  List.fold_left
    (fun acc k -> Ident_set.add k acc ) 
    Ident_set.empty ls 

let print_ident_set fmt s = 
  Format.fprintf fmt   "@[<v>{%a}@]@."
    (fun fmt s   -> 
       Ident_set.iter (fun e -> Format.fprintf fmt "@[<v>%a@],@ " Ident.print e) s
    )
    s     

let mk_apply_info ?(loc = Location.none)  apply_status : Lambda.apply_info =
  { apply_loc = loc; apply_status }



let is_function (lam : Lambda.lambda) = 
  match lam with 
  | Lfunction _ -> true | _ -> false

let not_function (lam : Lambda.lambda) = 
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
let (* rec *) lapply (fn : Lambda.lambda) args info = 
  (* match fn with *)
  (* | Lambda.Lfunction(kind, params, body) -> *)
  (*   let rec aux acc params args = *)
  (*     match params, args with *)
  (*     | [], [] -> acc, body *)
  (*     | [], args' -> acc, lapply body args' info *)
  (*     | params' , [] -> *)
  (*       acc, Lambda.Lfunction(kind, params', body) *)
  (*     | x::xs, y::ys -> aux ((x,y)::acc) xs ys in *)
  (*   let env, rest = aux [] params args in *)
  (*   List.fold_left *)
  (*     (fun acc (v,e) -> *)
  (*        Lambda.Llet (Strict,v,e ,acc) ) rest env *)

  (* | _ -> *)  Lambda.Lapply (fn, args, info )
(*
  let f x y =  x + y 
  Invariant: there is no currying 
  here since f's arity is 2, no side effect 
  f 3 --> function(y) -> f 3 y 
*)
let eta_conversion n info fn args = 
  let extra_args = Ext_list.init n
      (fun _ ->   (Ident.create Literals.param)) in
  let extra_lambdas = List.map (fun x -> Lambda.Lvar x) extra_args in
  begin match List.fold_right (fun lam (acc, bind) ->
      match lam with
      | Lambda.Lvar _
      | Lconst (Const_base _ | Const_pointer _ | Const_immstring _ ) 
      | Lprim (Lambda.Pfield (_), [Lprim (Lambda.Pgetglobal _, _)] )
      | Lfunction _ 
        ->
        (lam :: acc, bind)
      | _ ->
        let v = Ident.create Literals.partial_arg in
        (Lambda.Lvar v :: acc),  ((v, lam) :: bind)
    ) (fn::args) ([],[])   with 
  | fn::args , bindings ->

    let rest : Lambda.lambda = 
      Lfunction(Curried, extra_args,
                lapply fn (args @ extra_lambdas) info) in
    List.fold_left (fun lam (id,x) ->
        Lambda.Llet (Strict, id, x,lam)
      ) rest bindings
  | _, _ -> assert false
  end


(* FIXME: application location is important for error message *)
let default_apply_info : Lambda.apply_info = 
  { apply_status = App_na ; apply_loc = Location.none }

