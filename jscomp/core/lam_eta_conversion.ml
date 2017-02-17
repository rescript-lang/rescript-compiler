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


(*
  let f x y =  x + y 
  Invariant: there is no currying 
  here since f's arity is 2, no side effect 
  f 3 --> function(y) -> f 3 y 
*)

(** 
   [transform n loc status fn args]
   n is the number of missing arguments required for [fn].
   Return a function of airty [n]
*) 
let transform_under_supply n loc status fn args = 
  let extra_args = Ext_list.init n
      (fun _ ->   (Ident.create Literals.param)) in
  let extra_lambdas = List.map (fun x -> Lam.var x) extra_args in
  begin match List.fold_right (fun (lam : Lam.t) (acc, bind) ->
      match lam with
      | Lvar _
      | Lconst (Const_base _ | Const_pointer _ | Const_immstring _ ) 
      | Lprim {primitive = Pfield _;
               args =  [ Lglobal_module _ ]; _ }
      | Lfunction _ 
        ->
        (lam :: acc, bind)
      | _ ->
        let v = Ident.create Literals.partial_arg in
        (Lam.var v :: acc),  ((v, lam) :: bind)
    ) (fn::args) ([],[])   with 
  | fn :: args, [] -> 
    (* More than no side effect in the [args], 
       we try to avoid computation, so even if 
       [x + y] is side effect free, we need eval it only once 
    *)
    (* TODO: Note we could adjust [fn] if [fn] is already a function
      But it is dangerous to change the arity 
      of an existing function which may cause inconsistency
    *)
    Lam.function_ ~arity:n ~kind:Curried ~params:extra_args
      ~body:(Lam.apply fn (args @ extra_lambdas) 
               loc 
               status
            ) 
  | fn::args , bindings ->

    let rest : Lam.t = 
      Lam.function_ ~arity:n ~kind:Curried ~params:extra_args
        ~body:(Lam.apply fn (args @ extra_lambdas) 
                 loc 
                 status
              ) in
    List.fold_left (fun lam (id,x) ->
        Lam.let_ Strict id x lam
      ) rest bindings
  | _, _ -> assert false
  end

(** Unsafe function, we are changing arity here, it should be applied 
    cautiously, since 
    [let u = f] and we are chaning the arity of [f] it will affect 
    the collection of [u]
*)
let unsafe_adjust_to_arity loc ~to_:(to_:int) ~from:(from:int)
    (fn : Lam.t) = 
  if from = to_ then 
    fn 
  else if to_ = 0 then  
    match fn with 
    | Lfunction{params = [_]; body} -> 
      Lam.function_ ~arity:0 ~kind:Curried 
      ~params:[]
      ~body (* could be only introduced by 
        {[ Pjs_fn_make 0 ]} <- 
        {[ fun [@bs] () -> .. ]}
        *)
    | _ -> 
      let wrapper, new_fn  = 
        match fn with 
        | Lvar _ 
        | Lprim{primitive = Pfield _ ; args = [Lglobal_module _]; _ }
          -> 
          None, fn 
        | _ -> 
          let partial_arg = Ext_ident.create Literals.partial_arg in 
          Some partial_arg, Lam.var partial_arg in 

      let cont = Lam.function_ 
      ~arity:0
      ~kind:Curried 
      ~params:[]
      ~body:(
        Lam.apply new_fn [Lam.unit] loc App_na
      ) in 
      match wrapper with 
      | None -> cont 
      | Some partial_arg 
      -> Lam.let_ Strict partial_arg fn cont 

  else if to_ > from then 
    match fn with 
    | Lfunction{params;body; kind} -> 
      (* {[fun x -> f]} -> 
         {[ fun x y -> f y ]}
      *)
      let extra_args = Ext_list.init (to_ - from) (fun _ -> Ident.create Literals.param) in 
      Lam.function_
        ~arity:to_ 
        ~kind:Curried
        ~params:(params @ extra_args )
        ~body:(Lam.apply body (List.map Lam.var extra_args) loc App_na)
    | _ -> 
      let arity = to_ in 
      let extra_args = Ext_list.init to_  (fun _ -> Ident.create Literals.param ) in 
      let wrapper, new_fn = 
        match fn with 
        | Lvar _ 
        | Lprim {primitive = Pfield _ ; args = [ Lglobal_module _] ; _}  -> 
          None, fn
        | _ -> 
          let partial_arg = Ext_ident.create Literals.partial_arg in 
          Some partial_arg, Lam.var partial_arg
      in   
      let cont = 
        Lam.function_ 
          ~arity
          ~kind:Curried
          ~params:extra_args 
          ~body:(
            let first_args, rest_args = Ext_list.take from extra_args in 
            Lam.apply (Lam.apply new_fn (List.map Lam.var first_args) loc App_ml_full) (List.map Lam.var rest_args) loc App_na ) in 
      begin match wrapper with 
        | None -> cont 
        | Some partial_arg -> 
          Lam.let_ Strict partial_arg fn cont 
      end    
  else 
    (* add3  --adjust to arity 1 ->
       fun x -> (fun y z -> add3 x y z )

       [fun x y z -> f x y z ]
       [fun x -> [fun y z -> f x y z ]]
       This is okay if the function is not held by other..
    *)
    match fn with 

    | Lfunction 
        {params; body; kind } (* TODO check arity = List.length params in debug mode *)
      -> 
      let arity = to_ in 
      let extra_outer_args, extra_inner_args = Ext_list.take arity params in 
      Lam.function_ 
        ~arity 
        ~kind:Curried
        ~params:extra_outer_args 
        ~body:(
          Lam.function_ ~arity:(from - to_)
            ~kind:Curried ~params:extra_inner_args ~body:body)
    | _
      -> 
      let extra_outer_args = 
        Ext_list.init to_
          (fun _ ->   Ident.create Literals.param) in
      let wrapper, new_fn = 
        match fn with 
        | Lvar _ 
        | Lprim {primitive = Pfield _ ; args = [ Lglobal_module _] ; _}  -> 
          None, fn
        | _ -> 
          let partial_arg = Ext_ident.create Literals.partial_arg in 
          Some partial_arg, Lam.var partial_arg
      in   
      let cont = 
        Lam.function_ ~arity:to_ ~kind:Curried ~params:extra_outer_args 
          ~body:(
            let arity = from - to_ in 
            let extra_inner_args = Ext_list.init arity (fun _ -> Ident.create Literals.param ) in 
            Lam.function_ ~arity ~kind:Curried ~params:extra_inner_args 
              ~body:(Lam.apply new_fn 
                       (Ext_list.map_acc (List.map Lam.var extra_inner_args) Lam.var extra_outer_args )      
                       loc App_ml_full)
          )  in 
      begin match wrapper with 
        | None -> cont 
        | Some partial_arg -> Lam.let_ Strict partial_arg fn  cont    
      end
(* | _ -> 
   let partial_arg = Ext_ident.create Literals.partial_arg in 
   Lam.let_ Strict partial_arg fn 
    (let arity = to_ in 
     let extra_args = Ext_list.init arity (fun _ -> Ident.create Literals.param) in 
     Lam.function_ ~arity ~kind:Curried ~params:extra_args 
       ~body:(Lam.apply fn (List.map Lam.var extra_args ) loc Lam.App_na )
    ) *)