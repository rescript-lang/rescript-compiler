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








let _l idents = 
  Ext_log.err __LOC__ "hey .. %s@." 
    (String.concat "," @@ Ext_list.map (fun  i -> i.Ident.name ) idents) 

(* 

    Base line 
    {[
     for i = 1 to n do (function (i){...}(i))
     done
       (* This is okay, since all ocaml follow the lexical scope, 
          for generrated code too (* TODO: check *)
        *)
    ]}

  For nested loops 
  {[
   for i = 0 to n do 
     for j = 0 to n do 
       arrr.(j)<- ()=>{ i}
     done
   done
 ]}
    Three kind of variables  (defined in the loop scope)
    1. loop mutable variables 
       As long as variables change per iteration, defined in a loop (in the same loop)
        and captured by a closure
       the loop, iff  be lexically scoped 
       Tailcall parameters are considered defined inside the loop
    - unless it's defined 
       outside all the loops - note that for nested loops, if it's defined 
       in the outerloop and captured by the inner loop, 
       it still has to be lexically scoped. 

       How do we detect whether it is loop invariant or not 
       - depend on loop variant 
       - depend on mutuable valuse
       - non pure (function call)

       so we need collect mutable variables 
       1. from lambda + loop (for/i) + tailcall params
       2. defined in the loop and can not determine it is invariant  
          in such cases we can determine it's immutable
          1. const 
          2. only depend on immutable values and no function call?

    ## The following would take advantage of nested loops 
    2. loop invariant observable varaibles 
        {[ 
         var x = (console.log(3), 32)
        ]}
    3. loop invariant non-observable variables 

    Invariant: 
    loop invariant (observable or not) variables can not depend on 
    loop mutable values so that once we detect loop Invariant variables 
    all its dependency are loop invariant as well, so we can do loop 
    Invariant code motion.
    
    TODO:
    loop invariant can be layered, it will be loop invariant 
    in the inner layer while loop variant in the outer layer.
    {[
    for i = 0 to 10 do 
      for j  = 10 do 
        let  k0 = param * 100 in (* loop invariant *)
        let  k1 = i * i in (* inner loop invariant, loop variant *)
        let  k2 = j * i in (* variant *)
        ..
      done 
    done 
    ]}
*)

let scope_pass  = 
  object(self)
    inherit Js_fold.fold as super

    val  defined_idents = Ident_set.empty 
        
    (** [used_idents] 
        does not contain locally defined idents *)
    val  used_idents = Ident_set.empty 
    (** we need collect mutable values and loop defined varaibles *)    
    val loop_mutable_values = Ident_set.empty 

    val mutable_values = Ident_set.empty      

    val closured_idents = Ident_set.empty 

    (** check if in loop or not *)    
    val in_loop = false 

    method get_in_loop = in_loop

    method get_defined_idents = defined_idents

    method get_used_idents = used_idents

    method get_loop_mutable_values = loop_mutable_values 

    method get_mutable_values = mutable_values 

    method get_closured_idents = closured_idents 

    method with_in_loop b = 
      if b = self#get_in_loop then self
      else {< in_loop = b >}
     (* Since it's loop mutable variable, for sure
         it is mutable variable
         *)       
    method with_loop_mutable_values b = 
      {< loop_mutable_values =  b >}

    method add_loop_mutable_variable id = 
      {< loop_mutable_values = Ident_set.add id loop_mutable_values;
         mutable_values = Ident_set.add id mutable_values
         >}

    method add_mutable_variable id = 
      {< mutable_values = Ident_set.add id mutable_values >}

    method add_defined_ident ident = 
      {< defined_idents = Ident_set.add ident defined_idents >} 

    method! expression x = 
      match x.expression_desc with 
      | Fun (_method_, params, block , env) -> 
        (* Function is the only place to introduce a new scope in 
            ES5
            TODO: check 
            {[ try .. catch(exn) {.. }]}
            what's the scope of exn
        *)
        (* Note that [used_idents] is not complete
            it ignores some locally defined idents *)
        let param_set = Ident_set.of_list params in
        let obj =  {<defined_idents = Ident_set.empty;
                     (** pass [empty]
                         so that we can check which parameter was actually used *)
                     used_idents = Ident_set.empty ;
                     in_loop = false;
                     loop_mutable_values = Ident_set.empty;
                     mutable_values = Ident_set.of_list (Js_fun_env.get_mutable_params params env) ; 
                     closured_idents = Ident_set.empty; (* think about nested function*)
                   >} # block block in
        let defined_idents', used_idents' = 
          obj#get_defined_idents, obj#get_used_idents  in
        (* mark which param is used *)
        params |> List.iteri 
          (fun i v -> 
             if not (Ident_set.mem v used_idents') then 
               Js_fun_env.mark_unused env i) ;
        let closured_idents' =  (* pass param_set down *)
          Ident_set.(diff used_idents' (union defined_idents' param_set )) in

        (* Noe that we don't know which variables are exactly mutable yet ..
           due to the recursive thing
         *)
        Js_fun_env.set_unbounded env closured_idents'   ; 
        let lexical_scopes = Ident_set.(inter closured_idents' self#get_loop_mutable_values) in
        Js_fun_env.set_lexical_scope env lexical_scopes;
        (* tailcall , note that these varibles are used in another pass *)
        {< used_idents = 
             Ident_set.union used_idents closured_idents' ;
           (* There is a bug in ocaml -dsource*)           
           closured_idents = Ident_set.union closured_idents closured_idents'
        >}
      | _ -> super#expression x 
            (* TODO: most variables are immutable *)

    method! variable_declaration x = 
      match x with
      | { ident ; value; property  }  -> 
        let obj = 
          (match self#get_in_loop, property with 
           | true, Variable 
             -> 
             self#add_loop_mutable_variable ident 
           | true, (Strict | StrictOpt | Alias)
           (* Not real true immutable in javascript 
               since it's in the loop 

               TODO: we should also 
           *)
             -> 
             begin match value with 
               | None -> self#add_loop_mutable_variable ident
               (* TODO: Check why assertion failure *)
               (* self#add_loop_mutable_variable ident *) (* assert false *)
               | Some x
                 ->
                 (** 
                     when x is an immediate immutable value, 
                     (like integer .. )
                     not a reference, it should be Immutable
                     or string, 
                     type system might help here
                     TODO:
                 *)
                 match x.expression_desc with
                 | Fun _  | Number _ | Str _ 
                   -> self 
                 | _ -> 
                   (* if Ident_set.(is_empty @@ *)
                   (*   inter self#get_mutable_values  *)
                   (*     ( ({<  *)
                   (*         defined_idents = Ident_set.empty;  *)
                   (*         used_idents = Ident_set.empty; *)
                   (*         >} # expression x) # get_used_idents)) then *)
                   (*   (\* FIXME: still need to check expression is pure or not*\) *)
                   (*   self *)
                   (* else  *)
                   self#add_loop_mutable_variable ident
             end
           | false, Variable
             -> 
             self#add_mutable_variable ident      
           | false, (Strict | StrictOpt | Alias)
             -> self
          )#add_defined_ident ident
        in
        begin match value with 
          | None -> obj 
          | Some x -> obj # expression x  
        end 

        
    method! statement x = 
      match x.statement_desc with 
      | ForRange  (_,_, loop_id, _,_,a_env) as y -> (* TODO: simplify definition of For *)
          let obj = 
            {< in_loop = true ;
               loop_mutable_values = Ident_set.singleton loop_id ;
               used_idents = Ident_set.empty; (* TODO: if unused, can we generate better code? *)
               defined_idents = Ident_set.singleton loop_id ;
               closured_idents = Ident_set.empty (* Think about nested for blocks *)
                 (* Invariant: Finish id is never used *)
                 >}
              # statement_desc y in

          let defined_idents', used_idents', closured_idents' = 
            obj#get_defined_idents, obj#get_used_idents, obj#get_closured_idents in


          let lexical_scope =  Ident_set.(inter (diff closured_idents' defined_idents') self#get_loop_mutable_values) in
          let () = Js_closure.set_lexical_scope a_env lexical_scope in
          (* set scope *)
          {< used_idents = Ident_set.union used_idents used_idents';
             (* walk around ocaml -dsource bug 
                {[ 
                  Ident_set.(union used_idents used_idents)                  
                ]}                
             *)             
             defined_idents = Ident_set.union defined_idents defined_idents';
             (* TODO: if we our generated code also follow lexical scope,
                this is not necessary ;
                [varaibles] are mutable or not is known at definition
              *)
             closured_idents = Ident_set.union closured_idents lexical_scope
             >}

      | While (_label,pred,body, _env) ->  
          (((self#expression pred)#with_in_loop true) # block  body )
            #with_in_loop (self#get_in_loop)
      | _ -> 
          super#statement x 

    method! exception_ident x = 
      (* we can not simply skip it, since it can be used 
          TODO: check loop exception
          (loop {
          excption(i){
          () => {i}
          } 
          })
       *)
      {< used_idents = Ident_set.add x used_idents;
         defined_idents = Ident_set.add x defined_idents
         >}
    method! for_ident x = {< loop_mutable_values = Ident_set.add x loop_mutable_values >}

    method! ident x = 
      if Ident_set.mem x defined_idents then 
        self
      else {< used_idents = Ident_set.add x used_idents >}
  end

let program js = 
  (scope_pass # program js ) # get_loop_mutable_values
