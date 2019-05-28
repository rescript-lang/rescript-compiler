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



class count_deps (add : Ident.t -> unit )  = 
  object(self)
    inherit  Js_fold.fold as super
    method! expression lam = 
      match lam.expression_desc with 
      | Fun (_, _, block, _) -> self#block block
      (** Call 
          actually depends on parameter, 
          since closure 
          {[
            n = n - 1
                    acc = () => n 
          ]}
          should be 

          {[
            acc = (function (n) {() => n} (n))
              n = n - 1
          ]}
      *)
      | _ -> super#expression lam
    method! ident x = add x ; self
  end

let add_lam_module_ident = Lam_module_ident.Hash_set.add
let create = Lam_module_ident.Hash_set.create
class count_hard_dependencies = 
  object(self : 'self_type)
    inherit  Js_fold.fold as super
    val hard_dependencies =  create 17
    method! vident vid = 
      match vid with 
      | Qualified (id,kind,_) ->
          add_lam_module_ident  hard_dependencies (Lam_module_ident.mk kind id); self
      | Id id -> self
    method! expression x : 'self_type  = 
      (* check {!Js_pass_scope} when making changes *)
      (match  Js_block_runtime.check_additional_id x with
       | Some id -> 
         add_lam_module_ident hard_dependencies
           (Lam_module_ident.of_runtime 
              id)
       | _ -> ());
      super#expression x
    method get_hard_dependencies = hard_dependencies
  end

let calculate_hard_dependencies block = 
  ((new count_hard_dependencies)#block block) # get_hard_dependencies

(*
   Given a set of [variables], count which variables  [lam] will depend on
   Invariant:
   [variables] are parameters which means immutable so that [Call] 
   will not depend [variables]

*)
let depends_j (lam : J.expression) (variables : Ident_set.t) = 
  let v = ref Ident_set.empty in
  let add id = 
    if Ident_set.mem variables id then 
      v := Ident_set.add !v id
  in
  ignore @@ (new count_deps add ) # expression lam ;
  !v

