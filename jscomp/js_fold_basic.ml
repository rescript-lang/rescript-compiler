(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)




class count_deps (add : Ident.t -> unit )  = 
  object(self)
    inherit  Js_fold.fold as super
    method! expression lam = 
      match lam.expression_desc with 
      | Fun (_, block, _) -> self#block block
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

class count_hard_dependencies = 
  object(self)
    inherit  Js_fold.fold as super
    val hard_dependencies = Hash_set.create 17
    method! vident vid = 
      match vid with 
      | Qualified (id,kind,_) ->
          Hash_set.add  hard_dependencies (Lam_module_ident.mk kind id); self
      | Id id -> self
  
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
    if Ident_set.mem id variables then 
      v := Ident_set.add id !v 
  in
  ignore @@ (new count_deps add ) # expression lam ;
  !v

