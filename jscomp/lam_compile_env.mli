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








(** Helper for global Ocaml module index into meaningful names  *) 

type primitive_description = Types.type_expr option Primitive.description

type key = 
  Ident.t * Env.t * bool 
  (** the boolean is expand or not
      when it's passed as module, it should be expanded, 
      otherwise for alias, [include Array], it's okay to return an identifier
      TODO: be more clear about its concept
  *)
  (** we need register which global variable is an dependency *)


type ident_info = {
  id : Ident.t;
  name : string;
  signatures : Types.signature;
  arity : Lam_stats.function_arities; 
  closed_lambda : Lambda.lambda option 
}

type module_info = {
  signature :  Types.signature ;
  pure : bool 
}

type _ t = 
  | No_env :  Js_cmj_format.t t 
  | Has_env : Env.t  -> module_info t 

val find_and_add_if_not_exist : 
  Ident.t * int -> 
  Env.t -> 
  not_found:(Ident.t -> 'a) -> 
  found:(ident_info -> 'a) -> 'a

val query_and_add_if_not_exist : 
  Lam_module_ident.t ->
  'a t -> not_found:(unit -> 'b) ->
  found:('a -> 'b) -> 'b

val add_js_module : ?id:Ident.t -> string  -> Ident.t 
(** add third party dependency *)

(* The other dependencies are captured by querying 
   either when [access] or when expansion, 
   however such dependency can be removed after inlining etc.

   When we register such compile time dependency we classified 
   it as 
   Visit (ml), Builtin(built in js), External()

   For external, we never remove, we only consider 
   remove dependency for Runtime and Visit, so 
   when compile OCaml to Javascript, we only need 
   pay attention to for those modules are actually used or not
*)

val reset : unit -> unit 

val is_pure : Lam_module_ident.t -> bool
val get_goog_package_name : Lam_module_ident.t -> string option
(* The second argument is mostly from [runtime] modules 
    will change the input [hard_dependencies]
*)
val get_requried_modules : 
  Env.t ->
  Lam_module_ident.t list ->
  Lam_module_ident.t Hash_set.hashset -> 
  Lam_module_ident.t list
