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



(** Types defined for lambda analysis *)

type function_arities = 
  | Determin of bool * (int * Ident.t list) list  * bool
  (** when the first argument is true, it is for sure 
      approximation sound but not complete 
      
      the last one means it can take any params later, 
      for an exception: it is (Determin (true,[], true))
   *)
  | NA 

type alias_tbl =  (Ident.t, Ident.t) Hashtbl.t
(** Keep track of which identifiers are aliased
  *)

type state = 
  | Live (** Globals are always live *)
  | Dead  (** removed after elimination *)
  | NA

type function_kind = 
  | Functor 
  | Function
  | NA

type function_id = {
  kind : function_kind ; 
  mutable arity : function_arities;
  lambda  : Lambda.lambda ;
  (* TODO: This may contain some closure environment,
     check how it will interact with dead code elimination
  *)
}

type element = 
  | NA 
  | SimpleForm of Lambda.lambda 

type kind = 
  | ImmutableBlock of element array
  | MutableBlock of element array
  | Constant of Lambda.structured_constant
  | Module of Ident.t
        (** TODO: static module vs first class module *)
  | Function of function_id 
  | Exception 
  | Parameter
      (** For this case, it can help us determine whether it should be inlined or not *)

  | NA (** Not such information is associated with an identifier, it is immutable, 
           if you only associate a property to an identifier 
           we should consider [Lassign]
        *)

type ident_tbl = (Ident.t, kind) Hashtbl.t 

type ident_info = {
  kind : kind ; 
  state : state
}

type meta = {
  env : Env.t;
  filename : string ;
  mutable export_idents : Ident.t list;
  alias_tbl : alias_tbl; 
  exit_codes : int Hash_set.hashset;
  mutable unused_exit_code : int ; (* clean up later, not used any more*)
  ident_tbl : ident_tbl;
  (** we don't need count arities for all identifiers, for identifiers
      for sure it's not a function, there is no need to count them
   *)
  exports : Ident.t list ;
  mutable required_modules : Lam_module_ident.t list ;
}
