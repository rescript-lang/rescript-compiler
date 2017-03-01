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








(** Types defined for lambda analysis *)

type alias_tbl =  Ident.t Ident_hashtbl.t

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

type rec_flag = 
  | Rec 
  | Non_rec

type function_id = {
  kind : function_kind ; 
  mutable arity : Lam.function_arities;
  lambda  : Lam.t ;
  (* TODO: This may contain some closure environment,
     check how it will interact with dead code elimination
  *)
  rec_flag : rec_flag
}

type element = 
  | NA 
  | SimpleForm of Lam.t 

type boxed_nullable
  = 
  | Undefined 
  | Null 
  | Null_undefined
  | Normal 

type kind = 
  | ImmutableBlock of element array * boxed_nullable
  | MutableBlock of element array
  | Constant of Lam.constant
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
type ident_tbl = kind Ident_hashtbl.t 


type ident_info = {
  kind : kind ; 
  state : state
}

type meta = {
  env : Env.t;
  filename : string ;
  export_idents : Ident_set.t ;
  exports : Ident.t list ;
  alias_tbl : alias_tbl; 
  exit_codes : Int_hash_set.t;

  ident_tbl : ident_tbl;
  (** we don't need count arities for all identifiers, for identifiers
      for sure it's not a function, there is no need to count them
   *)


}
