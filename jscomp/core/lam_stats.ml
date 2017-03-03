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








(* It can be useful for common sub expression elimination ? 
    if two lambdas are not equal, it should return false, other wise, 
    it might return true , this is only used as a way of optimizaton

    Use case :
    1. switch case -- common fall through
 *)

(* lambda pass for alpha conversion 
    and alias
    we need think about the order of the pass, might be the alias pass can be done 
    in the  beginning, when we do alpha conversion, we can instrument the table 
 *)

type function_arities = Lam.function_arities

type alias_tbl =  Ident.t Ident_hashtbl.t

type function_kind = 
  | Functor 
  | Function
  | NA

type rec_flag = 
  | Rec 
  | Non_rec

type function_id = {
  kind : function_kind ; 
  mutable arity : function_arities ;
  lambda  : Lam.t ;
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
        (** Global module, local module is treated as an array
         *)
  | Function of function_id (** True then functor *)
  | Exception 
  | Parameter
      (** For this case, it can help us determine whether it should be inlined or not *)
  | NA 
  (* | Boxed_nullable of Ident.t  *)
    (** 
       {[ let v/2 =  Pnull_to_opt u]} 

       {[ let v/2 = Pnull_to_opt exp]}
       can be translated into 
       {[
         let v/1 = exp in 
         let v/2 =a Pnull_to_opt exp 
       ]}
       so that [Pfield v/2 0] will be replaced by [v/1], 
       [Lif(v/1)] will be translated into [Lif (v/2 === undefined )]
    *)
type ident_tbl = kind Ident_hashtbl.t 

type state = 
  | Live (** Globals are always live *)
  | Dead  (** removed after elimination *)
  | NA

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
  exit_codes :  Int_hash_set.t;

  ident_tbl : ident_tbl;
  (** we don't need count arities for all identifiers, for identifiers
      for sure it's not a function, there is no need to count them
  *)
  
}

