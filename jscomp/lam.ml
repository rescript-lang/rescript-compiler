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


type t = Lambda.lambda = 
  | Lvar of Ident.t
  | Lconst of Lambda.structured_constant
  | Lapply of t * t list * Lambda.apply_info
  | Lfunction of Lambda.function_kind * Ident.t list * t
  | Llet of Lambda.let_kind * Ident.t * t * t
  | Lletrec of (Ident.t * t) list * t
  | Lprim of Lambda.primitive * t list
  | Lswitch of t * Lambda.lambda_switch
  | Lstringswitch of t * (string * t) list * t option
  | Lstaticraise of int * t list
  | Lstaticcatch of t * (int * Ident.t list) * t
  | Ltrywith of t * Ident.t * t
  | Lifthenelse of t * t * t
  | Lsequence of t * t
  | Lwhile of t * t
  | Lfor of Ident.t * t * t * Asttypes.direction_flag * t
  | Lassign of Ident.t * t
  | Lsend of Lambda.meth_kind * t * t * t list * Location.t
  | Levent of t * Lambda.lambda_event
  | Lifused of Ident.t * t
