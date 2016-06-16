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









let maybe_functor (name : string) = 
  name.[0] >= 'A' && name.[0] <= 'Z'


let should_be_functor (name : string) lam = 
  maybe_functor name  && 
  (function | Lam.Lfunction _ -> true | _ -> false) lam

(* TODO: add a context, like 
    [args]
    [Lfunction(params,body)]
 *)

(* HONGBO .. doe snot look like this function is used (not in .mli) *) 
(* let app_definitely_inlined (body : Lam.t) =  *)
(*   match body with  *)
(*   | Lvar _  *)
(*   | Lconst _ *)
(*   | Lprim _  *)
(*   | Lapply _ -> true  *)
(*   | Llet _  *)
(*   | Lletrec  _ *)
(*   | Lstringswitch _  *)
(*   | Lswitch _  *)
(*   | Lstaticraise _ *)
(*   | Lfunction _  *)
(*   | Lstaticcatch _  *)
(*   | Ltrywith _  *)
(*   | Lifthenelse _  *)
(*   | Lsequence _  *)
(*   | Lwhile _ *)
(*   | Lfor _  *)
(*   | Lassign _  *)
(*   | Lsend _  *)
(*   | Levent _ *)
(*   | Lifused _ -> false *)
