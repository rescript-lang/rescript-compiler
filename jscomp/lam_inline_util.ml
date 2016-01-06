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




let maybe_functor (name : string) = 
  name.[0] >= 'A' && name.[0] <= 'Z'

(* TODO: add a context, like 
    [args]
    [Lfunction(params,body)]
 *)

(* HONGBO .. doe snot look like this function is used (not in .mli) *) 
let app_definitely_inlined (body : Lambda.lambda) = 
  match body with 
  | Lvar _ 
  | Lconst _
  | Lprim _ 
  | Lapply _ -> true 
  | Llet _ 
  | Lletrec  _
  | Lstringswitch _ 
  | Lswitch _ 
  | Lstaticraise _
  | Lfunction _ 
  | Lstaticcatch _ 
  | Ltrywith _ 
  | Lifthenelse _ 
  | Lsequence _ 
  | Lwhile _
  | Lfor _ 
  | Lassign _ 
  | Lsend _ 
  | Levent _
  | Lifused _ -> false
