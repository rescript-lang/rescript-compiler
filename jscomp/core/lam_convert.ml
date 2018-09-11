(* Copyright (C) 2018 - Authors of BuckleScript
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


 let abs_int x = if x < 0 then - x else x
 let no_over_flow x  = abs_int x < 0x1fff_ffff 

(** Make sure no int range overflow happens
    also we only check [int]
*)
let happens_to_be_diff
    (sw_consts :
       (int * Lambda.lambda) list) : int option =
  match sw_consts with
  | (a, Lconst (Const_pointer (a0,_)| Const_base (Const_int a0)))::
    (b, Lconst (Const_pointer (b0,_)| Const_base (Const_int b0)))::
    rest when
     no_over_flow a  &&
     no_over_flow a0 &&
     no_over_flow b &&
     no_over_flow b0 ->
    let diff = a0 - a in
    if b0 - b = diff then
      if List.for_all (fun (x, (lam : Lambda.lambda )) ->
          match lam with
          | Lconst (Const_pointer(x0,_) | Const_base(Const_int x0))
            when no_over_flow x0 && no_over_flow x ->
            x0 - x = diff
          | _ -> false
        ) rest  then
        Some diff
      else
        None
    else None
  | _ -> None 

