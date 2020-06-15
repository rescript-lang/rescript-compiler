(* Copyright (C) 2020- Authors of BuckleScript 
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


 
let make_test_sequence_variant_constant 
  (fail : Lambda.lambda option) (arg : Lambda.lambda) 
  (int_lambda_list : (int * Lambda.lambda ) list) : Lambda.lambda=
  match int_lambda_list, fail with 
  | (_, act) :: rest, None -> 
    Ext_list.fold_right rest act (fun (hash1,act1) acc -> 
        Lifthenelse (Lprim(Pintcomp Ceq, 
          [arg; Lconst (Const_base(Const_int hash1))], Location.none),
          act1, acc
          )
      )
  | _, Some fail -> 
    Ext_list.fold_right int_lambda_list fail (fun (hash1,act1) acc -> 
        Lifthenelse (Lprim(Pintcomp Ceq, 
                           [arg; Lconst (Const_base(Const_int hash1))], Location.none),
                     act1, acc
                    )
      )
  | [], None -> assert false    