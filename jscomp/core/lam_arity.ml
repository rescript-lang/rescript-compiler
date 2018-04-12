(* Copyright (C) Authors of BuckleScript
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


type t = 
  | Arity_info of bool * int  list  * bool
  (**
     when the first argument is true, it is for sure 
     the last one means it can take any params later, 
     for an exception: it is (Determin (true,[], true))
     1. approximation sound but not complete 

  *)
  | Arity_na

let pp = Format.fprintf

let print (fmt : Format.formatter) (x : t) = 
  match x with 
  | Arity_na -> pp fmt "?"
  | Arity_info (b,ls,tail) -> 
    begin 
      pp fmt "@[";
      (if not b 
       then 
         pp fmt "~");
      pp fmt "[";
      Format.pp_print_list ~pp_sep:(fun fmt () -> pp fmt ",")
        (fun fmt  x -> Format.pp_print_int fmt x)
        fmt ls ;
      if tail 
      then pp fmt "@ *";
      pp fmt "]@]";
    end

let print_arities_tbl 
    (fmt : Format.formatter) 
    (arities_tbl : (Ident.t, t ref) Hashtbl.t) = 
  Hashtbl.fold (fun (i:Ident.t) (v : t ref) _ -> 
      pp Format.err_formatter "@[%s -> %a@]@."i.name print !v ) arities_tbl ()





let merge 
    (n : int )
    (x : t) : t = 
  match x with 
  | Arity_na -> Arity_info (false, [n], false)
  | Arity_info (b,xs,tail) -> Arity_info (b, n :: xs, tail)


let non_function_arity_info =   
  Arity_info (true, [], false)

let raise_arity_info =   
  Arity_info (true,[],true)

let na = Arity_na

let info b0 args b1 = 
  Arity_info (b0, args, b1)


let first_arity_na ( x : t ) =   
  match x with 
  | Arity_na 
  | Arity_info (_, [], _) -> true
  | _ -> false

let extract_arity ( x : t) =   
  match x with 
  | Arity_na  -> None
  | Arity_info(_,xs,_) -> Some xs 

(* let update_arity (x : t) xs =    *)