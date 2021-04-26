(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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

[@@@bs.config {flags = [|"-unboxed-types"|]}]
(**/**)
external unsafe_to_method : 'a -> 'a =  "#fn_method"
(**/**)
module Callback = struct
  type 'a arity1 = {
    i1 : 'a [@internal]
  }
  type 'a arity2 = {
    i2 : 'a [@internal]
  }
  type 'a arity3 = {
    i3 : 'a [@internal]
  }
  type 'a arity4 = {
    i4 : 'a [@internal]
  }
  type 'a arity5 = {
    i5 : 'a [@internal]
  }
  type 'a arity6 = {
    i6 : 'a [@internal]
  }
  type 'a arity7 = {
    i7 : 'a [@internal]
  }
  type 'a arity8 = {
    i8 : 'a [@internal]
  }
  type 'a arity9 = {
    i9 : 'a [@internal]
  }
  type 'a arity10 = {
    i10 : 'a [@internal]
  }
  type 'a arity11 = {
    i11 : 'a [@internal]
  }
  type 'a arity12 = {
    i12 : 'a [@internal]
  }
  type 'a arity13 = {
    i13 : 'a [@internal]
  }
  type 'a arity14 = {
    i14 : 'a [@internal]
  }
  type 'a arity15 = {
    i15 : 'a [@internal]
  }
  type 'a arity16 = {
    i16 : 'a [@internal]
  }
  type 'a arity17 = {
    i17 : 'a [@internal]
  }
  type 'a arity18 = {
    i18 : 'a [@internal]
  }
  type 'a arity19 = {
    i19 : 'a [@internal]
  }
  type 'a arity20 = {
    i20 : 'a [@internal]
  }
  type 'a arity21 = {
    i21 : 'a [@internal]
  }
  type 'a arity22 = {
    i22 : 'a [@internal]
  }  
end  
module Meth = struct
  type + 'a arity0 
  type 'a arity1 = {
    i1 : 'a [@internal]
  }
  type 'a arity2 = {
    i2 : 'a [@internal]
  }
  type 'a arity3 = {
    i3 : 'a [@internal]
  }
  type 'a arity4 = {
    i4 : 'a [@internal]
  }
  type 'a arity5 = {
    i5 : 'a [@internal]
  }
  type 'a arity6 = {
    i6 : 'a [@internal]
  }
  type 'a arity7 = {
    i7 : 'a [@internal]
  }
  type 'a arity8 = {
    i8 : 'a [@internal]
  }
  type 'a arity9 = {
    i9 : 'a [@internal]
  }
  type 'a arity10 = {
    i10 : 'a [@internal]
  }
  type 'a arity11 = {
    i11 : 'a [@internal]
  }
  type 'a arity12 = {
    i12 : 'a [@internal]
  }
  type 'a arity13 = {
    i13 : 'a [@internal]
  }
  type 'a arity14 = {
    i14 : 'a [@internal]
  }
  type 'a arity15 = {
    i15 : 'a [@internal]
  }
  type 'a arity16 = {
    i16 : 'a [@internal]
  }
  type 'a arity17 = {
    i17 : 'a [@internal]
  }
  type 'a arity18 = {
    i18 : 'a [@internal]
  }
  type 'a arity19 = {
    i19 : 'a [@internal]
  }
  type 'a arity20 = {
    i20 : 'a [@internal]
  }
  type 'a arity21 = {
    i21 : 'a [@internal]
  }
  type 'a arity22 = {
    i22 : 'a [@internal]
  }
end

(**/**)
module Internal = struct
  open Meth
  (* Use opaque instead of `._n` to prevent some optimizations happening *)

  external run : 'a arity0 -> 'a = "#run" "0"
  (* 
    x##meth a b --> 
    fullApppy (
        (id (unsafe_downgrade x)#meth).I_2) 
        a b)
  *)  
end  
(**/**)
