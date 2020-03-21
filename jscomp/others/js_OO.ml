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

(**/**)
external unsafe_downgrade : 'a Js.t -> 'a = "#unsafe_downgrade"
external unsafe_to_method : 'a -> 'a = "#fn_method"
(**/**)
module Callback = struct
  type 'a arity1 = {
    _1 : 'a [@internal]
  }[@@unboxed]
  type 'a arity2 = {
    _2 : 'a [@internal]
  }[@@unboxed]
  type 'a arity3 = {
    _3 : 'a [@internal]
  }[@@unboxed]
  type 'a arity4 = {
    _4 : 'a [@internal]
  }[@@unboxed]
  type 'a arity5 = {
    _5 : 'a [@internal]
  }[@@unboxed]
  type 'a arity6 = {
    _6 : 'a [@internal]
  }[@@unboxed]
  type 'a arity7 = {
    _7 : 'a [@internal]
  }[@@unboxed]
  type 'a arity8 = {
    _8 : 'a [@internal]
  }[@@unboxed]
  type 'a arity9 = {
    _9 : 'a [@internal]
  }[@@unboxed]
  type 'a arity10 = {
    _10 : 'a [@internal]
  }[@@unboxed]
  type 'a arity11 = {
    _11 : 'a [@internal]
  }[@@unboxed]
  type 'a arity12 = {
    _12 : 'a [@internal]
  }[@@unboxed]
  type 'a arity13 = {
    _13 : 'a [@internal]
  }[@@unboxed]
  type 'a arity14 = {
    _14 : 'a [@internal]
  }[@@unboxed]
  type 'a arity15 = {
    _15 : 'a [@internal]
  }[@@unboxed]
  type 'a arity16 = {
    _16 : 'a [@internal]
  }[@@unboxed]
  type 'a arity17 = {
    _17 : 'a [@internal]
  }[@@unboxed]
  type 'a arity18 = {
    _18 : 'a [@internal]
  }[@@unboxed]
  type 'a arity19 = {
    _19 : 'a [@internal]
  }[@@unboxed]
  type 'a arity20 = {
    _20 : 'a [@internal]
  }[@@unboxed]
  type 'a arity21 = {
    _21 : 'a [@internal]
  }[@@unboxed]
  type 'a arity22 = {
    _22 : 'a [@internal]
  }[@@unboxed]  
end  
module Meth = struct
  type + 'a arity0 
  type 'a arity1 = {
    _1 : 'a [@internal]
  }[@@unboxed]
  type 'a arity2 = {
    _2 : 'a [@internal]
  }[@@unboxed]
  type 'a arity3 = {
    _3 : 'a [@internal]
  }[@@unboxed]
  type 'a arity4 = {
    _4 : 'a [@internal]
  }[@@unboxed]
  type 'a arity5 = {
    _5 : 'a [@internal]
  }[@@unboxed]
  type 'a arity6 = {
    _6 : 'a [@internal]
  }[@@unboxed]
  type 'a arity7 = {
    _7 : 'a [@internal]
  }[@@unboxed]
  type 'a arity8 = {
    _8 : 'a [@internal]
  }[@@unboxed]
  type 'a arity9 = {
    _9 : 'a [@internal]
  }[@@unboxed]
  type 'a arity10 = {
    _10 : 'a [@internal]
  }[@@unboxed]
  type 'a arity11 = {
    _11 : 'a [@internal]
  }[@@unboxed]
  type 'a arity12 = {
    _12 : 'a [@internal]
  }[@@unboxed]
  type 'a arity13 = {
    _13 : 'a [@internal]
  }[@@unboxed]
  type 'a arity14 = {
    _14 : 'a [@internal]
  }[@@unboxed]
  type 'a arity15 = {
    _15 : 'a [@internal]
  }[@@unboxed]
  type 'a arity16 = {
    _16 : 'a [@internal]
  }[@@unboxed]
  type 'a arity17 = {
    _17 : 'a [@internal]
  }[@@unboxed]
  type 'a arity18 = {
    _18 : 'a [@internal]
  }[@@unboxed]
  type 'a arity19 = {
    _19 : 'a [@internal]
  }[@@unboxed]
  type 'a arity20 = {
    _20 : 'a [@internal]
  }[@@unboxed]
  type 'a arity21 = {
    _21 : 'a [@internal]
  }[@@unboxed]
  type 'a arity22 = {
    _22 : 'a [@internal]
  }[@@unboxed]
end

module Internal = struct
  open Meth
  (* Use opaque instead of [._n] to prevent some optimizations happening *)
  external id : 'a -> 'a = "%opaque"

  external run0 : 'a arity0 -> 'a = "#run" "0"
  (* 
    x##meth a b --> 
    fullApppy (
        (id (unsafe_downgrade x)#meth).I_2) 
        a b)
  *)  
end  