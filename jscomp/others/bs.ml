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



(**/*)
(**/*)

module Dict = Bs_Dict
module Array = Bs_Array
module MutableQueue = Bs_Queue
module List = Bs_List
module MutableStack = Bs_Stack
module Range = Bs_Range

(** {!Bs.Set}
    The toplevel provides generic immutable set operations.
    It also has three specialized inner modules
    {!Bs.Set.Int} and {!Bs.Set.String}
    {!Bs.Set.Dict}: This module separate date from function
    which  is more verbbose but slightly more efficient

*)
module Set = Bs_Set


(** {!Bs.Map},
    The toplevel provides generic immutable map operations.
    It also has three specialized inner modules
    {!Bs.Map.Int} and {!Bs.Map.String}
    {!Bs.Map.Dict}: This module separate date from function
    which  is more verbbose but slightly more efficient
*)    
module Map = Bs_Map


(** {!Bs.MutableSet}
    The toplevel provides generic mutable set operations.
    It also has two specialized inner modules
    {!Bs.MutableSet.Int} and {!Bs.MutableSet.String}
*)
module MutableSet = Bs_SetM

(** {!Bs.MutableMap}
    The toplevel provides generic mutable map operations.
    It also has two specialized inner modules
    {!Bs.MutableMap.Int} and {!Bs.MutableMap.String}

*)
module MutableMap = Bs_MapM


(** {!Bs.HashSet}
    The toplevel provides generic mutable hash set operations.
    It also has two specialized inner modules
    {!Bs.HashSet.Int} and {!Bs.HashSet.String}
*)
module HashSet = Bs_HashSet


(** {!Bs.HashMap}
    The toplevel provides generic mutable hash map operations.
    It also has two specialized inner modules
    {!Bs.HashMap.Int} and {!Bs.HashMap.String}
*)
module HashMap = Bs_HashMap
  

(** {!Bs.SortArray}
    The toplevel provides some generic sort related utililties.
    It also has two specialized inner modules
    {!Bs.SortArray.Int} and {!Bs.SortArray.String}
*)
module SortArray = Bs_SortArray



