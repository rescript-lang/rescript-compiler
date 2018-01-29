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
module Cmp = Bs_Cmp
module Hash = Bs_Hash
module Array = Bs_Array
module Queue = Bs_Queue
module List = Bs_List
module Stack = Bs_Stack
module Range = Bs_Range

(** Immutable sorted set *)
module Set = Bs_Set

(** The implementation detail for {!Bs.Set}, also slightly more efficient *)    
module SetDict = Bs_SortedSetDict

(** can also be accessed via {!Bs.Set.Int}*)    
module SetInt = Bs_SetInt

(** can also be accessed via {!Bs.Set.String}*)    
module SetString = Bs_SetString


module MutableSet = Bs_SetM

(** can also be accessed via {!Bs.MutableSet.Int}*)        
module MutableSetInt = Bs_SetIntM

(** can also be accessed via {!Bs.MutableSet.String}*)        
module MutableSetString = Bs_SetIntM


module UnorderedMutableSet = Bs_HashSet

(** can also be accessed via {!Bs.UnordedMutableSet.Int}*)          
module UnorderedMutableSetInt = Bs_HashSetInt

(** can also be accessed via {!Bs.UnordedMutableSet.String}*)
module UnorderedMutableSetString = Bs_HashSetString

  
module Map = Bs_Map

(** The implementaion detail for {!Bs.Map},  also slightly more efficient*)    
module MapDict = Bs_SortedMapDict

(** can also be accessed via {!Bs.Map.Int}*)  
module MapInt = Bs_MapInt

(** can also be accessed via {!Bs.Map.String}*)    
module MapString = Bs_MapString


module MutableMap = Bs_MapM

(** can also be accessed via {!Bs.MutableMap.Int}*)    
module MutableMapInt = Bs_MapIntM

(** can also be accessed via {!Bs.MutableMap.String}*)
module MutableMapString = Bs_MapStringM

  
module UnorderedMutableMap = Bs_HashMap
  
(** can also be accessed via {!Bs.UnorderedMutableMap.Int}*)        
module UnorderedMutableMapInt = Bs_HashMapInt  

(** can also be accessed via {!Bs.UnorderedMutableMap.String}*)      
module UnorderedMutableMapString = Bs_HashMapString


module SortArray = Bs_SortArray

(** can also be accessed via {!Bs.SortArray.Int}*)          
module SortArrayInt = Bs_SortArrayInt

(** can also be accessed via {!Bs.SortArray.String}*)          
module SortArrayString = Bs_SortArrayString




(* module HashMultiMap = Bs_HashMultiMap *)
