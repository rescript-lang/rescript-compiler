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

module Set = Bs_Set  
module SetDict = Bs_SortedSetDict
(** The implementation detail for {!Bs.Set}, also slightly more efficient *)  
module SetInt = Bs_SetInt
(** can also be accessed via {!Bs.Set.Int}*)    
module SetString = Bs_SetString
(** can also be accessed via {!Bs.Set.String}*)    

module MutableSet = Bs_SetM
module MutableSetInt = Bs_SetIntM
(** can also be accessed via {!Bs.MutableSet.Int}*)      
module MutableSetString = Bs_SetIntM
(** can also be accessed via {!Bs.MutableSet.String}*)        

module UnorderedMutableSet = Bs_HashSet
module UnorderedMutableSetInt = Bs_HashSetInt
(** can also be accessed via {!Bs.UnordedMutableSet.Int}*)        
module UnorderedMutableSetString = Bs_HashSetString
(** can also be accessed via {!Bs.UnordedMutableSet.String}*)
  
module Map = Bs_Map
module MapDict = Bs_SortedMapDict
(** The implementaion detail for {!Bs.Map},  also slightly more efficient*)  
module MapInt = Bs_MapInt
(** can also be accessed via {!Bs.Map.Int}*)  
module MapString = Bs_MapString
(** can also be accessed via {!Bs.Map.String}*)    

module MutableMap = Bs_MapM
module MutableMapInt = Bs_MapIntM
(** can also be accessed via {!Bs.MutableMap.Int}*)    
module MutableMapString = Bs_MapStringM
(** can also be accessed via {!Bs.MutableMap.String}*)
  
module UnorderedMutableMap = Bs_HashMap
module UnorderedMutableMapInt = Bs_HashMapInt  
(** can also be accessed via {!Bs.UnorderedMutableMap.Int}*)      
module UnorderedMutableMapString = Bs_HashMapString
(** can also be accessed via {!Bs.UnorderedMutableMap.String}*)      

module SortArray = Bs_SortArray
module SortArrayInt = Bs_SortArrayInt
(** can also be accessed via {!Bs.SortArray.Int}*)        
module SortArrayString = Bs_SortArrayString
(** can also be accessed via {!Bs.SortArray.String}*)          



(* module HashMultiMap = Bs_HashMultiMap *)
