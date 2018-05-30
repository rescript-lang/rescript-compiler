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





(** *)

(* Note that when we introduce it in {!Js_dump} 
   we need introduce dependency properly *)
let __ tag block = 
  Obj.set_tag block tag; block


type symbol


external cacheSymbol : string -> symbol = "for"
 [@@bs.scope "Symbol"] [@@bs.val]
external addProp : 'a -> symbol -> <value: 'b> Js.t -> 'a = 
  "defineProperty"  [@@bs.scope "Object"] [@@bs.val]


(* It won't affect [Object.keys] using [Object.defineProperty*)
let record  meta xs =
  xs |.addProp (cacheSymbol "BsRecord") [%obj {value = meta}]

let variant meta tag xs =     
  xs |. Obj.set_tag tag;
  xs |. addProp (cacheSymbol "BsVariant") [%obj {value = meta }] 

let simpleVariant meta xs =       
  xs |. addProp (cacheSymbol "BsVariant") [%obj {value = meta }] 
  
let localModule meta xs =   
  xs |. addProp (cacheSymbol "BsLocalModule") [%obj {value = meta}]

let polyVar meta xs =   
  xs |. addProp (cacheSymbol "BsPolyVar") [%obj {value = meta}]