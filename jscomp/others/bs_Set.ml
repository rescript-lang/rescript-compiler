(* Copyright (C) 2017 Authors of BuckleScript
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

module N = Bs_SetDict
module A = Bs_Array


type ('key, 'id) dict = ('key, 'id) Bs_Dict.comparable
type ('key, 'id ) cmp = ('key, 'id) Bs_Dict.cmp

module S = struct 
  type ('k,'id) t = {
    cmp: ('k, 'id) cmp;
    data: ('k, 'id) N.t;
  } [@@bs.deriving abstract]
end 

type ('k, 'id) t = ('k, 'id) S.t
    
let ofArray (type elt) (type id) data ~(dict : (elt,id) dict)  = 
  let module M = (val dict ) in
  let cmp = M.cmp in 
  S.t ~cmp ~data:(N.ofArray ~cmp data)

let remove m e =      
  let cmp, data  = S.cmp m, S.data m  in
  let newData = N.remove ~cmp data e in 
  if newData == data then m 
  else S.t ~cmp ~data:newData  

let add m e =   
  let cmp, data = S.cmp m, S.data m in
  let newData = N.add ~cmp  data e in 
  if newData == data then m 
  else 
    S.t ~cmp ~data:newData

let mergeMany m e =
  let cmp = S.cmp m in 
  S.t ~cmp  ~data:(N.mergeMany ~cmp (S.data m) e )

let removeMany  m e = 
  let cmp = S.cmp m in 
  S.t ~cmp ~data:(N.removeMany ~cmp (S.data m) e)

let union m n =   
  let cmp = S.cmp m in 
  S.t ~data:(N.union ~cmp (S.data m) (S.data n)) ~cmp

let intersect m n =
  let cmp = S.cmp m in 
  S.t ~data:(N.intersect ~cmp (S.data m) (S.data n)) ~cmp

let diff m n =
  let cmp = S.cmp m in 
  S.t ~cmp ~data:(N.diff ~cmp (S.data m) (S.data n))

let subset m n =     
  let cmp = S.cmp m in 
  N.subset ~cmp (S.data m) (S.data n) 

let split m e = 
  let cmp = S.cmp m in  
  let (l,  r), b = N.split ~cmp (S.data m) e in 
  (S.t ~cmp ~data:l, S.t ~cmp ~data:r), b
  
let make (type elt) (type id) ~(dict : (elt, id) dict) =
  let module M = (val dict) in 
  S.t ~cmp:M.cmp  ~data:N.empty

let isEmpty m = N.isEmpty (S.data m)

let cmp m n =
  let cmp = S.cmp m in 
  N.cmp ~cmp (S.data m) (S.data n)

let eq m n =     
  N.eq ~cmp:(S.cmp m) (S.data m) (S.data n)    

let forEach m f  = N.forEach (S.data m) f 

let reduce m acc f = N.reduce (S.data m) acc f

let every m f  = N.every  (S.data m) f

let some m f = N.some  (S.data m) f 

let keep m f  = 
  S.t ~cmp:(S.cmp m) ~data:(N.keep (S.data m) f )

let partition m f  = 
  let l,r = N.partition (S.data m) f in
  let cmp = S.cmp m in 
  S.t ~data:l ~cmp, S.t ~data:r ~cmp

let size m = N.size (S.data m) 
let toList m = N.toList (S.data m)
let toArray m = N.toArray (S.data m)

let minimum m = N.minimum (S.data m)
let minUndefined m = N.minUndefined (S.data m) 
let maximum m = N.maximum (S.data m)
let maxUndefined m = N.maxUndefined (S.data m)


let get m e =   
  N.get ~cmp:(S.cmp m) (S.data m) e

let getUndefined m e =   
  N.getUndefined ~cmp:(S.cmp m) (S.data m) e

let getExn m e =   
  N.getExn ~cmp:(S.cmp m) (S.data m) e

let has m e = 
  N.has ~cmp:(S.cmp m) (S.data m) e

let ofSortedArrayUnsafe (type elt) (type id) xs ~(dict : (elt,id) dict ) =
  let module M = (val dict) in 
  S.t ~cmp:M.cmp ~data:(N.ofSortedArrayUnsafe xs)


  

let getData = S.data

let getDict (type elt) (type id) (m : (elt,id) t) : (elt, id) dict =
  let module T = struct
    type nonrec id = id
    type nonrec t = elt
    let cmp =  S.cmp m
  end in
  (module T)
  
let packDictData (type elt) (type id) ~(dict : (elt, id) dict) ~data  =
  let module M = (val dict) in 
  S.t ~cmp:M.cmp ~data

let checkInvariantInternal d = N.checkInvariantInternal (S.data d)


module Int = Bs_SetInt
module String = Bs_SetString
