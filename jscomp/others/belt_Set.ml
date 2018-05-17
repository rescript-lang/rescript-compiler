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

module Int = Belt_SetInt
module String = Belt_SetString
module Dict = Belt_SetDict
  
module A = Belt_Array


type ('value, 'id) id = ('value, 'id) Belt_Id.comparable
type ('value, 'id ) cmp = ('value, 'id) Belt_Id.cmp

module S = struct 
  type ('value, 'id) t = {
    cmp: ('value, 'id) cmp;
    data: ('value, 'id) Dict.t;
  } [@@bs.deriving abstract]
end 

type ('value, 'id) t = ('value, 'id) S.t
    
let fromArray (type value) (type identity) data ~(id : (value,identity) id)  = 
  let module M = (val id ) in
  let cmp = M.cmp in 
  S.t ~cmp ~data:(Dict.fromArray ~cmp data)

let remove m e =      
  let cmp, data  = S.cmp m, S.data m  in
  let newData = Dict.remove ~cmp data e in 
  if newData == data then m 
  else S.t ~cmp ~data:newData  

let add m e =   
  let cmp, data = S.cmp m, S.data m in
  let newData = Dict.add ~cmp  data e in 
  if newData == data then m 
  else 
    S.t ~cmp ~data:newData

let mergeMany m e =
  let cmp = S.cmp m in 
  S.t ~cmp  ~data:(Dict.mergeMany ~cmp (S.data m) e )

let removeMany  m e = 
  let cmp = S.cmp m in 
  S.t ~cmp ~data:(Dict.removeMany ~cmp (S.data m) e)

let union m n =   
  let cmp = S.cmp m in 
  S.t ~data:(Dict.union ~cmp (S.data m) (S.data n)) ~cmp

let intersect m n =
  let cmp = S.cmp m in 
  S.t ~data:(Dict.intersect ~cmp (S.data m) (S.data n)) ~cmp

let diff m n =
  let cmp = S.cmp m in 
  S.t ~cmp ~data:(Dict.diff ~cmp (S.data m) (S.data n))

let subset m n =     
  let cmp = S.cmp m in 
  Dict.subset ~cmp (S.data m) (S.data n) 

let split m e = 
  let cmp = S.cmp m in  
  let (l,  r), b = Dict.split ~cmp (S.data m) e in 
  (S.t ~cmp ~data:l, S.t ~cmp ~data:r), b
  
let make (type value) (type identity) ~(id : (value, identity) id) =
  let module M = (val id) in 
  S.t ~cmp:M.cmp  ~data:Dict.empty

let isEmpty m = Dict.isEmpty (S.data m)

let cmp m n =
  let cmp = S.cmp m in 
  Dict.cmp ~cmp (S.data m) (S.data n)

let eq m n =     
  Dict.eq ~cmp:(S.cmp m) (S.data m) (S.data n)    

let forEachU m f  = Dict.forEachU (S.data m) f 
let forEach m  f = forEachU m (fun [@bs] a -> f a)

let reduceU m acc f = Dict.reduceU (S.data m) acc f
let reduce m acc f = reduceU m acc (fun [@bs] a b -> f a b)
    
let everyU m f  = Dict.everyU  (S.data m) f
let every m f  = everyU m (fun [@bs] a -> f a)
    
let someU m f = Dict.someU  (S.data m) f
let some m f = someU m (fun [@bs] a -> f a)

let keepU m f  = 
  S.t ~cmp:(S.cmp m) ~data:(Dict.keepU (S.data m) f )
let keep m f = keepU m (fun [@bs] a -> f a)
    
let partitionU m f  = 
  let l,r = Dict.partitionU (S.data m) f in
  let cmp = S.cmp m in 
  S.t ~data:l ~cmp, S.t ~data:r ~cmp
let partition m f = partitionU m (fun [@bs] a -> f a)
    
let size m = Dict.size (S.data m) 
let toList m = Dict.toList (S.data m)
let toArray m = Dict.toArray (S.data m)

let minimum m = Dict.minimum (S.data m)
let minUndefined m = Dict.minUndefined (S.data m) 
let maximum m = Dict.maximum (S.data m)
let maxUndefined m = Dict.maxUndefined (S.data m)


let get m e =   
  Dict.get ~cmp:(S.cmp m) (S.data m) e

let getUndefined m e =   
  Dict.getUndefined ~cmp:(S.cmp m) (S.data m) e

let getExn m e =   
  Dict.getExn ~cmp:(S.cmp m) (S.data m) e

let has m e = 
  Dict.has ~cmp:(S.cmp m) (S.data m) e

let fromSortedArrayUnsafe (type value) (type identity) xs ~(id : (value,identity) id ) =
  let module M = (val id) in 
  S.t ~cmp:M.cmp ~data:(Dict.fromSortedArrayUnsafe xs)

let getData = S.data

let getId (type value) (type identity) (m : (value,identity) t) : (value, identity) id =
  let module T = struct
    type nonrec identity = identity
    type nonrec t = value
    let cmp =  S.cmp m
  end in
  (module T)
  
let packIdData (type value) (type identity) ~(id : (value, identity) id) ~data  =
  let module M = (val id) in 
  S.t ~cmp:M.cmp ~data

let checkInvariantInternal d = Dict.checkInvariantInternal (S.data d)


