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


type ('value, 'id) t = {
    cmp: ('value, 'id) cmp;
    data: ('value, 'id) Dict.t;
  } [@@bs.deriving abstract]



    
let fromArray (type value) (type identity) data ~(id : (value,identity) id)  = 
  let module M = (val id ) in
  let cmp = M.cmp in 
  t ~cmp ~data:(Dict.fromArray ~cmp data)

let remove m e =      
  let cmp, data  = cmpGet m, dataGet m  in
  let newData = Dict.remove ~cmp data e in 
  if newData == data then m 
  else t ~cmp ~data:newData  

let add m e =   
  let cmp, data = cmpGet m, dataGet m in
  let newData = Dict.add ~cmp  data e in 
  if newData == data then m 
  else 
    t ~cmp ~data:newData

let mergeMany m e =
  let cmp = cmpGet m in 
  t ~cmp  ~data:(Dict.mergeMany ~cmp (dataGet m) e )

let removeMany  m e = 
  let cmp = cmpGet m in 
  t ~cmp ~data:(Dict.removeMany ~cmp (dataGet m) e)

let union m n =   
  let cmp = cmpGet m in 
  t ~data:(Dict.union ~cmp (dataGet m) (dataGet n)) ~cmp

let intersect m n =
  let cmp = cmpGet m in 
  t ~data:(Dict.intersect ~cmp (dataGet m) (dataGet n)) ~cmp

let diff m n =
  let cmp = cmpGet m in 
  t ~cmp ~data:(Dict.diff ~cmp (dataGet m) (dataGet n))

let subset m n =     
  let cmp = cmpGet m in 
  Dict.subset ~cmp (dataGet m) (dataGet n) 

let split m e = 
  let cmp = cmpGet m in  
  let (l,  r), b = Dict.split ~cmp (dataGet m) e in 
  (t ~cmp ~data:l, t ~cmp ~data:r), b
  
let make (type value) (type identity) ~(id : (value, identity) id) =
  let module M = (val id) in 
  t ~cmp:M.cmp  ~data:Dict.empty

let isEmpty m = Dict.isEmpty (dataGet m)

let cmp m n =
  let cmp = cmpGet m in 
  Dict.cmp ~cmp (dataGet m) (dataGet n)

let eq m n =     
  Dict.eq ~cmp:(cmpGet m) (dataGet m) (dataGet n)    

let forEachU m f  = Dict.forEachU (dataGet m) f 
let forEach m  f = forEachU m (fun [@bs] a -> f a)

let reduceU m acc f = Dict.reduceU (dataGet m) acc f
let reduce m acc f = reduceU m acc (fun [@bs] a b -> f a b)
    
let everyU m f  = Dict.everyU  (dataGet m) f
let every m f  = everyU m (fun [@bs] a -> f a)
    
let someU m f = Dict.someU  (dataGet m) f
let some m f = someU m (fun [@bs] a -> f a)

let keepU m f  = 
  t ~cmp:(cmpGet m) ~data:(Dict.keepU (dataGet m) f )
let keep m f = keepU m (fun [@bs] a -> f a)
    
let partitionU m f  = 
  let l,r = Dict.partitionU (dataGet m) f in
  let cmp = cmpGet m in 
  t ~data:l ~cmp, t ~data:r ~cmp
let partition m f = partitionU m (fun [@bs] a -> f a)
    
let size m = Dict.size (dataGet m) 
let toList m = Dict.toList (dataGet m)
let toArray m = Dict.toArray (dataGet m)

let minimum m = Dict.minimum (dataGet m)
let minUndefined m = Dict.minUndefined (dataGet m) 
let maximum m = Dict.maximum (dataGet m)
let maxUndefined m = Dict.maxUndefined (dataGet m)


let get m e =   
  Dict.get ~cmp:(cmpGet m) (dataGet m) e

let getUndefined m e =   
  Dict.getUndefined ~cmp:(cmpGet m) (dataGet m) e

let getExn m e =   
  Dict.getExn ~cmp:(cmpGet m) (dataGet m) e

let has m e = 
  Dict.has ~cmp:(cmpGet m) (dataGet m) e

let fromSortedArrayUnsafe (type value) (type identity) xs ~(id : (value,identity) id ) =
  let module M = (val id) in 
  t ~cmp:M.cmp ~data:(Dict.fromSortedArrayUnsafe xs)

let getData = dataGet

let getId (type value) (type identity) (m : (value,identity) t) : (value, identity) id =
  let module T = struct
    type nonrec identity = identity
    type nonrec t = value
    let cmp =  cmpGet m
  end in
  (module T)
  
let packIdData (type value) (type identity) ~(id : (value, identity) id) ~data  =
  let module M = (val id) in 
  t ~cmp:M.cmp ~data

let checkInvariantInternal d = Dict.checkInvariantInternal (dataGet d)


