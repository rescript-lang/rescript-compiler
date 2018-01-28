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

module N = Bs_SortedSetDict
module A = Bs_Array

type ('k,'id) t0 = ('k, 'id) N.t
type ('key, 'id) dict = ('key, 'id) Bs_Cmp.t

type ('k,'id) t = {
  dict: ('k, 'id) dict ;
  data: ('k, 'id) t0
} [@@bs.deriving abstract]
  


let ofArray (type elt) (type id) data ~(dict : (elt,id) dict)  = 
  let module M = (val dict ) in 
  t ~dict ~data:(N.ofArray ~cmp:M.cmp data)

let remove (type elt) (type id) (m : (elt,id) t) e =      
  let module M = (val dict m) in
  let data =  data m in
  let newData = N.remove ~cmp:M.cmp data e in 
  if newData == data then m 
  else t ~dict:(module M) ~data:newData  

let add (type elt) (type id) (m : (elt,id) t) e =   
  let module M = (val dict m) in
  let data = data m in
  let newData = N.add ~cmp:M.cmp  data e in 
  if newData == data then m 
  else 
    t ~dict:(module M) ~data:newData

let mergeMany (type elt) (type id) (m : (elt,id) t) e = 
  let module M = (val dict m) in
  t ~dict:(module M) ~data:(N.mergeMany ~cmp:M.cmp (data m) e )

let removeMany (type elt) (type id) (m : (elt,id) t) e = 
  let module M = (val dict m) in 
  t ~dict:(module M) ~data:(N.removeMany ~cmp:M.cmp (data m) e)

let union (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let module M = (val dict m) in 
  t ~data:(N.union ~cmp:M.cmp (data m) (data n)) ~dict:(module M)

let intersect (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let module M = (val dict m) in 
  t ~data:(N.intersect ~cmp:M.cmp (data m) (data n)) ~dict:(module M)

let diff (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =   
  let module M = (val dict m) in 
  t ~dict:(module M) ~data:(N.diff ~cmp:M.cmp (data m) (data n))

let subset (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let module M = (val dict m) in 
  N.subset ~cmp:M.cmp (data m) (data n) 

let split (type elt) (type id) (m : (elt,id) t) e = 
  let module M = (val dict m) in 
  let (l,  r), b = N.split ~cmp:M.cmp (data m) e in 
  (t ~dict:(module M) ~data:l, t ~dict:(module M) ~data:r), b
  
let empty ~dict = 
  t ~dict  ~data:N.empty

let isEmpty m = N.isEmpty (data m)

let cmp (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let module M = (val dict m) in 
  N.cmp ~cmp:M.cmp (data m) (data n)

let eq (type elt) (type id) (m : (elt,id) t) (n : (elt,id) t) =     
  let module M = (val dict m) in 
  N.eq ~cmp:M.cmp (data m) (data n)    

let forEach m f  = N.forEach (data m) f 

let reduce m acc f = N.reduce (data m) acc f

let every m f  = N.every  (data m) f

let some m f = N.some  (data m) f 

let keepBy m f  = 
  t ~dict:(dict m) ~data:(N.keepBy (data m) f )

let partition m f  = 
  let l,r = N.partition (data m) f in
  let dict = dict m in 
  t ~data:l ~dict, t ~data:r ~dict

let size m = N.size (data m) 
let toList m = N.toList (data m)
let toArray m = N.toArray (data m)

let minimum m = N.minimum (data m)
let minUndefined m = N.minUndefined (data m) 
let maximum m = N.maximum (data m)
let maxUndefined m = N.maxUndefined (data m)


let get (type elt) (type id)  (m : (elt,id) t) e =   
  let module M = (val dict m) in 
  N.get ~cmp:M.cmp (data m) e

let getUndefined (type elt) (type id) (m : (elt,id) t) e =   
  let module M = (val dict m) in 
  N.getUndefined ~cmp:M.cmp (data m) e

let getExn (type elt) (type id) (m : (elt,id) t) e =   
  let module M = (val dict m) in 
  N.getExn ~cmp:M.cmp (data m) e

let has (type elt) (type id) (m : (elt,id) t) e = 
  let module M = (val dict m) in 
  N.has ~cmp:M.cmp (data m) e

let ofSortedArrayUnsafe xs ~dict =
  t ~dict ~data:(N.ofSortedArrayUnsafe xs)


  

let getData = data
let getDict = dict
let packDictData = t                
let checkInvariantInternal d = N.checkInvariantInternal (data d)



