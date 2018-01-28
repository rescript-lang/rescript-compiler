(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(** Adapted by authors of BuckleScript without using functors          *)

module N = Bs_SortedMapDict
module A = Bs_Array
type ('key,  'a, 'id) t0 = ('key, 'a, 'id ) N.t
type ('key, 'id ) dict = ('key, 'id) Bs_Cmp.t
type ('k,'v,'id) t = {
  dict : ('k,'id) dict;
  data : ('k,'v, 'id) t0
}
[@@bs.deriving abstract]


let ofArray (type k) (type id) data ~(dict : (k,id) Bs_Cmp.t)  =
  let module M = (val dict) in 
  t ~dict ~data:(N.ofArray ~cmp:M.cmp data)


let remove (type k) (type id) (m : (k,_,id) t) x  =   
  let module M = (val dict m) in
  let odata = data m in
  let newData = N.remove odata x ~cmp:M.cmp in
  if newData == odata then m
  else t ~dict:(module M) ~data:newData

let removeMany (type k) (type id) (m : (k,_,id) t) x =     
  let module M = (val dict m) in
  let odata = data m in
  let newData = N.removeMany odata x ~cmp:M.cmp in
  if newData == odata then m
  else t ~dict:(module M) ~data:newData

let set (type k) (type id) (m : (k,_,id) t) key d  = 
  let module X = (val dict m) in 
  t ~dict:(module X) ~data:(N.set ~cmp:X.cmp (data m) key d)

let mergeMany (type elt) (type id) (m : (elt,_,id) t) e = 
  let module M = (val dict m) in 
  t ~dict:(module M) ~data:(N.mergeMany ~cmp:M.cmp (data m) e)

let update (type k) (type id) (m : (k,_,id) t) key f  = 
  let module X = (val dict m) in 
  t ~dict:(module X) ~data:(N.update ~cmp:X.cmp (data m) key f )

let split (type k)  (type id) (m : (k,_,id) t) x =   
  let module M = (val dict m) in 
  let (l,r),b = N.split ~cmp:M.cmp (data m) x in 
  (t ~dict:(module M) ~data:l, t ~dict:(module M) ~data:r), b  

let merge (type k) (type id)  (s1 : (k,_,id) t) 
    (s2 : (k,_,id) t) f = 
  let module X = (val dict s1) in 
  t ~dict:(module X) ~data:(N.merge ~cmp:X.cmp  (data s1) (data s2) f)

let empty ~dict = 
  t  ~dict  ~data:N.empty

let isEmpty map = 
  N.isEmpty (data map)

let cmp (type k)  (type id)  (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp
  = 
  let module X = (val dict m1) in 
  N.cmp ~kcmp:X.cmp ~vcmp:cmp (data m1) (data m2)

let eq (type k) (type id) 
    (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp = 
  let module X = (val dict m1) in 
  N.eq ~kcmp:X.cmp ~veq:cmp (data m1) (data m2)

let forEach m f = N.forEach (data m) f

let reduce m acc f = N.reduce (data m) acc f  

let every m f = N.every (data m) f  

let some m f = N.some (data m) f

let keepBy m f = 
  t ~dict:(dict m) ~data:(N.keepBy (data m) f)

let partition m p =   
  let dict = dict m in 
  let l,r = N.partition (data m) p in 
  t ~dict ~data:l, t ~dict ~data:r 

let map m f = 
  t ~dict:(dict m) ~data:(N.map (data m) f)
    
let mapWithKey m  f = 
  t ~dict:(dict m) ~data:(N.mapWithKey (data m) f)

let size map = N.size (data map)   
let toList map = N.toList (data map) 
let toArray m = N.toArray (data m)
let keysToArray m = N.keysToArray (data m)
let valuesToArray m = N.valuesToArray (data m)
let minKey m = N.minKey (data m)
let minKeyUndefined m = N.minKeyUndefined (data m)
let maxKey m = N.maxKey (data m)
let maxKeyUndefined m = N.maxKeyUndefined (data m)    
let minimum m = N.minimum (data m)
let minUndefined m = N.minUndefined (data m) 
let maximum m = N.maximum (data m)
let maxUndefined m = N.maxUndefined (data m)

let get (type k) (type id) (map : (k,_,id) t) x  = 
  let module X = (val dict map) in 
  N.get ~cmp:X.cmp  (data map) x 

let getUndefined (type k) (type id) (map : (k,_,id) t) x = 
  let module X = (val dict map) in 
  N.getUndefined ~cmp:X.cmp  (data map) x

let getWithDefault (type k) (type id)  (map : (k,_,id) t) x def = 
  let module X = (val dict map) in 
  N.getWithDefault ~cmp:X.cmp (data map) x  def

let getExn (type k) (type id)  (map : (k,_,id) t) x = 
  let module X = (val dict map) in 
  N.getExn ~cmp:X.cmp (data map) x 

let has (type k) (type id)  (map : (k,_,id) t) x = 
  let module X = (val dict map) in 
  N.has ~cmp:X.cmp (data map) x

let checkInvariantInternal m  =
  N.checkInvariantInternal (data m)

let getData = data
let getDict = dict
let packDictData = t
