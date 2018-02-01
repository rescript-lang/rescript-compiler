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

module N = Bs_MapDict
module A = Bs_Array

type ('key, 'id ) dict = ('key, 'id) Bs_Dict.comparable
type ('key, 'id ) cmp = ('key, 'id) Bs_Dict.cmp
module S = struct 
  type ('k,'v,'id) t = {
    cmp: ('k,'id) cmp;
    data: ('k,'v, 'id) N.t
  }
  [@@bs.deriving abstract]
end

type ('k, 'v, 'id ) t = ('k, 'v, 'id) S.t 

let ofArray (type k) (type id) data ~(dict : (k,id) dict)  =
  let module M = (val dict) in
  let cmp = M.cmp in 
  S.t ~cmp ~data:(N.ofArray ~cmp data) 


let remove m x  =   
  let cmp, odata = S.cmp m, S.data m in 
  let newData = N.remove odata x ~cmp  in
  if newData == odata then m
  else S.t ~cmp ~data:newData 

let removeMany m x =     
  let cmp, odata = S.cmp m,  S.data m in
  let newData = N.removeMany odata x ~cmp in
  S.t ~cmp  ~data:newData

let set m key d  = 
  let cmp = S.cmp m in 
  S.t ~cmp  ~data:(N.set ~cmp (S.data m) key d) 

let mergeMany m e = 
  let cmp = S.cmp m in 
  S.t ~cmp  ~data:(N.mergeMany ~cmp (S.data m) e)

let update m key f  = 
  let cmp = S.cmp m in 
  S.t ~cmp ~data:(N.update ~cmp (S.data m) key f )

let split m x =   
  let cmp = S.cmp m in 
  let (l,r),b = N.split ~cmp (S.data m) x in 
  (S.t ~cmp ~data:l, S.t ~cmp  ~data:r), b  

let merge s1 s2 f = 
  let cmp = S.cmp s1 in 
  S.t ~cmp ~data:(N.merge ~cmp  (S.data s1) (S.data s2) f)

let make (type elt) (type id) ~(dict: (elt, id) dict) =
  let module M = (val dict) in 
  S.t  ~cmp:M.cmp  ~data:N.empty

let isEmpty map = 
  N.isEmpty (S.data map)


let forEach m f = N.forEach (S.data m) f

let reduce m acc f = N.reduce (S.data m) acc f  

let every m f = N.every (S.data m) f  

let some m f = N.some (S.data m) f

let keep m f =
  S.t ~cmp:(S.cmp m) ~data:(N.keep (S.data m) f)

let partition m p =   
  let cmp = S.cmp m in 
  let l,r = N.partition (S.data m) p in 
  S.t ~cmp ~data:l, S.t ~cmp ~data:r 

let map m f = 
  S.t ~cmp:(S.cmp m) ~data:(N.map (S.data m) f)
    
let mapWithKey m  f = 
  S.t ~cmp:(S.cmp m) ~data:(N.mapWithKey (S.data m) f)

let size map = N.size (S.data map)   
let toList map = N.toList (S.data map) 
let toArray m = N.toArray (S.data m)
let keysToArray m = N.keysToArray (S.data m)
let valuesToArray m = N.valuesToArray (S.data m)
let minKey m = N.minKey (S.data m)
let minKeyUndefined m = N.minKeyUndefined (S.data m)
let maxKey m = N.maxKey (S.data m)
let maxKeyUndefined m = N.maxKeyUndefined (S.data m)    
let minimum m = N.minimum (S.data m)
let minUndefined m = N.minUndefined (S.data m) 
let maximum m = N.maximum (S.data m)
let maxUndefined m = N.maxUndefined (S.data m)

let get map x  =
  N.get ~cmp:(S.cmp map)  (S.data map) x 

let getUndefined map x = 
  N.getUndefined ~cmp:(S.cmp map) (S.data map) x

let getWithDefault map x def = 
  N.getWithDefault ~cmp:(S.cmp map) (S.data map) x  def

let getExn map x = 
  N.getExn ~cmp:(S.cmp map) (S.data map) x 

let has map x = 
  N.has ~cmp:(S.cmp map) (S.data map) x

let checkInvariantInternal m  =
  N.checkInvariantInternal (S.data m)

let eq m1 m2 veq = 
  N.eq ~kcmp:(S.cmp m1) ~veq (S.data m1) (S.data m2)

let cmp m1 m2 vcmp =
  N.cmp ~kcmp:(S.cmp m1)  ~vcmp (S.data m1) (S.data m2)

let getData = S.data
  
let getDict (type elt) (type id) (m : (elt,_,id) t) : (elt, id) dict =
  let module T = struct
    type nonrec id = id
    type nonrec t = elt
    let cmp =  S.cmp m
  end in
  (module T )
  
let packDictData (type elt) (type id) ~(dict : (elt, id) dict) ~data  =
  let module M = (val dict) in 
  S.t ~cmp:M.cmp ~data

module Int = Bs_MapInt
module String = Bs_MapString
