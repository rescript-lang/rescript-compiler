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

(** specalized when key type is [int], more efficient
    than the gerneic type
*)
module Int = Bs_MapInt
(** specalized when key type is [string], more efficient
    than the gerneic type *)  
module String = Bs_MapString

(** seprate function from data, a more verbsoe but slightly
    more efficient
*)  
module Dict = Bs_MapDict

module N = Bs_MapDict
module A = Bs_Array

type ('key, 'id ) dict = ('key, 'id) Bs_Dict.comparable
type ('key, 'id ) cmp = ('key, 'id) Bs_Dict.cmp
module S = struct 
  type ('k,'v,'id) t = {
    cmp: ('k,'id) cmp;
    data: ('k,'v, 'id) Dict.t
  }
  [@@bs.deriving abstract]
end

type ('k, 'v, 'id ) t = ('k, 'v, 'id) S.t 

let ofArray (type k) (type id) data ~(dict : (k,id) dict)  =
  let module M = (val dict) in
  let cmp = M.cmp in 
  S.t ~cmp ~data:(Dict.ofArray ~cmp data) 


let remove m x  =   
  let cmp, odata = S.cmp m, S.data m in 
  let newData = Dict.remove odata x ~cmp  in
  if newData == odata then m
  else S.t ~cmp ~data:newData 

let removeMany m x =     
  let cmp, odata = S.cmp m,  S.data m in
  let newData = Dict.removeMany odata x ~cmp in
  S.t ~cmp  ~data:newData

let set m key d  = 
  let cmp = S.cmp m in 
  S.t ~cmp  ~data:(Dict.set ~cmp (S.data m) key d) 

let mergeMany m e = 
  let cmp = S.cmp m in 
  S.t ~cmp  ~data:(Dict.mergeMany ~cmp (S.data m) e)

let update m key f  = 
  let cmp = S.cmp m in 
  S.t ~cmp ~data:(Dict.update ~cmp (S.data m) key f )

let split m x =   
  let cmp = S.cmp m in 
  let (l,r),b = Dict.split ~cmp (S.data m) x in 
  (S.t ~cmp ~data:l, S.t ~cmp  ~data:r), b  

let merge s1 s2 f = 
  let cmp = S.cmp s1 in 
  S.t ~cmp ~data:(Dict.merge ~cmp  (S.data s1) (S.data s2) f)

let make (type elt) (type id) ~(dict: (elt, id) dict) =
  let module M = (val dict) in 
  S.t  ~cmp:M.cmp  ~data:Dict.empty

let isEmpty map = 
  Dict.isEmpty (S.data map)


let forEach m f = Dict.forEach (S.data m) f

let reduce m acc f = Dict.reduce (S.data m) acc f  

let every m f = Dict.every (S.data m) f  

let some m f = Dict.some (S.data m) f

let keep m f =
  S.t ~cmp:(S.cmp m) ~data:(Dict.keep (S.data m) f)

let partition m p =   
  let cmp = S.cmp m in 
  let l,r = Dict.partition (S.data m) p in 
  S.t ~cmp ~data:l, S.t ~cmp ~data:r 

let map m f = 
  S.t ~cmp:(S.cmp m) ~data:(Dict.map (S.data m) f)
    
let mapWithKey m  f = 
  S.t ~cmp:(S.cmp m) ~data:(Dict.mapWithKey (S.data m) f)

let size map = Dict.size (S.data map)   
let toList map = Dict.toList (S.data map) 
let toArray m = Dict.toArray (S.data m)
let keysToArray m = Dict.keysToArray (S.data m)
let valuesToArray m = Dict.valuesToArray (S.data m)
let minKey m = Dict.minKey (S.data m)
let minKeyUndefined m = Dict.minKeyUndefined (S.data m)
let maxKey m = Dict.maxKey (S.data m)
let maxKeyUndefined m = Dict.maxKeyUndefined (S.data m)    
let minimum m = Dict.minimum (S.data m)
let minUndefined m = Dict.minUndefined (S.data m) 
let maximum m = Dict.maximum (S.data m)
let maxUndefined m = Dict.maxUndefined (S.data m)

let get map x  =
  Dict.get ~cmp:(S.cmp map)  (S.data map) x 

let getUndefined map x = 
  Dict.getUndefined ~cmp:(S.cmp map) (S.data map) x

let getWithDefault map x def = 
  Dict.getWithDefault ~cmp:(S.cmp map) (S.data map) x  def

let getExn map x = 
  Dict.getExn ~cmp:(S.cmp map) (S.data map) x 

let has map x = 
  Dict.has ~cmp:(S.cmp map) (S.data map) x

let checkInvariantInternal m  =
  Dict.checkInvariantInternal (S.data m)

let eq m1 m2 veq = 
  Dict.eq ~kcmp:(S.cmp m1) ~veq (S.data m1) (S.data m2)

let cmp m1 m2 vcmp =
  Dict.cmp ~kcmp:(S.cmp m1)  ~vcmp (S.data m1) (S.data m2)

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

