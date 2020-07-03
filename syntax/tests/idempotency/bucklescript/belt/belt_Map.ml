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
(*  Adapted by authors of BuckleScript without using functors          *)
(***********************************************************************)

(** specalized when key type is [int], more efficient
    than the generic type
*)
module Int = Belt_MapInt
(** specalized when key type is [string], more efficient
    than the generic type *)
module String = Belt_MapString

(** seprate function from data, a more verboe but slightly
    more efficient
*)
module Dict = Belt_MapDict

module N = Belt_MapDict
module A = Belt_Array

type ('key, 'id ) id = ('key, 'id) Belt_Id.comparable
type ('key, 'id ) cmp = ('key, 'id) Belt_Id.cmp

type ('k,'v,'id) t = {
    cmp: ('k,'id) cmp;
    data: ('k,'v, 'id) Dict.t
}
[@@bs.deriving abstract]




let fromArray (type k) (type idx) data ~(id : (k,idx) id)  =
  let module M = (val id) in
  let cmp = M.cmp in
  t ~cmp ~data:(Dict.fromArray ~cmp data)

let remove m x  =
  let cmp, odata = m |. (cmpGet, dataGet) in
  let newData = Dict.remove odata x ~cmp  in
  if newData == odata then m
  else t ~cmp ~data:newData

let removeMany m x =
  let cmp, odata = m |. (cmpGet,  dataGet) in
  let newData = Dict.removeMany odata x ~cmp in
  t ~cmp  ~data:newData

let set m key d  =
  let cmp = cmpGet m in
  t ~cmp  ~data:(Dict.set ~cmp (dataGet m) key d)

let mergeMany m e =
  let cmp = cmpGet m in
  t ~cmp  ~data:(Dict.mergeMany ~cmp (dataGet m) e)

let updateU m key f  =
  let cmp = cmpGet m in
  t ~cmp ~data:(Dict.updateU ~cmp (dataGet m) key f )
let update m key f = updateU m key (fun [@bs] a  -> f a )
let split m x =
  let cmp = cmpGet m in
  let (l,r),b = Dict.split ~cmp (dataGet m) x in
  (t ~cmp ~data:l, t ~cmp  ~data:r), b

let mergeU s1 s2 f =
  let cmp = cmpGet s1 in
  t ~cmp ~data:(Dict.mergeU ~cmp  (dataGet s1) (dataGet s2) f)

let merge s1 s2 f =
  mergeU s1 s2 (fun [@bs] a b c -> f a b c)

let make (type key) (type idx) ~(id: (key, idx) id) =
  let module M = (val id) in
  t  ~cmp:M.cmp  ~data:Dict.empty

let isEmpty map =
  Dict.isEmpty (dataGet map)

let findFirstByU m f = Dict.findFirstByU (dataGet m) f
let findFirstBy m f = findFirstByU m (fun [@bs] a b -> f a b) 
let forEachU m f = Dict.forEachU (dataGet m) f
let forEach m f = forEachU m (fun [@bs] a b -> f a b)
let reduceU m acc f = Dict.reduceU (dataGet m) acc f
let reduce m acc f = reduceU m acc (fun[@bs] a b c -> f a b c )
let everyU m f = Dict.everyU (dataGet m) f
let every m f = everyU m (fun [@bs] a b -> f a b)
let someU m f = Dict.someU (dataGet m) f
let some m f = someU m (fun[@bs] a b -> f a b)
let keepU m f =
  t ~cmp:(cmpGet m) ~data:(Dict.keepU (dataGet m) f)
let keep m f = keepU m (fun [@bs] a b -> f a b)

let partitionU m p =
  let cmp = cmpGet m in
  let l,r = m |. dataGet |. Dict.partitionU  p in
  t ~cmp ~data:l, t ~cmp ~data:r
let partition m p = partitionU m (fun [@bs] a b -> p a b)

let mapU m f =
  t ~cmp:(cmpGet m) ~data:(Dict.mapU (dataGet m) f)
let map m f = mapU m (fun [@bs] a  -> f a)
let mapWithKeyU m  f =
  t ~cmp:(cmpGet m) ~data:(Dict.mapWithKeyU (dataGet m) f)
let mapWithKey m f = mapWithKeyU m (fun [@bs] a b -> f a b)
let size map = Dict.size (dataGet map)
let toList map = Dict.toList (dataGet map)
let toArray m = Dict.toArray (dataGet m)
let keysToArray m = Dict.keysToArray (dataGet m)
let valuesToArray m = Dict.valuesToArray (dataGet m)
let minKey m = Dict.minKey (dataGet m)
let minKeyUndefined m = Dict.minKeyUndefined (dataGet m)
let maxKey m = Dict.maxKey (dataGet m)
let maxKeyUndefined m = Dict.maxKeyUndefined (dataGet m)
let minimum m = Dict.minimum (dataGet m)
let minUndefined m = Dict.minUndefined (dataGet m)
let maximum m = Dict.maximum (dataGet m)
let maxUndefined m = Dict.maxUndefined (dataGet m)

let get map x  =
  Dict.get ~cmp:(cmpGet map)  (dataGet map) x

let getUndefined map x =
  Dict.getUndefined ~cmp:(cmpGet map) (dataGet map) x

let getWithDefault map x def =
  Dict.getWithDefault ~cmp:(cmpGet map) (dataGet map) x  def

let getExn map x =
  Dict.getExn ~cmp:(cmpGet map) (dataGet map) x

let has map x =
  Dict.has ~cmp:(cmpGet map) (dataGet map) x

let checkInvariantInternal m  =
  Dict.checkInvariantInternal (dataGet m)

let eqU m1 m2 veq =
  Dict.eqU ~kcmp:(cmpGet m1) ~veq (dataGet m1) (dataGet m2)
let eq m1 m2 veq = eqU m1 m2 (fun[@bs] a b -> veq a b)

let cmpU m1 m2 vcmp =
  Dict.cmpU ~kcmp:(cmpGet m1)  ~vcmp (dataGet m1) (dataGet m2)
let cmp m1 m2 vcmp = cmpU m1 m2 (fun [@bs] a b -> vcmp a b)

let getData = dataGet

let getId (type key) (type identity) (m : (key,_,identity) t) : (key, identity) id =
  let module T = struct
    type nonrec identity = identity
    type nonrec t = key
    let cmp =  cmpGet m
  end in
  (module T )

let packIdData (type key) (type idx) ~(id : (key, idx) id) ~data  =
  let module M = (val id) in
  t ~cmp:M.cmp ~data

