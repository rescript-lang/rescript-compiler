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
(*  Adapted by authors of ReScript without using functors          *)
(***********************************************************************)

(** specalized when key type is `int`, more efficient
    than the generic type
*)
module Int = Belt_MapInt


(** specalized when key type is `string`, more efficient
    than the generic type *)
module String = Belt_MapString

(** seprate function from data, a more verboe but slightly
    more efficient
*)
module Dict = Belt_MapDict



type ('key, 'id ) id = ('key, 'id) Belt_Id.comparable
type ('key, 'id ) cmp = ('key, 'id) Belt_Id.cmp

type ('k,'v,'id) t = {
    cmp: ('k,'id) cmp;
    data: ('k,'v, 'id) Dict.t
}





let fromArray (type k idx) data ~(id : (k,idx) id)  =
  let module M = (val id) in
  let cmp = M.cmp in
  {cmp; data = (Dict.fromArray ~cmp data)}

let remove m x  =
  let {cmp; data=odata} = m in
  let newData = Dict.remove odata x ~cmp  in
  if newData == odata then m
  else {cmp; data = newData}

let removeMany m x =
  let {cmp; data=odata} = m  in
  let newData = Dict.removeMany odata x ~cmp in
  {cmp;  data = newData}

let set m key d  =
  let cmp = m.cmp in
  {cmp; data = Dict.set ~cmp m.data key d}

let mergeMany m e =
  let cmp = m.cmp in
  {cmp; data = Dict.mergeMany ~cmp m.data e}

let updateU m key f  =
  let cmp = m.cmp in
  {cmp; data = Dict.updateU ~cmp m.data key f }
let update m key f = updateU m key (fun [@bs] a  -> f a )
let split m x =
  let cmp = m.cmp in
  let (l,r),b = Dict.split ~cmp m.data x in
  ({cmp; data=l}, {cmp;  data=r}), b

let mergeU s1 s2 f =
  let cmp = s1.cmp in
  {cmp; data=(Dict.mergeU ~cmp  (s1.data) (s2.data) f)}

let merge s1 s2 f =
  mergeU s1 s2 (fun [@bs] a b c -> f a b c)

let make (type key idx) ~(id: (key, idx) id) =
  let module M = (val id) in
  {cmp = M.cmp; data = Dict.empty}

let isEmpty map =
  Dict.isEmpty map.data

let findFirstByU m f = Dict.findFirstByU m.data f
let findFirstBy m f = findFirstByU m (fun [@bs] a b -> f a b) 
let forEachU m f = Dict.forEachU m.data f
let forEach m f = forEachU m (fun [@bs] a b -> f a b)
let reduceU m acc f = Dict.reduceU m.data acc f
let reduce m acc f = reduceU m acc (fun[@bs] a b c -> f a b c )
let everyU m f = Dict.everyU m.data f
let every m f = everyU m (fun [@bs] a b -> f a b)
let someU m f = Dict.someU m.data f
let some m f = someU m (fun[@bs] a b -> f a b)
let keepU m f =
  {cmp = m.cmp; data = (Dict.keepU m.data f)}
let keep m f = keepU m (fun [@bs] a b -> f a b)

let partitionU m p =
  let cmp = m.cmp in
  let l,r = m . data |. Dict.partitionU  p in
  {cmp; data=l}, {cmp; data=r}
let partition m p = partitionU m (fun [@bs] a b -> p a b)

let mapU m f =
  {cmp = m.cmp; data = Dict.mapU m.data f}
let map m f = mapU m (fun [@bs] a  -> f a)
let mapWithKeyU m  f =
  {cmp = m.cmp; data = Dict.mapWithKeyU m.data f}
let mapWithKey m f = mapWithKeyU m (fun [@bs] a b -> f a b)
let size map = Dict.size map.data
let toList map = Dict.toList map.data
let toArray m = Dict.toArray m.data
let keysToArray m = Dict.keysToArray m.data
let valuesToArray m = Dict.valuesToArray m.data
let minKey m = Dict.minKey m.data
let minKeyUndefined m = Dict.minKeyUndefined m.data
let maxKey m = Dict.maxKey m.data
let maxKeyUndefined m = Dict.maxKeyUndefined m.data
let minimum m = Dict.minimum m.data
let minUndefined m = Dict.minUndefined m.data
let maximum m = Dict.maximum m.data
let maxUndefined m = Dict.maxUndefined m.data

let get map x  =
  Dict.get ~cmp:map.cmp  map.data x

let getUndefined map x =
  Dict.getUndefined ~cmp:map.cmp map.data x

let getWithDefault map x def =
  Dict.getWithDefault ~cmp:map.cmp map.data x  def

let getExn map x =
  Dict.getExn ~cmp:map.cmp map.data x

let has map x =
  Dict.has ~cmp:map.cmp map.data x

let checkInvariantInternal m  =
  Dict.checkInvariantInternal m.data

let eqU m1 m2 veq =
  Dict.eqU ~kcmp:m1.cmp ~veq m1.data m2.data
let eq m1 m2 veq = eqU m1 m2 (fun[@bs] a b -> veq a b)

let cmpU m1 m2 vcmp =
  Dict.cmpU ~kcmp:m1.cmp  ~vcmp m1.data m2.data
let cmp m1 m2 vcmp = cmpU m1 m2 (fun [@bs] a b -> vcmp a b)

let getData m = m.data

let getId (type key identity) (m : (key,_,identity) t) : (key, identity) id =
  let module T = struct
    type nonrec identity = identity
    type nonrec t = key
    let cmp =  m.cmp
  end in
  (module T )

let packIdData (type key idx) ~(id : (key, idx) id) ~data  =
  let module M = (val id) in
  {cmp = M.cmp ; data}

