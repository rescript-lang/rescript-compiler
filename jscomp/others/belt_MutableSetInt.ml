# 1 "others/setm.cppo.ml"
(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

(** This module is [`Belt.MutableSet`]() specialized with key type to be a primitive type.
    It is more efficient in general, the  API is the same with [`Belt_MutableSet`]() except its key type is fixed,
    and identity is not needed(using the built-in one)
*)

# 31 "others/setm.cppo.ml"
module I = Belt_internalSetInt
module S = Belt_SortArrayInt
# 39 "others/setm.cppo.ml"
module N = Belt_internalAVLset
module A = Belt_Array

type value = I.value
(** The type of the set elements. *)


type t = {
  mutable data : I.t
}
(** The type of sets. *)


let rec remove0 nt (x : value)=
  let k = nt.N.value in
  if x = k then
    let {N.left = l; right = r} = nt in
    match l, r with
    | None, _ -> r
    | _, None -> l
    | Some _,  Some nr ->
      nt.right <- (N.removeMinAuxWithRootMutate nt nr);
      Some (N.balMutate nt)
  else
    begin
      if x < k then
        match nt.left with
        | None -> Some nt
        | Some l ->
          nt.left <- (remove0 l x );
          Some (N.balMutate nt)
      else
        match nt.right with
        | None -> Some nt
        | Some r ->
          nt.right <- (remove0 r x);
          Some (N.balMutate nt)
    end

let remove d v =
  let oldRoot = d.data in
  match oldRoot with
  | None -> ()
  | Some oldRoot2 ->
  let newRoot = remove0 oldRoot2 v in
  if newRoot != oldRoot then
    d.data <- newRoot

let rec removeMany0 t xs i len  =
  if i < len then
    let ele = A.getUnsafe xs i in
    let u = remove0 t ele in
    match u with
    | None -> None
    | Some t -> removeMany0 t xs (i+1) len
  else Some t


let removeMany  (d : t) xs =
  let oldRoot = d.data in
  match oldRoot with
  | None -> ()
  | Some nt ->
    let len = A.length xs in
    d.data <- removeMany0 nt xs 0 len

let rec removeCheck0  nt (x : value) removed =
  let k = nt.N.value in
  if x = k then
    let () = removed .contents<- true in
    let {N.left = l; right = r} = nt in
    match l, r with
    | None, _ -> r
    | _ , None -> l
    | Some _,  Some nr ->
      nt.right <- (N.removeMinAuxWithRootMutate nt nr);
      Some (N.balMutate nt)
  else
    begin
      if x < k then
        match nt.left with
        | None -> Some nt
        | Some l ->
          nt.left <- (removeCheck0  l x removed);
          Some (N.balMutate nt)
      else
        match nt.right with
        | None -> Some nt
        | Some r ->
          nt.right <- (removeCheck0  r x removed);
          Some (N.balMutate nt)
    end



let removeCheck  (d :  t) v =
  let oldRoot = d.data in
  match oldRoot with
  | None -> false
  | Some oldRoot2 ->
    let removed = ref false in
    let newRoot = removeCheck0  oldRoot2 v removed in
    if newRoot != oldRoot then
      d.data <- newRoot ;
    removed.contents


let rec addCheck0  t (x : value) added  =
  match t with
  | None ->
    added .contents<- true;
    N.singleton x
  | Some nt ->
    let k = nt.N.value in
    if x = k then t
    else
      let {N.left = l; right =  r} = nt in
      (if x < k then
         let ll = addCheck0  l x added in
         nt.left <- ll
       else
         nt.right <- (addCheck0 r x added );
      );
      Some (N.balMutate nt)

let addCheck (m :  t) e =
  let oldRoot = m.data in
  let added = ref false in
  let newRoot = addCheck0 oldRoot e added in
  if newRoot != oldRoot then
    m.data <- newRoot;
  added.contents

let add d k =
  let oldRoot = d.data in
  let v = I.addMutate oldRoot k in
  if v != oldRoot then
     d.data <- v


let addArrayMutate t  xs =
  let v = ref t in
  for i = 0 to A.length xs - 1 do
    v.contents<- I.addMutate v.contents (A.getUnsafe xs i)
  done ;
  v.contents

let mergeMany d arr =
  d.data <- addArrayMutate (d.data) arr



let make  () = {data = None}

let isEmpty d =
  N.isEmpty (d.data)

let minimum d =
  N.minimum (d.data)

let minUndefined d =
  N.minUndefined (d.data)

let maximum d = N.maximum (d.data)

let maxUndefined d = N.maxUndefined (d.data)

let forEachU d f = N.forEachU (d.data) f
let forEach d f = forEachU d (fun[@bs] a -> f a)

let reduceU d acc cb = N.reduceU (d.data) acc cb
let reduce d acc cb = reduceU d acc (fun[@bs] a b -> cb a b)

let everyU d p = N.everyU (d.data) p
let every d p = everyU d (fun[@bs] a -> p a)
let someU d p = N.someU (d.data) p
let some d p = someU d (fun [@bs] a -> p a)
let size d =
  N.size (d.data)
let toList d =
  N.toList (d.data)
let toArray d =
  N.toArray (d.data)


let fromSortedArrayUnsafe xs =
  {data = N.fromSortedArrayUnsafe xs}

let checkInvariantInternal d =
  N.checkInvariantInternal (d.data)



let fromArray xs =
  { data = I.fromArray xs}

let cmp d0 d1 =
  I.cmp (d0.data) (d1.data)
let eq d0 d1 =
  I.eq (d0.data) (d1.data)
let get d x =
  I.get (d.data) x
let getUndefined d x =
  I.getUndefined (d.data) x
let getExn d x =
  I.getExn (d.data) x

let split d  key =
  let arr = N.toArray (d.data) in
  let i = S.binarySearch arr key   in
  let len = A.length arr in
  if i < 0 then
    let next = - i -1 in
    (
      {data = N.fromSortedArrayAux arr 0 next}
    ,

      {data = N.fromSortedArrayAux arr next (len - next)}
    ), false
  else
    (
      {data = N.fromSortedArrayAux arr 0 i}
    ,

      {data = (N.fromSortedArrayAux arr (i+1) (len - i - 1))}
      ), true

let keepU d p =
  {data = (N.keepCopyU (d.data) p )}
let keep d p = keepU d (fun[@bs] a -> p a)

let partitionU d p =
  let a , b = N.partitionCopyU (d.data) p in
  {data = a}, {data = b}
let partition d p = partitionU d (fun[@bs] a -> p a)

let subset a b = I.subset  a.data b.data
let intersect dataa datab  =
  let dataa, datab = dataa.data, datab.data in
    match dataa, datab with
    | None, _ -> make ()
    | _, None -> make ()
    | Some dataa0, Some datab0 ->
    let sizea, sizeb =
        N.lengthNode dataa0, N.lengthNode datab0 in
    let totalSize = sizea + sizeb in
    let tmp = A.makeUninitializedUnsafe totalSize in
    ignore @@ N.fillArray dataa0 0 tmp ;
    ignore @@ N.fillArray datab0 sizea tmp;
    if ((A.getUnsafe tmp (sizea - 1) <
        A.getUnsafe tmp sizea))
      ||
      (
      (A.getUnsafe tmp (totalSize - 1) <
      A.getUnsafe tmp 0)
      )
       then make ()
    else
    let tmp2 = A.makeUninitializedUnsafe (Pervasives.min sizea sizeb) in
    let k = S.intersect tmp 0 sizea tmp sizea sizeb tmp2 0  in
    {data = (N.fromSortedArrayAux tmp2 0 k)}

let diff dataa datab : t =
  let dataa, datab = dataa.data, datab.data in
  match dataa, datab with
  | None, _ -> make ()
  | _, None -> {data = N.copy dataa}
  | Some dataa0, Some datab0 ->
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in
    let totalSize = sizea + sizeb in
    let tmp = A.makeUninitializedUnsafe totalSize in
    ignore @@ N.fillArray dataa0 0 tmp ;
    ignore @@ N.fillArray datab0 sizea tmp;
    if ( (A.getUnsafe tmp (sizea - 1)) <
        (A.getUnsafe tmp sizea))
      ||
      (A.getUnsafe tmp (totalSize - 1)
      < A.getUnsafe tmp 0)
       then {data = N.copy dataa}
    else
    let tmp2 = A.makeUninitializedUnsafe sizea in
    let k = S.diff tmp 0 sizea tmp sizea sizeb tmp2 0  in
    {data = (N.fromSortedArrayAux tmp2 0 k)}

let union (dataa : t)  (datab : t) : t =
  let dataa, datab = dataa.data, datab.data in
   match dataa, datab with
  | None, _ -> {data = (N.copy datab)}
  | _, None -> {data = (N.copy dataa)}
  | Some dataa0, Some datab0
    ->
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in
    let totalSize = sizea + sizeb in
    let tmp = A.makeUninitializedUnsafe totalSize in
    ignore @@ N.fillArray dataa0 0 tmp ;
    ignore @@ N.fillArray datab0 sizea tmp ;
    if
      (A.getUnsafe tmp (sizea - 1) <
      A.getUnsafe tmp sizea)  then
      {data = (N.fromSortedArrayAux tmp 0 totalSize) }
    else
      let tmp2 = A.makeUninitializedUnsafe totalSize in
      let k = S.union tmp 0 sizea tmp sizea sizeb tmp2 0  in
      {data = (N.fromSortedArrayAux tmp2 0 k) }

let has d x = I.has (d.data) x

let copy d = {data = (N.copy (d.data))}
