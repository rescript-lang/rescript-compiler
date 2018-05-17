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

module N = Belt_internalAVLtree
module A = Belt_Array

type ('key,  'a, 'id) t = ('key, 'a) N.t

type ('key, 'id) cmp = ('key, 'id)  Belt_Id.cmp

let empty = N.empty
let fromArray = N.fromArray
let isEmpty = N.isEmpty
let cmp = N.cmp
let cmpU = N.cmpU             
let eq = N.eq
let eqU = N.eqU  
let has = N.has

let forEach = N.forEach
let forEachU = N.forEachU
let reduce = N.reduce
let reduceU = N.reduceU
let every = N.every                
let everyU = N.everyU
let some = N.some 
let someU = N.someU
              
let size = N.size
let toList = N.toList
let toArray = N.toArray
let keysToArray = N.keysToArray
let valuesToArray = N.valuesToArray

let minimum = N.minimum
let maximum = N.maximum
let minKey = N.minKey
let maxKey = N.maxKey
let minKeyUndefined = N.minKeyUndefined
let maxKeyUndefined = N.maxKeyUndefined
let minUndefined = N.minUndefined
let maxUndefined = N.maxUndefined
let get = N.get
let getUndefined = N.getUndefined
let getWithDefault = N.getWithDefault
let getExn = N.getExn

let mapWithKey = N.mapWithKey
let mapWithKeyU = N.mapWithKeyU

let mapU  = N.mapU
let map = N.map
let keep = N.keepShared 
let keepU = N.keepSharedU
let partitionU = N.partitionSharedU
let partition = N.partitionShared                   
let checkInvariantInternal = N.checkInvariantInternal
let rec set  (t : _ t) newK newD  ~cmp =
  match N.toOpt t with 
  | None -> N.singleton newK newD 
  | Some n  ->
    let k= N.key n in 
    let c = (Belt_Id.getCmpInternal cmp) newK k [@bs] in
    if c = 0 then
      N.return (N.updateValue n newD) 
    else 
      let l,r,v = N.left n, N.right n, N.value n in 
      if c < 0 then (* Worth optimize for reference equality? *)
        N.bal (set ~cmp l newK newD ) k v  r
      else
        N.bal l k v (set ~cmp r newK newD )

let rec updateU  (t : _ t) newK f  ~cmp :  _ t =
  match N.toOpt t with 
  | None ->
    begin match f None [@bs] with 
      | None -> t 
      | Some newD -> N.singleton newK newD 
    end 
  | Some n  ->
    let k= N.key n in 
    let c = (Belt_Id.getCmpInternal cmp) newK k [@bs] in
    if c = 0 then
      match f (Some (N.value n)) [@bs] with 
      | None ->
        let l, r = N.left n , N.right n in  
        begin match N.toOpt l, N.toOpt r with
        | None, _ -> r
        | _, None -> l
        | _, Some rn ->
          let kr, vr = ref (N.key rn), ref (N.value rn) in
          let r = N.removeMinAuxWithRef rn kr vr in
          N.bal l !kr !vr r 
        end
      | Some newD -> N.return (N.updateValue n newD)
    else 
      let l,r,v = N.left n, N.right n, N.value n in 
      if c < 0 then
        let ll = (updateU ~cmp l newK f ) in
        if l == ll then
          t
        else 
          N.bal ll k v  r            
      else
        let rr = (updateU ~cmp r newK f) in
        if r == rr then t 
        else N.bal l k v rr

let update t newK f ~cmp =
  updateU t newK (fun [@bs] a  -> f a ) ~cmp
    
(*  unboxing API was not exported
    since the correct API is really awkard
    [bool -> 'k Js.null -> ('a Js.null * bool)]
    even for specialized [k] the first [bool] can 
    be erased, maybe the perf boost does not justify the inclusion of such API

    [updateWithNull m x f]
    the callback to [f exist v] 
    when [v] is non-null,
    [exist] is guaranteed to be true
    [v] is guranteed to be [null],
    when [exist] is [true], [v] could be [null], 
    since ['a] is polymorphic
*)


let rec removeAux0  n x ~cmp = 
  let l,v,r = N.(left n, key n, right n ) in 
  let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in
  if c = 0 then
    match N.toOpt l, N.toOpt r with 
    | None, _ -> r 
    | _, None -> l 
    | _, Some rn -> 
      let kr, vr = ref (N.key rn), ref (N.value rn) in 
      let r = N.removeMinAuxWithRef rn kr vr in 
      N.bal l !kr !vr r
  else if c < 0 then
    match N.toOpt l with 
    | None -> N.return n (* Nothing to remove *)
    | Some left ->
      let ll = removeAux0 left x ~cmp in 
      if ll == l then (N.return n)
      else N.bal ll v (N.value n) r
  else
    match N.toOpt r with 
    | None -> N.return n (* Nothing to remove *)
    | Some right -> 
      let rr = removeAux0 ~cmp right x in
      if rr == r then N.return n
      else N.bal l v (N.value n)  rr


let remove n x ~cmp = 
  match N.toOpt n with        
  | None -> N.empty
  | Some n -> removeAux0 n x ~cmp 
  
let mergeMany   h arr ~cmp =   
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key,value = A.getUnsafe arr i in 
    v := set !v  ~cmp key value
  done ;
  !v 

let rec splitAuxPivot n x pres  ~cmp =  
  let l,v,d,r = N.(left n , key n, value n, right n) in  
  let c = (Belt_Id.getCmpInternal cmp) x v [@bs] in 
  if c = 0 then begin 
    pres := Some d; 
    (l,  r)
  end
  else     
  if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.(empty, return n)
    | Some l -> 
      let (ll,rl) = splitAuxPivot ~cmp l x pres in
      (ll,  N.join rl v d r)
  else
    match N.toOpt r with 
    | None ->
      N.(return n, empty)
    | Some r -> 
      let (lr,  rr) = splitAuxPivot ~cmp r x pres in
      (N.join l v d lr,  rr)


let split  n x ~cmp = 
  match N.toOpt n with 
  | None ->     
    N.(empty, empty), None
  | Some n  ->
    let pres = ref None in
    let v = splitAuxPivot ~cmp n x pres in 
    v, !pres

let rec mergeU s1 s2 f ~cmp =
  match N.(toOpt s1, toOpt s2) with
    (None, None) -> N.empty
  | Some _, None -> 
    N.keepMapU s1 (fun[@bs] k v -> 
        f k (Some v) None [@bs]
      )
  | None, Some _ -> 
    N.keepMapU s2 (fun[@bs] k v -> 
        f k None (Some v) [@bs]
      )
  | Some s1n , Some s2n -> 
    if N.height s1n  >= N.height s2n  then
      let l1, v1, d1, r1 = N.(left s1n, key s1n, value s1n, right s1n) in 
      let d2 = ref None in 
      let (l2, r2) = splitAuxPivot ~cmp s2n v1 d2 in
      let d2 = !d2 in 
      let newLeft = mergeU ~cmp l1 l2 f in 
      let newD = f v1 (Some d1) d2 [@bs] in 
      let newRight = mergeU ~cmp r1 r2 f in 
      N.concatOrJoin newLeft v1 newD  newRight
    else
      let l2,v2,d2,r2 = N.(left s2n, key s2n, value s2n, right s2n) in 
      let d1 = ref None in 
      let (l1,  r1) = splitAuxPivot ~cmp s1n v2 d1 in
      let d1 = !d1 in 
      let newLeft = mergeU ~cmp l1 l2 f in 
      let newD = (f v2 d1 (Some d2) [@bs]) in 
      let newRight = (mergeU ~cmp r1 r2 f) in 
      N.concatOrJoin newLeft v2 newD newRight

let merge s1 s2 f ~cmp =
  mergeU s1 s2 (fun[@bs] a b c -> f a b c) ~cmp
    
let rec removeMany0 t xs i len ~cmp =
  if i < len then
    let ele = A.getUnsafe xs i in
    let u =  removeAux0 t ele ~cmp in
    match N.toOpt u with
    | None -> u
    | Some t -> removeMany0 t xs (i + 1) len ~cmp 
  else
    N.return t
      
let removeMany t keys ~cmp =
  let len = A.length keys in
  match N.toOpt t with
  | None -> N.empty
  | Some t ->  removeMany0 t keys 0 len ~cmp 




