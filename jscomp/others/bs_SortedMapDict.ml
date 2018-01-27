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

module N = Bs_internalAVLtree
module A = Bs_Array

type ('key,  'a, 'id) t = ('key, 'a) N.t0

type ('key, 'id) cmp = ('key, 'id)  Bs_Cmp.cmp

let empty = N.empty0
let ofArray = N.ofArray0
let isEmpty = N.isEmpty0
let cmp = N.cmp0
let eq = N.eq0   
let has = N.mem0
let forEach = N.iter0      
let reduce = N.fold0
let every = N.every0
let some = N.some0    
let size = N.length0
let toList = N.toList0
let toArray = N.toArray0
let keysToArray = N.keysToArray0
let valuesToArray = N.valuesToArray0

let minimum = N.minKVOpt0
let maximum = N.maxKVOpt0
let minKey = N.minKeyOpt0
let maxKey = N.maxKeyOpt0
let minKeyNull = N.minKeyNull0
let maxKeyNull = N.maxKeyNull0
let minNull = N.minKVNull0
let maxNull = N.maxKVNull0
let get = N.findOpt0
let getNull = N.findNull0
let getWithDefault = N.findWithDefault0
let getExn = N.findExn0

let mapWithKey = N.mapi0
let map  = N.map0

let keepBy = N.filterShared0
let partition = N.partitionShared0
let checkInvariantInternal = N.checkInvariantInternal
let rec set  (t : _ t) newK newD  ~cmp =
  match N.toOpt t with 
  | None -> N.singleton0 newK newD 
  | Some n  ->
    let k= N.key n in 
    let c = (Bs_Cmp.getCmp cmp) newK k [@bs] in
    if c = 0 then
      N.return (N.updateValue n newD) 
    else 
      let l,r,v = N.left n, N.right n, N.value n in 
      if c < 0 then (* Worth optimize for reference equality? *)
        N.bal (set ~cmp l newK newD ) k v  r
      else
        N.bal l k v (set ~cmp r newK newD )

let rec update  (t : _ t) newK f  ~cmp :  _ t =
  match N.toOpt t with 
  | None ->
    begin match f None [@bs] with 
      | None -> t 
      | Some newD -> N.singleton0 newK newD 
    end 
  | Some n  ->
    let k= N.key n in 
    let c = (Bs_Cmp.getCmp cmp) newK k [@bs] in
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
        let ll = (update ~cmp l newK f ) in
        if l == ll then
          t
        else 
          N.bal ll k v  r            
      else
        let rr = (update ~cmp r newK f) in
        if r == rr then t 
        else N.bal l k v rr

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
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
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
  | None -> N.empty0
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
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in 
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

let rec merge s1 s2 f ~cmp =
  match N.(toOpt s1, toOpt s2) with
    (None, None) -> N.empty
  | Some _, None -> 
    N.filterMap0 s1 (fun[@bs] k v -> 
        f k (Some v) None [@bs]
      )
  | None, Some _ -> 
    N.filterMap0 s2 (fun[@bs] k v -> 
        f k None (Some v) [@bs]
      )
  | Some s1n , Some s2n -> 
    if N.h s1n  >= N.h s2n  then
      let l1, v1, d1, r1 = N.(left s1n, key s1n, value s1n, right s1n) in 
      let d2 = ref None in 
      let (l2, r2) = splitAuxPivot ~cmp s2n v1 d2 in
      let d2 = !d2 in 
      let newLeft = merge ~cmp l1 l2 f in 
      let newD = f v1 (Some d1) d2 [@bs] in 
      let newRight = merge ~cmp r1 r2 f in 
      N.concatOrJoin newLeft v1 newD  newRight
    else
      let l2,v2,d2,r2 = N.(left s2n, key s2n, value s2n, right s2n) in 
      let d1 = ref None in 
      let (l1,  r1) = splitAuxPivot ~cmp s1n v2 d1 in
      let d1 = !d1 in 
      let newLeft = merge ~cmp l1 l2 f in 
      let newD = (f v2 d1 (Some d2) [@bs]) in 
      let newRight = (merge ~cmp r1 r2 f) in 
      N.concatOrJoin newLeft v2 newD newRight

let rec removeArrayAux t xs i len ~cmp =
  if i < len then
    let ele = A.getUnsafe xs i in
    let u =  removeAux0 t ele ~cmp in
    match N.toOpt u with
    | None -> u
    | Some t -> removeArrayAux t xs (i + 1) len ~cmp 
  else
    N.return t
      
let removeMany t keys ~cmp =
  let len = A.length keys in
  match N.toOpt t with
  | None -> N.empty0
  | Some t ->  removeArrayAux t keys 0 len ~cmp 




