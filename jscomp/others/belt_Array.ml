(***********************************************************************)
(*                                                                     *)
(*                           OCaml                                     *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Array operations *)

external length: 'a array -> int = "%array_length"
external size: 'a array -> int = "%array_length"
external getUnsafe: 'a array -> int -> 'a = "%array_unsafe_get"
external setUnsafe: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external (.!()): 'a array -> int -> 'a = "%array_unsafe_get"
external (.!()<-): 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external getUndefined: 'a array -> int -> 'a Js.undefined = "%array_unsafe_get"
(* external get: 'a array -> int -> 'a = "%array_safe_get" *)
let get arr i =
  if i >= 0 && i < length arr then Some arr.!(i) else None
let getExn arr i =
    assert (i >= 0 && i < length arr) ;
    arr.!(i)
let set arr i v =
  if i >= 0 && i < length arr then (arr.!(i) <- v; true) else false

let setExn arr i v =
  assert (i >= 0 && i < length arr);
  arr.!(i) <- v


external truncateToLengthUnsafe : 'a array -> int ->  unit = "length" [@@bs.set]
external makeUninitialized : int -> 'a Js.undefined array = "Array" [@@bs.new]
external makeUninitializedUnsafe : int -> 'a  array = "Array" [@@bs.new]


external copy : 'a array -> (_ [@bs.as 0]) -> 'a array =
  "slice"  [@@bs.send]


let swapUnsafe xs i j =
  let tmp = xs.!(i) in
  xs.!(i) <- xs.!(j) ;
  xs.!(j) <- tmp

let shuffleInPlace xs =
  let len = length xs in
  for i = 0 to len - 1 do
    swapUnsafe xs i (Js_math.random_int i len) (* [i,len)*)
  done

let shuffle xs =
  let result = copy xs in
  shuffleInPlace result; (* TODO: improve*)
  result

let reverseAux xs ofs len =
  for i = 0 to len/2 - 1 do
    swapUnsafe xs (ofs + i) (ofs + len - i - 1)
  done

let reverseInPlace xs =
  let len = length xs in
  reverseAux xs 0 len

let reverse xs =
  let len = length xs in
  let result = makeUninitializedUnsafe len in
  for i = 0 to len - 1 do
    result.!(i) <- xs.!(len - 1 - i)
  done;
  result

let make l f =
  if l <= 0 then [||]
  else
    let res = makeUninitializedUnsafe l in
    for i = 0 to  l - 1 do
      res.!(i) <- f
    done;
    res


(* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... *)
let makeByU l f =
  if l <= 0 then [||]
  else
    let res = makeUninitializedUnsafe l in
    for i = 0 to  l - 1 do
      res.!(i) <- f i [@bs]
    done;
    res

let makeBy l f = makeByU l (fun[@bs] a -> f a)

let makeByAndShuffleU l f =
  let u  = makeByU l f in
  shuffleInPlace u ;
  u

let makeByAndShuffle l f = makeByAndShuffleU l (fun[@bs] a -> f a)

let range start finish =
  let cut = finish - start in
  if cut < 0  then [||]
  else
    let arr = makeUninitializedUnsafe (cut + 1 ) in
    for i = 0 to cut do
       arr.!(i) <- start + i
    done;
    arr

let rangeBy start finish ~step =
  let cut = finish - start in
  if cut < 0 || step <=0 then
    [||]
  else
    let nb = cut/step + 1 in
    let arr = makeUninitializedUnsafe  nb in
    let cur = ref start in
    for i = 0 to nb - 1 do
      arr.!(i) <- cur.contents;
      cur.contents<- cur.contents + step ;
    done;
    arr

let zip xs ys =
  let lenx, leny = length xs, length ys in
  let len = Pervasives.min lenx leny  in
  let s = makeUninitializedUnsafe len in
  for i = 0 to len - 1 do
    s.!(i) <- xs.!(i), ys.!(i)
  done ;
  s

let zipByU xs ys f =
  let lenx, leny = length xs, length ys in
  let len = Pervasives.min lenx leny  in
  let s = makeUninitializedUnsafe len in
  for i = 0 to len - 1 do
    s.!(i) <- f xs.!(i) ys.!(i) [@bs]
  done ;
  s

let zipBy xs ys f = zipByU xs ys (fun [@bs] a b -> f a b)

let concat a1 a2 =
  let l1 = length a1 in
  let l2 = length a2 in
  let a1a2 = makeUninitializedUnsafe (l1 + l2) in
  for i = 0 to l1 - 1 do
    a1a2.!(i) <- a1.!( i)
  done ;
  for i = 0 to l2 - 1 do
    a1a2.!(l1 + i) <-  a2.!(i)
  done ;
  a1a2

let concatMany arrs =
  let lenArrs = length arrs in
  let totalLen = ref 0 in
  for i = 0 to lenArrs - 1 do
    totalLen .contents<- totalLen.contents + length arrs.!(i)
  done;
  let result = makeUninitializedUnsafe totalLen.contents in
  totalLen.contents<- 0 ;
  for j = 0 to lenArrs - 1 do
    let cur = arrs.!(j) in
    for k = 0 to length cur - 1 do
      result.!(totalLen.contents) <- cur.!(k);
      totalLen.contents <- totalLen.contents + 1
    done
  done ;
  result

let slice a ~offset ~len =
  if len <= 0  then  [||]
  else
    let lena = length a in
    let ofs =
      if offset < 0 then
        Pervasives.max (lena + offset) 0
      else offset in
    let hasLen = lena - ofs in
    let copyLength = Pervasives.min hasLen len in
    if copyLength <= 0 then [||]
    else
      let result = makeUninitializedUnsafe copyLength  in
      for i = 0 to copyLength - 1 do
        result.!(i) <- a.!(ofs + i)
      done ;
      result

let sliceToEnd a offset =
  let lena = length a in
  let ofs = if offset < 0 then Pervasives.max (lena + offset) 0 else offset in
  let len = lena - ofs in
  let result = makeUninitializedUnsafe len in
  for i = 0 to len - 1 do
    result.!(i) <- a.!(ofs + i)
  done;
  result

let fill a ~offset ~len v =
  if len > 0 then
    let lena = length a in
    let ofs =
      if offset < 0 then
        Pervasives.max (lena + offset ) 0
      else offset in
    let hasLen = lena - ofs in
    let fillLength = Pervasives.min hasLen len in
    if fillLength > 0 then
      for i = ofs to  ofs + fillLength - 1 do
        a.!(i) <- v
      done


let blitUnsafe ~src:a1  ~srcOffset:srcofs1 ~dst:a2 ~dstOffset:srcofs2 ~len:blitLength =
  if srcofs2 <= srcofs1 then
    for j = 0 to blitLength - 1 do
      a2.!(j + srcofs2) <-  a1.!(j + srcofs1)
    done
  else
    for j = blitLength - 1 downto 0 do
      a2.!(j + srcofs2) <-  a1.!(j + srcofs1)
    done

(* We don't need check `blitLength` since when `blitLength < 0` the
   for loop will be nop
*)
let blit ~src:a1 ~srcOffset:ofs1 ~dst:a2 ~dstOffset:ofs2 ~len =
    let lena1 = length a1 in
    let lena2 = length a2 in
    let srcofs1 = if ofs1 < 0 then Pervasives.max (lena1 + ofs1) 0 else ofs1 in
    let srcofs2 = if ofs2 < 0 then Pervasives.max (lena2 + ofs2) 0 else ofs2 in
    let blitLength =
      Pervasives.min len (Pervasives.min (lena1 - srcofs1) (lena2 - srcofs2)) in
    (* blitUnsafe a1 srcofs1 a2 srcofs2 blitLength *)
    (if srcofs2 <= srcofs1 then
      for j = 0 to blitLength - 1 do
        a2.!(j + srcofs2) <- a1.!(j + srcofs1)
      done
    else
      for j = blitLength - 1 downto 0 do
        a2.!(j + srcofs2) <- a1.!(j + srcofs1)
      done)

let forEachU a f =
  for i = 0 to length a - 1 do f a.!(i) [@bs] done

let forEach a f = forEachU a (fun[@bs] a -> f a)

let mapU a f =
  let l = length a in
  let r = makeUninitializedUnsafe l in
  for i = 0 to l - 1 do
    r.!(i) <- f a.!(i) [@bs]
  done;
  r

let map a f = mapU a (fun[@bs] a -> f a)

let getByU a p =
  let l = length a in
  let i = ref 0 in
  let r = ref None in
  while r.contents = None && i.contents < l do
    let v = a.!(i.contents) in
    if p v [@bs] then
      begin
        r.contents<- Some v;
      end;
    i.contents <- i.contents + 1
  done;
  r.contents

let getBy a p = getByU a (fun[@bs] a -> p a)

let getIndexByU a p =
  let l = length a in
  let i = ref 0 in
  let r = ref None in
  while r.contents = None && i.contents < l do
    let v = a.!(i.contents) in
    if p v [@bs] then
      begin
        r .contents<- Some i.contents;
      end;
    i.contents <- i.contents + 1
  done;
  r.contents

let getIndexBy a p = getIndexByU a (fun[@bs] a -> p a)

let keepU a f =
  let l = length a in
  let r = makeUninitializedUnsafe l in
  let j = ref 0 in
  for i = 0 to l - 1 do
    let v = a.!(i) in
    if f v [@bs] then
      begin
        r.!(j.contents) <- v;
        j.contents <- j.contents + 1
      end
  done;
  truncateToLengthUnsafe r j.contents;
  r

let keep a f = keepU a (fun [@bs] a -> f a)

let keepWithIndexU a f =
  let l = length a in
  let r = makeUninitializedUnsafe l in
  let j = ref 0 in
  for i = 0 to l - 1 do
    let v =  a.!(i) in
    if f v i [@bs] then
      begin
        r.!(j.contents) <- v;
        j.contents <- j.contents + 1
      end
  done;
  truncateToLengthUnsafe r j.contents;
  r

let keepWithIndex a f = keepWithIndexU a (fun [@bs] a i -> f a i)

let keepMapU a f =
  let l = length a in
  let r = makeUninitializedUnsafe l in
  let j = ref 0 in
  for i = 0 to l - 1 do
    let v = a.!(i) in
    match f v [@bs] with
    | None -> ()
    | Some v ->
      begin
        r.!(j.contents) <- v;
        j.contents <- j.contents + 1
      end
  done;
  truncateToLengthUnsafe r j.contents;
  r

let keepMap a f = keepMapU a (fun[@bs] a -> f a)

let forEachWithIndexU a f=
  for i = 0 to length a - 1 do f i a.!(i) [@bs] done

let forEachWithIndex a f = forEachWithIndexU a (fun[@bs] a b -> f a b)

let mapWithIndexU  a f =
  let l = length a in
  let r = makeUninitializedUnsafe l in
  for i = 0 to l - 1 do
    r.!(i) <- f i a.!(i) [@bs]
  done;
  r

let mapWithIndex a f = mapWithIndexU a (fun[@bs] a b -> f a b)

let reduceU a x f =
  let r = ref x in
  for i = 0 to length a - 1 do
    r .contents<- f r.contents a.!(i) [@bs]
  done;
  r.contents

let reduce a x f = reduceU a x (fun[@bs] a b -> f a b)

let reduceReverseU a x f =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r .contents<- f  r.contents a.!(i) [@bs]
  done;
  r.contents

let reduceReverse a x f = reduceReverseU a x (fun[@bs] a b -> f a b)

let reduceReverse2U a b x f =
  let r = ref x in
  let len = Pervasives.min (length a) (length b) in
  for i = len - 1 downto  0 do
    r.contents<- f r.contents a.!(i) b.!(i) [@bs]
  done;
  r.contents

let reduceReverse2 a b x f =
  reduceReverse2U a b x (fun [@bs] a b c -> f a b c)

let reduceWithIndexU a x f =
  let r = ref x in
  for i = 0 to length a - 1 do
    r .contents<- f r.contents a.!(i) i [@bs]
  done;
  r.contents

let reduceWithIndex a x f =
  reduceWithIndexU a x (fun[@bs] a b c -> f a b c)

let rec everyAux arr i b len =
  if i = len then true
  else if b arr.!(i) [@bs] then
    everyAux arr (i + 1) b len
  else false

let rec someAux arr i b len =
  if i = len then false
  else
  if b arr.!(i) [@bs] then true
  else someAux arr (i + 1) b len

let everyU arr b =
  let len = length arr in
  everyAux arr 0 b len

let every arr f = everyU arr (fun[@bs] b -> f b)

let someU arr b =
  let len = length arr in
  someAux arr 0 b len
let some arr f = someU arr (fun [@bs] b -> f b)

let rec everyAux2 arr1 arr2 i b len =
  if i = len then true
  else if b arr1.!(i) arr2.!(i) [@bs] then
    everyAux2 arr1 arr2 (i + 1) b len
  else false

let rec someAux2 arr1 arr2 i b len =
  if i = len then false
  else if b arr1.!(i) arr2.!(i) [@bs] then
    true
  else someAux2 arr1 arr2 (i + 1) b len


let every2U  a b p =
  everyAux2  a b 0 p (Pervasives.min (length a) (length b))

let every2 a b p = every2U  a b (fun[@bs] a b -> p a b)

let some2U a b p =
  someAux2 a b 0 p (Pervasives.min (length a) (length b))

let some2 a b p = some2U a b (fun [@bs] a b -> p a b)

let eqU a b p =
  let lena = length a in
  let lenb = length b in
  if lena = lenb then
    everyAux2 a b 0 p lena
  else false

let eq a b p = eqU a b (fun [@bs] a b -> p a b )

let rec everyCmpAux2 arr1 arr2 i b len =
  if i = len then 0
  else
    let c = b arr1.!(i) arr2.!(i) [@bs]  in
    if c = 0 then
      everyCmpAux2 arr1 arr2 (i + 1) b len
    else c

let cmpU a b p =
  let lena = length a in
  let lenb = length b in
  if lena > lenb then 1
  else if lena < lenb then -1
  else everyCmpAux2 a b 0 p lena

let cmp a b p = cmpU a b (fun[@bs] a b -> p a b)

let partitionU a f =
  let l = length a in
  let i = ref 0 in
  let j = ref 0 in
  let a1 = makeUninitializedUnsafe l in
  let a2 = makeUninitializedUnsafe l in
  for ii = 0 to l - 1 do
    let v = a.!(ii) in
    if f v [@bs] then (
      a1.!(i.contents) <- v;
      i.contents <- i.contents + 1 
    )
    else (
      a2.!(j.contents) <- v;
      j.contents <- j.contents + 1
    )
  done;
  truncateToLengthUnsafe a1 i.contents;
  truncateToLengthUnsafe a2 j.contents;
  (a1, a2)

let partition a f = partitionU a (fun [@bs] x -> f x)

let unzip a =
  let l = length a in
  let a1 = makeUninitializedUnsafe l in
  let a2 = makeUninitializedUnsafe l in
  for i = 0 to l - 1 do
    let (v1, v2) = a.!(i) in
    a1.!(i) <- v1;
    a2.!(i) <- v2
  done;
  (a1, a2)

let joinWithU a sep toString =
  match length a with
  | 0 -> ""
  | l ->
      let lastIndex = l - 1 in
      let rec aux i res =
        if i = lastIndex then res ^ toString a.!(i) [@bs]
        else aux (i + 1) (res ^ toString a.!(i) [@bs] ^ sep) in
      aux 0 ""
let joinWith a sep toString = joinWithU a sep (fun [@bs] x -> toString x)
