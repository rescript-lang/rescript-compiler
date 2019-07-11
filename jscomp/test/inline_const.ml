let x = true [@@bs.inline]
let f = "hello" [@@bs.inline]
let f1 = {j|a|j} [@@bs.inline]
let f2 = {j|中文|j} [@@bs.inline]

(* Do we need fix let f2 : string = blabla *)

module N : sig
  val f3 : string [@@bs.inline {j|中文|j}]
end = struct
  let f3 = {j|中文|j} [@@bs.inline]
end

module N1 () = struct let f4 = {j|中文|j} [@@bs.inline] end

let h = f
let hh = f ^ f

open N
module H = N1 ()
open H

let a, b, c, d, e = (f, f1, f2, f3, f4)
let f5 = true [@@bs.inline]
let f6 = 1 [@@bs.inline]
