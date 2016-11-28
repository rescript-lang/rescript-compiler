(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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


(** it may return values not representable by [int] *)
external unsafe_ceil : float -> int = "Math.ceil" [@@bs.val]

external unsafe_floor : float -> int = "Math.floor" [@@bs.val]

type t = unit -> float [@bs]

(** [0,1) *)
external random :  t  = "Math.random" [@@bs.val]

let floor f =
  if f > float max_int then max_int
  else if f < float min_int then min_int
  else unsafe_floor f 

let ceil f =
  if f > float max_int then max_int
  else if f < float min_int then min_int
  else unsafe_ceil f 

(** [min,max)
*)
let random_int min max = 
  floor ((random () [@bs]) *. (float (max - min))) + min 

