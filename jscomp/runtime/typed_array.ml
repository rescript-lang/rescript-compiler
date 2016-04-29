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






type buffer




module Int32_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@bs.get]
  external get : t -> int -> int32  = "" [@@bs.get_index]
  external set : t -> int -> int32 -> unit = "" [@@bs.set_index]
  external create : int32 array -> t = "Int32Array" [@@bs.new]
  external of_buffer : buffer -> t = "Int32Array" [@@bs.new]
end 

module Float64_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@bs.get]
  external get : t -> int -> float  = "" [@@bs.get_index]
  external set : t -> int -> float -> unit = "" [@@bs.set_index]
  external create : float array -> t = "Float64Array" [@@bs.new]
  external of_buffer : buffer -> t = "Float64Array" [@@bs.new]
end 


(*
 it still return number, [float] in this case
*)
module Float32_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@bs.get]
  external get : t -> int -> float  = "" [@@bs.get_index]
  external set : t -> int -> float -> unit = "" [@@bs.set_index]
  external create : float array -> t = "Float32Array" [@@bs.new]
  external of_buffer : buffer -> t = "Float32Array" [@@bs.new]
end 
