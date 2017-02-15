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

type array_buffer
type 'a array_like (* should be shared with js_array *)

module type Type = sig
  type t
end


module ArrayBuffer = struct
  type t = array_buffer

  external make : int -> t = "ArrayBuffer" [@@bs.new]
  (** takes length. initializes elements to 0 *)

  (* ArrayBuffer.isView: seems pointless with a type system *)
  (* experimental
  external transfer : array_buffer -> t = "ArrayBuffer.transfer" [@@bs.val]
  external transferWithLength : array_buffer -> int -> t = "ArrayBuffer.transfer" [@@bs.val]
  *)

  external byteLength : t -> int = "" [@@bs.get]

  external slice : start:int -> end_:int -> array_buffer = "" [@@bs.send.pipe: t]
  external sliceFrom : int -> array_buffer = "slice" [@@bs.send.pipe: t]
end


module TypedArray (Type: Type) = struct
  type elt = Type.t
  type 'a typed_array

  external unsafe_get : elt typed_array -> int -> elt  = "" [@@bs.get_index]
  external unsafe_set : elt typed_array -> int -> elt -> unit = "" [@@bs.set_index]

  external buffer : elt typed_array -> array_buffer = "" [@@bs.get]
  external byteLength : elt typed_array -> int = "" [@@bs.get]
  external byteOffset : elt typed_array -> int = "" [@@bs.get]

  external setArray : elt array -> unit = "set" [@@bs.send.pipe: elt typed_array]
  external setArrayOffset : elt array -> int -> unit = "set" [@@bs.send.pipe: elt typed_array]
  (* There's also an overload for typed arrays, but don't know how to model that without subtyping *)

  (* Array interface(-ish)
  * ---
  *)
  external length : elt typed_array -> int = "" [@@bs.get]

  (* Mutator functions
  *)
  external copyWithin : to_:int -> elt typed_array = "" [@@bs.send.pipe: elt typed_array]
  external copyWithinFrom : to_:int -> from:int -> elt typed_array = "copyWithin" [@@bs.send.pipe: elt typed_array]
  external copyWithinFromRange : to_:int -> start:int -> end_:int -> elt typed_array = "copyWithin" [@@bs.send.pipe: elt typed_array]

  external fillInPlace : elt -> elt typed_array = "fill" [@@bs.send.pipe: elt typed_array]
  external fillFromInPlace : elt -> from:int -> elt typed_array = "fill" [@@bs.send.pipe: elt typed_array]
  external fillRangeInPlace : elt -> start:int -> end_:int -> elt typed_array = "fill" [@@bs.send.pipe: elt typed_array]

  external reverseInPlace : elt typed_array = "reverse" [@@bs.send.pipe: elt typed_array]

  external sortInPlace : elt typed_array = "sort" [@@bs.send.pipe: elt typed_array]
  external sortInPlaceWith : (elt -> elt -> int [@bs]) -> elt typed_array = "sort" [@@bs.send.pipe: elt typed_array]

  (* Accessor functions
  *)
  external includes : elt -> Js.boolean = "" [@@bs.send.pipe: elt typed_array] (** ES2016 *)

  external indexOf : elt  -> int = "" [@@bs.send.pipe: elt typed_array]
  external indexOfFrom : elt -> from:int -> int = "indexOf" [@@bs.send.pipe: elt typed_array]

  external join : string = "" [@@bs.send.pipe: elt typed_array]
  external joinWith : string -> string = "join" [@@bs.send.pipe: elt typed_array]

  external lastIndexOf : elt -> int = "" [@@bs.send.pipe: elt typed_array]
  external lastIndexOfFrom : elt -> from:int -> int = "lastIndexOf" [@@bs.send.pipe: elt typed_array]

  external slice : start:int -> end_:int -> elt typed_array = "" [@@bs.send.pipe: elt typed_array]
  external copy : elt typed_array = "slice" [@@bs.send.pipe: elt typed_array]
  external sliceFrom : int -> elt typed_array = "slice" [@@bs.send.pipe: elt typed_array]

  external toString : string = "" [@@bs.send.pipe: elt typed_array]
  external toLocaleString : string = "" [@@bs.send.pipe: elt typed_array]


  (* Iteration functions
  *)
  (* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@@bs.send.pipe: elt typed_array]
  *)

  external every : (elt  -> Js.boolean [@bs]) -> Js.boolean = "" [@@bs.send.pipe: elt typed_array]
  external everyi : (elt -> int -> Js.boolean [@bs]) -> Js.boolean = "every" [@@bs.send.pipe: elt typed_array]

  (** should we use [bool] or [boolan] seems they are intechangeable here *)
  external filter : (elt -> bool [@bs]) -> elt typed_array = "" [@@bs.send.pipe: elt typed_array]
  external filteri : (elt -> int  -> Js.boolean[@bs]) -> elt typed_array = "filter" [@@bs.send.pipe: elt typed_array]

  external find : (elt -> bool [@bs]) -> elt Js.undefined = "" [@@bs.send.pipe: elt typed_array]
  external findi : (elt -> int -> bool [@bs]) -> elt Js.undefined  = "find" [@@bs.send.pipe: elt typed_array]

  external findIndex : (elt -> bool [@bs]) -> int = "" [@@bs.send.pipe: elt typed_array]
  external findIndexi : (elt -> int -> bool [@bs]) -> int = "findIndex" [@@bs.send.pipe: elt typed_array]

  external forEach : (elt -> unit [@bs]) -> unit = "" [@@bs.send.pipe: elt typed_array]
  external forEachi : (elt -> int -> unit [@bs]) -> unit  = "forEach" [@@bs.send.pipe: elt typed_array]

  (* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@@bs.send.pipe: elt typed_array]
  *)

  external map : (elt  -> 'b [@bs]) -> 'b typed_array = "" [@@bs.send.pipe: elt typed_array]
  external mapi : (elt -> int ->  'b [@bs]) -> 'b typed_array = "map" [@@bs.send.pipe: elt typed_array]

  external reduce :  ('b -> elt  -> 'b [@bs]) -> 'b -> 'b = "" [@@bs.send.pipe: elt typed_array]
  external reducei : ('b -> elt -> int -> 'b [@bs]) -> 'b -> 'b = "reduce" [@@bs.send.pipe: elt typed_array]

  external reduceRight :  ('b -> elt  -> 'b [@bs]) -> 'b -> 'b = "" [@@bs.send.pipe: elt typed_array]
  external reduceRighti : ('b -> elt -> int -> 'b [@bs]) -> 'b -> 'b = "reduceRight" [@@bs.send.pipe: elt typed_array]

  external some : (elt  -> Js.boolean [@bs]) -> Js.boolean = "" [@@bs.send.pipe: elt typed_array]
  external somei : (elt  -> int -> Js.boolean [@bs]) -> Js.boolean = "some" [@@bs.send.pipe: elt typed_array]

  (* commented out until bs has a plan for iterators
  external values : elt array_iter = "" [@@bs.send.pipe: elt typed_array]
  *)
end


module Int8Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Int8Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Int8Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Int8Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Int8Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Int8Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Int8Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Int8Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

end


module Uint8Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint8Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Uint8Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Uint8Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Uint8Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Uint8Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Uint8Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Uint8Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Uint8ClampedArray = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint8ClampedArray.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Uint8ClampedArray" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Uint8ClampedArray" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Uint8ClampedArray" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Uint8ClampedArray" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Uint8ClampedArray" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Uint8ClampedArray.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Int16Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Int16Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Int16Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Int16Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Int16Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Int16Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Int16Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Int16Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Uint16Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint16Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Uint16Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Uint16Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Uint16Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Uint16Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Uint16Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Uint16Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Int32Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int32 end)

  external _BYTES_PER_ELEMENT: int = "Int32Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Int32Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Int32Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Int32Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Int32Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Int32Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Int32Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

  external create : int32 array -> elt typed_array = "Int32Array" [@@bs.new]
  [@@ocaml.deprecated "use `make` instead"]
  external of_buffer : array_buffer -> elt typed_array = "Int32Array" [@@bs.new]
  [@@ocaml.deprecated "use `fromBuffer` instead"]
end
module Int32_array = Int32Array
[@ocaml.deprecated "use `Int32Array` instead"]


module Uint32Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint32Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Uint32Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Uint32Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Uint32Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Uint32Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Uint32Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Uint32Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


(*
 it still return number, [float] in this case
*)
module Float32Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = float end)

  external _BYTES_PER_ELEMENT: int = "Float32Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Float32Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Float32Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Float32Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Float32Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Float32Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Float32Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

  external create : float array -> elt typed_array = "Float32Array" [@@bs.new]
  [@@ocaml.deprecated "use `make` instead"]
  external of_buffer : array_buffer -> elt typed_array = "Float32Array" [@@bs.new]
  [@@ocaml.deprecated "use `fromBuffer` instead"]
end
module Float32_array = Float32Array
[@ocaml.deprecated "use `Float32Array` instead"]


module Float64Array = struct
  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = float end)

  external _BYTES_PER_ELEMENT: int = "Float64Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> elt typed_array = "Float64Array" [@@bs.new]
  external fromBuffer : array_buffer -> elt typed_array = "Float64Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> elt typed_array = "Float64Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> elt typed_array = "Float64Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> elt typed_array = "Float64Array" [@@bs.new]
  external from : elt array_like -> elt typed_array = "Float64Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

  external create : float array -> elt typed_array = "Float64Array" [@@bs.new]
  [@@ocaml.deprecated "use `make` instead"]
  external of_buffer : array_buffer -> elt typed_array = "Float64Array" [@@bs.new]
  [@@ocaml.deprecated "use `fromBuffer` instead"]
end
module Float64_array = Float64Array
[@ocaml.deprecated "use `Float64Array` instead"]
