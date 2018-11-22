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

(** JavaScript Typed Array API

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray> MDN
*)

type array_buffer
type 'a array_like (* should be shared with js_array *)

module type Type = sig
  type t
end


module ArrayBuffer = struct
  (** The underlying buffer that the typed arrays provide views of

    @see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer> MDN
  *)

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
module type S =  sig
  (** Implements functionality common to all the typed arrays *)

  type elt 
  type 'a typed_array
  type t = elt typed_array

  external unsafe_get : t -> int -> elt  = "" [@@bs.get_index]
  external unsafe_set : t -> int -> elt -> unit = "" [@@bs.set_index]

  external buffer : t -> array_buffer = "" [@@bs.get]
  external byteLength : t -> int = "" [@@bs.get]
  external byteOffset : t -> int = "" [@@bs.get]

  external setArray : elt array -> unit = "set" [@@bs.send.pipe: t]
  external setArrayOffset : elt array -> int -> unit = "set" [@@bs.send.pipe: t]
  (* There's also an overload for typed arrays, but don't know how to model that without subtyping *)

  (* Array interface(-ish)
  * ---
  *)
  external length : t -> int = "" [@@bs.get]

  (* Mutator functions
  *)
  external copyWithin : to_:int -> t = "" [@@bs.send.pipe: t]
  external copyWithinFrom : to_:int -> from:int -> t = "copyWithin" [@@bs.send.pipe: t]
  external copyWithinFromRange : to_:int -> start:int -> end_:int -> t = "copyWithin" [@@bs.send.pipe: t]

  external fillInPlace : elt -> t = "fill" [@@bs.send.pipe: t]
  external fillFromInPlace : elt -> from:int -> t = "fill" [@@bs.send.pipe: t]
  external fillRangeInPlace : elt -> start:int -> end_:int -> t = "fill" [@@bs.send.pipe: t]

  external reverseInPlace : t = "reverse" [@@bs.send.pipe: t]

  external sortInPlace : t = "sort" [@@bs.send.pipe: t]
  external sortInPlaceWith : (elt -> elt -> int [@bs]) -> t = "sort" [@@bs.send.pipe: t]

  (* Accessor functions
  *)
  external includes : elt -> bool = "" [@@bs.send.pipe: t] (** ES2016 *)

  external indexOf : elt  -> int = "" [@@bs.send.pipe: t]
  external indexOfFrom : elt -> from:int -> int = "indexOf" [@@bs.send.pipe: t]

  external join : string = "" [@@bs.send.pipe: t]
  external joinWith : string -> string = "join" [@@bs.send.pipe: t]

  external lastIndexOf : elt -> int = "" [@@bs.send.pipe: t]
  external lastIndexOfFrom : elt -> from:int -> int = "lastIndexOf" [@@bs.send.pipe: t]

  external slice : start:int -> end_:int -> t = "" [@@bs.send.pipe: t]
  external copy : t = "slice" [@@bs.send.pipe: t]
  external sliceFrom : int -> t = "slice" [@@bs.send.pipe: t]

  external subarray : start:int -> end_:int -> t = "" [@@bs.send.pipe: t]
  external subarrayFrom : int -> t = "subarray" [@@bs.send.pipe: t]

  external toString : string = "" [@@bs.send.pipe: t]
  external toLocaleString : string = "" [@@bs.send.pipe: t]


  (* Iteration functions
  *)
  (* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@@bs.send.pipe: t]
  *)

  external every : (elt  -> bool [@bs]) -> bool = "" [@@bs.send.pipe: t]
  external everyi : (elt -> int -> bool [@bs]) -> bool = "every" [@@bs.send.pipe: t]

  (** should we use [bool] or [boolan] seems they are intechangeable here *)
  external filter : (elt -> bool [@bs]) -> t = "" [@@bs.send.pipe: t]
  external filteri : (elt -> int  -> bool [@bs]) -> t = "filter" [@@bs.send.pipe: t]

  external find : (elt -> bool [@bs]) -> elt Js.undefined = "" [@@bs.send.pipe: t]
  external findi : (elt -> int -> bool [@bs]) -> elt Js.undefined  = "find" [@@bs.send.pipe: t]

  external findIndex : (elt -> bool [@bs]) -> int = "" [@@bs.send.pipe: t]
  external findIndexi : (elt -> int -> bool [@bs]) -> int = "findIndex" [@@bs.send.pipe: t]

  external forEach : (elt -> unit [@bs]) -> unit = "" [@@bs.send.pipe: t]
  external forEachi : (elt -> int -> unit [@bs]) -> unit  = "forEach" [@@bs.send.pipe: t]

  (* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@@bs.send.pipe: t]
  *)

  external map : (elt  -> 'b [@bs]) -> 'b typed_array = "" [@@bs.send.pipe: t]
  external mapi : (elt -> int ->  'b [@bs]) -> 'b typed_array = "map" [@@bs.send.pipe: t]

  external reduce :  ('b -> elt  -> 'b [@bs]) -> 'b -> 'b = "" [@@bs.send.pipe: t]
  external reducei : ('b -> elt -> int -> 'b [@bs]) -> 'b -> 'b = "reduce" [@@bs.send.pipe: t]

  external reduceRight :  ('b -> elt  -> 'b [@bs]) -> 'b -> 'b = "" [@@bs.send.pipe: t]
  external reduceRighti : ('b -> elt -> int -> 'b [@bs]) -> 'b -> 'b = "reduceRight" [@@bs.send.pipe: t]

  external some : (elt  -> bool [@bs]) -> bool = "" [@@bs.send.pipe: t]
  external somei : (elt  -> int -> bool [@bs]) -> bool = "some" [@@bs.send.pipe: t]

  (* commented out until bs has a plan for iterators
  external values : elt array_iter = "" [@@bs.send.pipe: t]
  *)
end


(**/**)
module TypedArray (Type: Type) : S with type elt = Type.t  = struct
  (** Implements functionality common to all the typed arrays *)

  type elt = Type.t
  type 'a typed_array
  type t = elt typed_array

  external unsafe_get : t -> int -> elt  = "" [@@bs.get_index]
  external unsafe_set : t -> int -> elt -> unit = "" [@@bs.set_index]

  external buffer : t -> array_buffer = "" [@@bs.get]
  external byteLength : t -> int = "" [@@bs.get]
  external byteOffset : t -> int = "" [@@bs.get]

  external setArray : elt array -> unit = "set" [@@bs.send.pipe: t]
  external setArrayOffset : elt array -> int -> unit = "set" [@@bs.send.pipe: t]
  (* There's also an overload for typed arrays, but don't know how to model that without subtyping *)

  (* Array interface(-ish)
  * ---
  *)
  external length : t -> int = "" [@@bs.get]

  (* Mutator functions
  *)
  external copyWithin : to_:int -> t = "" [@@bs.send.pipe: t]
  external copyWithinFrom : to_:int -> from:int -> t = "copyWithin" [@@bs.send.pipe: t]
  external copyWithinFromRange : to_:int -> start:int -> end_:int -> t = "copyWithin" [@@bs.send.pipe: t]

  external fillInPlace : elt -> t = "fill" [@@bs.send.pipe: t]
  external fillFromInPlace : elt -> from:int -> t = "fill" [@@bs.send.pipe: t]
  external fillRangeInPlace : elt -> start:int -> end_:int -> t = "fill" [@@bs.send.pipe: t]

  external reverseInPlace : t = "reverse" [@@bs.send.pipe: t]

  external sortInPlace : t = "sort" [@@bs.send.pipe: t]
  external sortInPlaceWith : (elt -> elt -> int [@bs]) -> t = "sort" [@@bs.send.pipe: t]

  (* Accessor functions
  *)
  external includes : elt -> bool = "" [@@bs.send.pipe: t] (** ES2016 *)

  external indexOf : elt  -> int = "" [@@bs.send.pipe: t]
  external indexOfFrom : elt -> from:int -> int = "indexOf" [@@bs.send.pipe: t]

  external join : string = "" [@@bs.send.pipe: t]
  external joinWith : string -> string = "join" [@@bs.send.pipe: t]

  external lastIndexOf : elt -> int = "" [@@bs.send.pipe: t]
  external lastIndexOfFrom : elt -> from:int -> int = "lastIndexOf" [@@bs.send.pipe: t]

  external slice : start:int -> end_:int -> t = "" [@@bs.send.pipe: t]
  (** [start] is inclusive, [end_] exclusive *)
  external copy : t = "slice" [@@bs.send.pipe: t]
  external sliceFrom : int -> t = "slice" [@@bs.send.pipe: t]

  external subarray : start:int -> end_:int -> t = "" [@@bs.send.pipe: t]
  (** [start] is inclusive, [end_] exclusive *)
  external subarrayFrom : int -> t = "subarray" [@@bs.send.pipe: t]

  external toString : string = "" [@@bs.send.pipe: t]
  external toLocaleString : string = "" [@@bs.send.pipe: t]


  (* Iteration functions
  *)
  (* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@@bs.send.pipe: t]
  *)

  external every : (elt  -> bool [@bs]) -> bool = "" [@@bs.send.pipe: t]
  external everyi : (elt -> int -> bool [@bs]) -> bool = "every" [@@bs.send.pipe: t]

  (** should we use [bool] or [boolan] seems they are intechangeable here *)
  external filter : (elt -> bool [@bs]) -> t = "" [@@bs.send.pipe: t]
  external filteri : (elt -> int  -> bool [@bs]) -> t = "filter" [@@bs.send.pipe: t]

  external find : (elt -> bool [@bs]) -> elt Js.undefined = "" [@@bs.send.pipe: t]
  external findi : (elt -> int -> bool [@bs]) -> elt Js.undefined  = "find" [@@bs.send.pipe: t]

  external findIndex : (elt -> bool [@bs]) -> int = "" [@@bs.send.pipe: t]
  external findIndexi : (elt -> int -> bool [@bs]) -> int = "findIndex" [@@bs.send.pipe: t]

  external forEach : (elt -> unit [@bs]) -> unit = "" [@@bs.send.pipe: t]
  external forEachi : (elt -> int -> unit [@bs]) -> unit  = "forEach" [@@bs.send.pipe: t]

  (* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@@bs.send.pipe: t]
  *)

  external map : (elt  -> 'b [@bs]) -> 'b typed_array = "" [@@bs.send.pipe: t]
  external mapi : (elt -> int ->  'b [@bs]) -> 'b typed_array = "map" [@@bs.send.pipe: t]

  external reduce :  ('b -> elt  -> 'b [@bs]) -> 'b -> 'b = "" [@@bs.send.pipe: t]
  external reducei : ('b -> elt -> int -> 'b [@bs]) -> 'b -> 'b = "reduce" [@@bs.send.pipe: t]

  external reduceRight :  ('b -> elt  -> 'b [@bs]) -> 'b -> 'b = "" [@@bs.send.pipe: t]
  external reduceRighti : ('b -> elt -> int -> 'b [@bs]) -> 'b -> 'b = "reduceRight" [@@bs.send.pipe: t]

  external some : (elt  -> bool [@bs]) -> bool = "" [@@bs.send.pipe: t]
  external somei : (elt  -> int -> bool [@bs]) -> bool = "some" [@@bs.send.pipe: t]

  (* commented out until bs has a plan for iterators
  external values : elt array_iter = "" [@@bs.send.pipe: t]
  *)
end
(**/**)

module Int8Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  (** see signatures in {!S} *)    
  include TypedArray(struct type t = int end)


  external _BYTES_PER_ELEMENT: int = "Int8Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Int8Array" [@@bs.new]

  (** can throw *)
  external fromBuffer : array_buffer -> t = "Int8Array" [@@bs.new]


  external fromBufferOffset : array_buffer -> int -> t = "Int8Array" [@@bs.new]

  (** @raise Js.Exn.Error raise Js exception
      @param offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Int8Array" [@@bs.new]

  (** @raise Js.Exn.Error raises Js exception
      @param offset is in bytes, length in elements *)
  external fromLength : int -> t = "Int8Array" [@@bs.new]

  external from : elt array_like -> t = "Int8Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

end


module Uint8Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint8Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Uint8Array" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Uint8Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Uint8Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Uint8Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Uint8Array" [@@bs.new]
  external from : elt array_like -> t = "Uint8Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Uint8ClampedArray = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint8ClampedArray.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Uint8ClampedArray" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Uint8ClampedArray" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Uint8ClampedArray" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Uint8ClampedArray" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Uint8ClampedArray" [@@bs.new]
  external from : elt array_like -> t = "Uint8ClampedArray.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Int16Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Int16Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Int16Array" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Int16Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Int16Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Int16Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Int16Array" [@@bs.new]
  external from : elt array_like -> t = "Int16Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Uint16Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint16Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Uint16Array" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Uint16Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Uint16Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Uint16Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Uint16Array" [@@bs.new]
  external from : elt array_like -> t = "Uint16Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


module Int32Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int32 end)

  external _BYTES_PER_ELEMENT: int = "Int32Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Int32Array" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Int32Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Int32Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Int32Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Int32Array" [@@bs.new]
  external from : elt array_like -> t = "Int32Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

  external create : int32 array -> t = "Int32Array" [@@bs.new]
  [@@ocaml.deprecated "use `make` instead"]
  external of_buffer : array_buffer -> t = "Int32Array" [@@bs.new]
  [@@ocaml.deprecated "use `fromBuffer` instead"]
end
module Int32_array = Int32Array
[@ocaml.deprecated "use `Int32Array` instead"]


module Uint32Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = int end)

  external _BYTES_PER_ELEMENT: int = "Uint32Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Uint32Array" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Uint32Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Uint32Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Uint32Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Uint32Array" [@@bs.new]
  external from : elt array_like -> t = "Uint32Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)
end


(*
 it still return number, [float] in this case
*)
module Float32Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = float end)

  external _BYTES_PER_ELEMENT: int = "Float32Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Float32Array" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Float32Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Float32Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Float32Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Float32Array" [@@bs.new]
  external from : elt array_like -> t = "Float32Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

  external create : float array -> t = "Float32Array" [@@bs.new]
  [@@ocaml.deprecated "use `make` instead"]
  external of_buffer : array_buffer -> t = "Float32Array" [@@bs.new]
  [@@ocaml.deprecated "use `fromBuffer` instead"]
end
module Float32_array = Float32Array
[@ocaml.deprecated "use `Float32Array` instead"]


module Float64Array = struct
  (** *)

  (* defines elt, typed_array and a bunch of common functions *)
  include TypedArray(struct type t = float end)

  external _BYTES_PER_ELEMENT: int = "Float64Array.BYTES_PER_ELEMENT" [@@bs.val]

  external make : elt array -> t = "Float64Array" [@@bs.new]
  external fromBuffer : array_buffer -> t = "Float64Array" [@@bs.new]
  (** can throw *)
  external fromBufferOffset : array_buffer -> int -> t = "Float64Array" [@@bs.new]
  (** can throw, offset is in bytes *)
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "Float64Array" [@@bs.new]
  (** can throw, offset is in bytes, length in elements *)
  external fromLength : int -> t = "Float64Array" [@@bs.new]
  external from : elt array_like -> t = "Float64Array.from" [@@bs.val]
  (* *Array.of is redundant, use make *)

  external create : float array -> t = "Float64Array" [@@bs.new]
  [@@ocaml.deprecated "use `make` instead"]
  external of_buffer : array_buffer -> t = "Float64Array" [@@bs.new]
  [@@ocaml.deprecated "use `fromBuffer` instead"]
end
module Float64_array = Float64Array
[@ocaml.deprecated "use `Float64Array` instead"]

module DataView = struct
  (** The DataView view provides a low-level interface for reading and writing
      multiple number types in an ArrayBuffer irrespective of the platform's endianness.

    @see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView> MDN
  *)

  type t

  external make : array_buffer -> t = "DataView" [@@bs.new]
  external fromBuffer : array_buffer -> t = "DataView" [@@bs.new]
  external fromBufferOffset : array_buffer -> int -> t = "DataView" [@@bs.new]
  external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "DataView" [@@bs.new]

  external buffer : t -> array_buffer = "" [@@bs.get]
  external byteLength : t -> int = "" [@@bs.get]
  external byteOffset : t -> int = "" [@@bs.get]

  external getInt8 : t -> int -> int = "" [@@bs.send]
  external getUint8 : t -> int -> int = "" [@@bs.send]

  external getInt16: t -> int -> int = "" [@@bs.send]
  external getInt16LittleEndian : t -> int -> (_ [@bs.as 1]) -> int =
    "getInt16" [@@bs.send]

  external getUint16: t -> int -> int = "" [@@bs.send]
  external getUint16LittleEndian : t -> int -> (_ [@bs.as 1]) -> int =
    "getUint16" [@@bs.send]

  external getInt32: t -> int -> int = "" [@@bs.send]
  external getInt32LittleEndian : t -> int -> (_ [@bs.as 1]) -> int =
    "getInt32" [@@bs.send]

  external getUint32: t -> int -> int = "" [@@bs.send]
  external getUint32LittleEndian : t -> int -> (_ [@bs.as 1]) -> int =
    "getUint32" [@@bs.send]

  external getFloat32: t -> int -> float = "" [@@bs.send]
  external getFloat32LittleEndian : t -> int -> (_ [@bs.as 1]) -> float =
    "getFloat32" [@@bs.send]

  external getFloat64: t -> int -> float = "" [@@bs.send]
  external getFloat64LittleEndian : t -> int -> (_ [@bs.as 1]) -> float =
    "getFloat64" [@@bs.send]

  external setInt8 : t -> int -> int -> unit = "" [@@bs.send]
  external setUint8 : t -> int -> int -> unit = "" [@@bs.send]

  external setInt16: t -> int -> int -> unit = "" [@@bs.send]
  external setInt16LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit =
    "setInt16" [@@bs.send]

  external setUint16: t -> int -> int -> unit = "" [@@bs.send]
  external setUint16LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit =
    "setUint16" [@@bs.send]

  external setInt32: t -> int -> int -> unit = "" [@@bs.send]
  external setInt32LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit =
    "setInt32" [@@bs.send]

  external setUint32: t -> int -> int -> unit = "" [@@bs.send]
  external setUint32LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit =
    "setUint32" [@@bs.send]

  external setFloat32: t -> int -> float -> unit = "" [@@bs.send]
  external setFloat32LittleEndian : t -> int -> float -> (_ [@bs.as 1]) -> unit =
    "setFloat32" [@@bs.send]

  external setFloat64: t -> int -> float -> unit = "" [@@bs.send]
  external setFloat64LittleEndian : t -> int -> float -> (_ [@bs.as 1]) -> unit =
    "setFloat64" [@@bs.send]

end
