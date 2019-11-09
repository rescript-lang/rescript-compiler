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

(** Node Buffer API *)

type t = Node.buffer 

external isBuffer : 'a -> bool = "isBuffer" [@@bs.val] [@@bs.scope "Buffer"]

external fromString : string -> t = "from" [@@bs.val] [@@bs.scope "Buffer"]

external fromArray : int array -> t = "from" [@@bs.val] [@@bs.scope "Buffer"]

external fromBuffer : t -> t = "from" [@@bs.val] [@@bs.scope "Buffer"]

external fromArrayBuffer : Js_typed_array2.array_buffer -> ?offset:int -> ?length:int -> unit -> t = "from" [@@bs.val] [@@bs.scope "Buffer"]

external fromStringWithEncoding :
  string ->
  ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string])
  -> t = "from"
 [@@bs.val] [@@bs.scope "Buffer"]

external alloc : 
  int ->
  ?fill:([ `String of string | `Integer of int | `Buffer of t ] [@bs.unwrap]) ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) -> 
  unit -> t = "alloc"
[@@bs.val] [@@bs.scope "Buffer"]

external allocUnsafe : int -> t = "allocUnsafe"
[@@bs.val] [@@bs.scope "Buffer"]

external allocUnsafeSlow : int -> t = "allocUnsafeSlow"
[@@bs.val] [@@bs.scope "Buffer"]

external byteLength : t -> int = "byteLength"
[@@bs.get]

external byteOffset : t -> int = "byteOffset"
[@@bs.get]

external byteLengthWithEncoding : 
  ([ `String of string | `Buffer of t ] [@bs.unwrap]) ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit ->
  int = "byteLength"
[@@bs.val] [@@bs.scope "Buffer"]

external compare : t -> t -> int = "compare"
[@@bs.val] [@@bs.scope "Buffer"]

external toString : t -> string = "toString" [@@bs.send]

external toStringWithEncoding : 
  t ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  ?start: int ->
  ?end_: int ->
  unit -> string = "toString"
[@@bs.send]

external concat : t array -> t = "concat"
[@@bs.val] [@@bs.scope "Buffer"]

external concatWithTotalLength : t array -> int -> t = "concat"
[@@bs.val] [@@bs.scope "Buffer"]

external isEncoding : string -> bool = "isEncoding"
[@@bs.val] [@@bs.scope "Buffer"]

(* TODO: How to support mutation of this value? *)
external poolSize : int = "poolSize"
[@@bs.val] [@@bs.scope "Buffer"]

external unsafe_get : t -> int -> int = ""
[@@bs.get_index]

external unsafe_set : t -> int -> int -> unit = ""
[@@bs.set_index]

external buffer : t -> Js_typed_array2.array_buffer = "buffer"
[@@bs.get]

external compare :
  t -> t ->
  ?targetStart: int ->
  ?targetEnd: int ->
  ?sourceStart: int ->
  ?sourceEnd: int ->
  unit -> int = "compare"
[@@bs.send]

external copy :
  t -> t ->
  ?targetStart:int ->
  ?sourceStart:int ->
  ?sourceEnd:int ->
  unit -> int = "copy"
[@@bs.send]

external equals : t -> t -> bool = "equals"
[@@bs.send]

external fill :
  t ->
  ([ `String of string | `Integer of int | `Buffer of t ] [@bs.unwrap]) ->
  ?offset:int ->
  ?end_:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> t = "fill"
[@@bs.send]

external includes :
  t -> 
  ?byteOffset:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> bool = "includes"
[@@bs.send]

external indexOf :
  t -> 
  ([ `String of string | `Integer of int | `Buffer of t ] [@bs.unwrap]) ->
  ?byteOffset:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> int = "indexOf"
[@@bs.send]

external lastIndexOf :
  t -> 
  ([ `String of string | `Integer of int | `Buffer of t ] [@bs.unwrap]) ->
  ?byteOffset:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> int = "lastIndexOf"
[@@bs.send]

external length : t -> int = "length" 
[@@bs.get]

external readDoubleBE : t -> int -> float = "readDoubleBE" [@@bs.send]
external readDoubleLE : t -> int -> float = "readDoubleLE" [@@bs.send]
external readFloatBE : t -> int -> float = "readFloatBE" [@@bs.send]
external readFloatLE : t -> int -> float = "readFloatLE" [@@bs.send]

external readInt8 : t -> int -> int = "readInt8" [@@bs.send]
external readInt16BE : t -> int -> int = "readInt16BE" [@@bs.send]
external readInt16LE : t -> int -> int = "readInt16LE" [@@bs.send]
external readInt32BE : t -> int -> int = "readInt32BE" [@@bs.send]
external readInt32LE : t -> int -> int = "readInt32LE" [@@bs.send]
external readIntBE : t -> offset:int -> byteLength:int -> int = "readIntBE" [@@bs.send]
external readIntLE : t -> offset:int -> byteLength:int -> int = "readIntLE" [@@bs.send]

external readUInt8 : t -> int -> int = "readUInt8" [@@bs.send]
external readUInt16BE : t -> int -> int = "readUInt16BE" [@@bs.send]
external readUInt16LE : t -> int -> int = "readUInt16LE" [@@bs.send]
external readUInt32BE : t -> int -> int = "readUInt32BE" [@@bs.send]
external readUInt32LE : t -> int -> int = "readUInt32LE" [@@bs.send]
external readUIntBE : t -> offset:int -> byteLength:int -> int = "readUIntBE" [@@bs.send]
external readUIntLE : t -> offset:int -> byteLength:int -> int = "readUIntLE" [@@bs.send]

external subarray : t -> ?start: int -> ?end_: int -> unit -> t = "subarray" [@@bs.send]
external slice : t -> ?start: int -> ?end_:int -> unit -> t = "slice" [@@bs.send]

external swap16 : t -> t = "swap16" [@@bs.send]
external swap32 : t -> t = "swap32" [@@bs.send] 
external swap64 : t -> t = "swap64" [@@bs.send] 

external toJSON : t -> Js.Json.t = "toJSON" [@@bs.send]

external write :
  t ->
  string ->
  ?offset:int ->
  ?length:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> int = "write" [@@bs.send]

external writeDoubleBE : t -> float -> offset:int -> int = "writeDoubleBE" [@@bs.send]
external writeDoubleLE : t -> float -> offset:int -> int = "writeDoubleLE" [@@bs.send]
external writeFloatBE : t -> float -> offset:int -> int = "writeFloatBE" [@@bs.send]
external writeFloatLE : t -> float -> offset:int -> int = "writeFloatLE" [@@bs.send]

external writeInt8 : t -> int -> offset:int -> int = "writeInt8" [@@bs.send]
external writeInt16BE : t -> int -> offset:int -> int = "writeInt16BE" [@@bs.send]
external writeInt16LE : t -> int -> offset:int -> int = "writeInt16LE" [@@bs.send]
external writeInt32BE : t -> int -> offset:int -> int = "writeInt32BE" [@@bs.send]
external writeInt32LE : t -> int -> offset:int -> int = "writeInt32LE" [@@bs.send]
external writeIntBE : t -> int -> offset:int -> byteLength:int -> int = "writeIntBE" [@@bs.send]
external writeIntLE : t -> int -> offset:int -> byteLength:int -> int = "writeIntLE" [@@bs.send]

external writeUInt8 : t -> int -> offset:int -> int = "writeUInt8" [@@bs.send]
external writeUInt16BE : t -> int -> offset:int -> int = "writeUInt16BE" [@@bs.send]
external writeUInt16LE : t -> int -> offset:int -> int = "writeUInt16LE" [@@bs.send]
external writeUInt32BE : t -> int -> offset:int -> int = "writeUInt32BE" [@@bs.send]
external writeUInt32LE : t -> int -> offset:int -> int = "writeUInt32LE" [@@bs.send]
external writeUIntBE : t -> int -> offset:int -> byteLength:int -> int = "writeUIntBE" [@@bs.send]
external writeUIntLE : t -> int -> offset:int -> byteLength:int -> int = "writeUIntLE" [@@bs.send]

external _INSPECT_MAX_BYTES : t -> int = "INSPECT_MAX_BYTES" [@@bs.get]
external kMaxLength : t -> int = "kMaxLength" [@@bs.get]

external transcode : 
  t ->
  fromEnc: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  toEnc: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  t = "transcode"
[@@bs.val] [@@bs.scope "Buffer"]

external _MAX_LENGTH : int = "MAX_LENGTH" [@@bs.val] [@@bs.scope "Buffer.constants"]
external _MAX_STRING_LENGTH : int = "MAX_STRING_LENGTH" [@@bs.val] [@@bs.scope "Buffer.constants"]


