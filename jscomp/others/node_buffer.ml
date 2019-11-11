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

(** Node Buffer API 

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html> Node.js API Buffer module
    *)

type t = Node.buffer 

(** Allocates a new [Buffer] of [size] bytes. If [fill] is not passed, the 
    [Buffer] will be zero-filled.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_alloc_size_fill_encoding> Node.js API docs
    @since Node.js v5.10.0
  *)
external alloc : 
  int ->
  ?fill:([ `String of string | `Integer of int | `Buffer of t | `UInt8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) -> 
  unit -> t = "alloc"
[@@bs.val] [@@bs.scope "Buffer"]

(** Allocates a new [Buffer] of [size] bytes. 
    
    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_allocunsafe_size> Node.js API docs
    @since Node.js v5.10.0
  *)
external allocUnsafe : int -> t = "allocUnsafe" [@@bs.val] [@@bs.scope "Buffer"]

(** Allocates a new Buffer of [size] bytes. 
    Unpooled buffer allocation, see original docs for additional details.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_allocunsafeslow_size> Node.js API docs
    @since Node.js v5.12.0
  *)
external allocUnsafeSlow : int -> t = "allocUnsafeSlow" [@@bs.val] [@@bs.scope "Buffer"]

(** Returns the actual byte length of a thing. 

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_bytelength_string_encoding> Node.js API docs
    @since Node.js v0.1.90 
  *)
external byteLength : 
  ([
   | `Buffer of t
   | `ArrayBuffer of Js_typed_array2.ArrayBuffer.t
   | `DataView of Js_typed_array2.DataView.t
   | `Int8Array of Js_typed_array2.Int8Array.t
   | `Uint8Array of Js_typed_array2.Uint8Array.t
   | `Uint8ClampedArray of Js_typed_array2.Uint8ClampedArray.t
   | `Int16Array of Js_typed_array2.Int16Array.t
   | `Uint16Array of Js_typed_array2.Uint16Array.t
   | `Int32Array of Js_typed_array2.Int32Array.t
   | `Uint32Array of Js_typed_array2.Uint32Array.t
   | `Float32Array of Js_typed_array2.Float32Array.t
   | `Float64Array of Js_typed_array2.Float64Array.t
   ] [@bs.unwrap]) ->
  int = "byteLength"
[@@bs.val] [@@bs.scope "Buffer"]

(** Returns the actual byte length of a string.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_bytelength_string_encoding> Node.js API docs
    @since Node.js v0.1.90
  *)
external byteLengthOfString :
    string ->
    ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
    unit ->
    int = "byteLength"
[@@bs.val] [@@bs.scope "Buffer"]

(** Compares buf1 to buf2 for the purpose of sorting.  

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_compare_buf1_buf2> Node.js API docs
    @since Node.js v0.11.13
  *)
external compare : 
  ([`Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ([`Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) -> 
  int = "compare"
[@@bs.val] [@@bs.scope "Buffer"]

(** Compares buf with target and returns a number indicating sorting order.
    
    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_compare_target_targetstart_targetend_sourcestart_sourceend> Node.js API docs
    @since Node.js v0.11.13
  *)
external compareRanges :
  t -> t ->
  ?targetStart: int ->
  ?targetEnd: int ->
  ?sourceStart: int ->
  ?sourceEnd: int ->
  unit -> int = "compare"
[@@bs.send]

(** Returns a new [Buffer] which is the result of concatenating all the [Buffer]
    instances in the list together.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_concat_list_totallength> Node.js API docs
    @since Node.js v0.7.11
  *)
external concat : t array -> t = "concat" [@@bs.val] [@@bs.scope "Buffer"]

(** Returns a new [Buffer] which is the result of concatenating all the [Buffer]
    instances in the list together.

    Passing [totalLength] explicitly removes additional loop over list of buffers.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_concat_list_totallength> Node.js API docs
    @since Node.js v0.7.11
  *)
external concatWithLength : t array -> totalLength:int -> t = "concat" [@@bs.val] [@@bs.scope "Buffer"]

(** Allocates a new [Buffer] using from an array of octets.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_from_array> Node.js API docs
    @since Node.js v5.10.0
  *)
external fromArray : int array -> t = "from" [@@bs.val] [@@bs.scope "Buffer"]

(** This creates a view of the [ArrayBuffer] without copying the underlying memory.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_from_arraybuffer_byteoffset_length> Node.js API docs
    @since Node.js v5.10.0
  *)
external fromArrayBuffer : 
  Js_typed_array2.ArrayBuffer.t ->
  ?byteOffset:int ->
  ?length:int ->
  unit ->
  t = "from"
[@@bs.val] [@@bs.scope "Buffer"]

(** Copies the passed [buffer] data onto a new [Buffer] instance.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_from_buffer> Node.js API docs
    @since Node.js v5.10.0
  *)
external fromBuffer : 
  ([`Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  t = "from"
[@@bs.val] [@@bs.scope "Buffer"]

(** Creates a new [Buffer] containing a [string] using [`utf8] encoding.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_from_string_encoding> Node.js API docs
    @since Node.js v5.10.0
  *)
external fromString : string -> t = "from" [@@bs.val] [@@bs.scope "Buffer"]

(** Creates a new [Buffer] from a [string]. The [encoding] parameter specifies
    the character encoding of [string].
    
    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_from_string_encoding> Node.js API docs
    @since Node.js v5.10.0
  *)
external fromStringWithEncoding : string ->
  ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  t = "from"
[@@bs.val] [@@bs.scope "Buffer"]


(** Returns true if value is a [Buffer], [false] otherwise.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_isbuffer_obj> Node.js API docs
    @since Node.js v0.1.101
*)
external isBuffer : 'a -> bool = "isBuffer" [@@bs.val] [@@bs.scope "Buffer"]

(** Returns [true] if [encoding] contains a supported character encoding, or [false] otherwise.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_isencoding_encoding> Node.js API docs
    @since v0.9.1
  *)
external isEncoding : string -> bool = "isEncoding" [@@bs.val] [@@bs.scope "Buffer"]

(** This is the size (in bytes) of pre-allocated internal [Buffer] instances
    used for pooling.
  
    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_property_buffer_poolsize> Node.js API docs
    @since Node.js v0.11.3
  *)
external poolSize : int = "poolSize" [@@bs.val] [@@bs.scope "Buffer"]

(** Unsafe byte reading by its [index].
    Underneath it uses index access syntax in Javascript.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_index> Node.js API docs
  *)
external unsafe_get : t -> int -> int = "" [@@bs.get_index]

(** Unsafe byte writing by its [index].
    Underneath it uses index access syntax in Javascript.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_index> Node.js API docs
  *)
external unsafe_set : t -> index:int -> int -> unit = "" [@@bs.set_index]

(** Returns the underlying ArrayBuffer object.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_buffer> Node.js API docs
  *)
external buffer : t -> Js_typed_array2.ArrayBuffer.t = "buffer" [@@bs.get]

(** Returns [byteOffset] on the underlying [ArrayBuffer] object.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_byteoffset> Node.js API docs
  *)
external byteOffset : t -> int = "byteOffset" [@@bs.get]

(** Copies data from a region of [buf] to a region in [target].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_copy_target_targetstart_sourcestart_sourceend> Node.js API docs
    @since Node.js v0.1.90
  *)
external copy :
  t ->
  target:t ->
  ?targetStart:int ->
  ?sourceStart:int ->
  ?sourceEnd:int ->
  unit -> int = "copy"
[@@bs.send]

(** Returns [true] if both [buf] and [otherBuffer] have exactly the same bytes.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_equals_otherbuffer> Node.js API docs
    @since Node.js v0.11.13
  *)
external equals : t -> t -> bool = "equals" [@@bs.send]

(** Fills [buf] with the specified [value].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_fill_value_offset_end_encoding> Node.js API docs
    @since Node.js v0.5.0
 *)
external fill :
  t ->
  ([ `Integer of int | `Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ?offset:int ->
  ?end_:int ->
  unit -> t = "fill"
[@@bs.send]

(** Fills [buf] with the specified [string].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_fill_value_offset_end_encoding> Node.js API docs
    @since Node.js v0.5.0
 *)
external fillWithString :
  t ->
  string ->
  ?offset:int ->
  ?end_:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> t = "fill"
[@@bs.send]

(** Tests, if [buf] has provided [value], starting looking from [byteOffset].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_includes_value_byteoffset_encoding> Node.js API docs
    @since Node.js v5.3.0
  *)
external includes :
  t -> 
  ([ `Integer of int | `Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ?byteOffset:int ->
  unit -> bool = "includes"
[@@bs.send]

(** Tests, if [buf] has provided [value], starting looking from [byteOffset].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_includes_value_byteoffset_encoding> Node.js API docs
    @since Node.js v5.3.0
  *)
external includesString :
  t -> 
  string ->
  ?byteOffset:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> bool = "includes"
[@@bs.send]

(** Searches for first occurence of [value] in [buf].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_indexof_value_byteoffset_encoding> Node.js API docs
    @since Node.js v1.5.0
  *)
external indexOf :
  t -> 
  ([ `Integer of int | `Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ?byteOffset:int ->
  unit -> int = "indexOf"
[@@bs.send]

(** Searches for first occurence of [string] in [buf].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_indexof_value_byteoffset_encoding> Node.js API docs
    @since Node.js v1.5.0
  *)
external indexOfString :
  t -> 
  string ->
  ?byteOffset:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> int = "indexOf"
[@@bs.send]

(** Searches for last occurence of [value] in [buf].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_lastindexof_value_byteoffset_encoding> Node.js API docs
    @since Node.js v6.0.0
  *)
external lastIndexOf :
  t -> 
  ([ `Integer of int | `Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ?byteOffset:int ->
  unit -> int = "lastIndexOf"
[@@bs.send]

(** Searches for last occurence of [string] in [buf].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_lastindexof_value_byteoffset_encoding> Node.js API docs
    @since Node.js v6.0.0
  *)
external lastIndexOfString :
  t -> 
  string ->
  ?byteOffset:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit -> int = "lastIndexOf"
[@@bs.send]

(** Returns the amount of memory allocated for [buf] in bytes.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_length> Node.js API docs
    @since Node.js v0.1.90
  *)
external length : t -> int = "length" [@@bs.get]


external readDoubleBE : t -> ?offset:int -> unit -> float = "readDoubleBE" [@@bs.send]
external readDoubleLE : t -> ?offset:int -> unit -> float = "readDoubleLE" [@@bs.send]
external readFloatBE : t -> ?offset:int -> unit -> float = "readFloatBE" [@@bs.send]
external readFloatLE : t -> ?offset:int -> unit -> float = "readFloatLE" [@@bs.send]

external readInt8 : t -> ?offset:int -> unit -> int = "readInt8" [@@bs.send]
external readInt16BE : t -> ?offset:int -> unit -> int = "readInt16BE" [@@bs.send]
external readInt16LE : t -> ?offset:int -> unit -> int = "readInt16LE" [@@bs.send]
external readInt32BE : t -> ?offset:int -> unit -> int = "readInt32BE" [@@bs.send]
external readInt32LE : t -> ?offset:int -> unit -> int = "readInt32LE" [@@bs.send]
external readIntBE : t -> offset:int -> byteLength:int -> ?offset:int -> unit = "readIntBE" [@@bs.send]
external readIntLE : t -> offset:int -> byteLength:int -> ?offset:int -> unit = "readIntLE" [@@bs.send]

external readUInt8 : t -> ?offset:int -> unit -> int = "readUInt8" [@@bs.send]
external readUInt16BE : t -> ?offset:int -> unit -> int = "readUInt16BE" [@@bs.send]
external readUInt16LE : t -> ?offset:int -> unit -> int = "readUInt16LE" [@@bs.send]
external readUInt32BE : t -> ?offset:int -> unit -> int = "readUInt32BE" [@@bs.send]
external readUInt32LE : t -> ?offset:int -> unit -> int = "readUInt32LE" [@@bs.send]
external readUIntBE : t -> offset:int -> byteLength:int -> ?offset:int -> unit = "readUIntBE" [@@bs.send]
external readUIntLE : t -> offset:int -> byteLength:int -> ?offset:int -> unit = "readUIntLE" [@@bs.send]

(** Returns a new [Buffer] that references the same memory as the original, 
    but offset and cropped by the [start] and [end_] indices.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_subarray_start_end> Node.js API docs
    @since Node.js v3.0.0
  *)
external subarray : t -> ?start: int -> ?end_: int -> unit -> t = "subarray" [@@bs.send]

(** Returns a new [Buffer] that references the same memory as the original, 
    but offset and cropped by the [start] and [end_] indices.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_slice_start_end> Node.js API docs
    @since Node.js v0.3.0
  *)
external slice : t -> ?start: int -> ?end_:int -> unit -> t = "slice" [@@bs.send]

(** Interprets [buf] as an array of unsigned 16-bit integers and swaps the byte order in-place.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_swap16> Node.js API docs
    @since Node.js v5.10.0
  *)
external swap16 : t -> t = "swap16" [@@bs.send]

(** Interprets [buf] as an array of unsigned 32-bit integers and swaps the byte order in-place.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_swap32> Node.js API docs
    @since Node.js v5.10.0
  *)
external swap32 : t -> t = "swap32" [@@bs.send] 

(** Interprets [buf] as an array of unsigned 64-bit integers and swaps the byte order in-place.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_swap64> Node.js API docs
    @since Node.js v6.3.0
  *)
external swap64 : t -> t = "swap64" [@@bs.send] 

(** Returns a JSON representation of [buf]. 

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_tojson> Node.js API docs
    @since Node.js v0.9.2
  *)
external toJSON : t -> Js.Json.t = "toJSON" [@@bs.send]

(** Serializes [buf] to [string] using [`utf8] encoding.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_tostring_encoding_start_end> Node.js API docs
    @since Node.js v0.1.90
*)
external toString : t -> string = "toString" [@@bs.send]

(** Serializes [buf] to [string] using [encoding].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_tostring_encoding_start_end> Node.js API docs
    @since Node.js v0.1.90
*)
external toStringWithEncoding : 
  t ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  ?start: int ->
  ?end_: int ->
  unit -> 
  string = "toString"
[@@bs.send]

(** Writes [string] to [buf] at [offset] according to the character [encoding].

    @see <https://nodejs.org/api/buffer.html#buffer_buf_write_string_offset_length_encoding> Node.js API docs
    @since Node.js v0.1.90
  *)
external write :
  t ->
  string ->
  ?offset:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit ->
  int = "write" [@@bs.send]

(** Writes [string] to [buf] at [offset] according to the character [encoding].
    The [length] parameter is the number of bytes to write.

    @see <https://nodejs.org/api/buffer.html#buffer_buf_write_string_offset_length_encoding> Node.js API docs
    @since Node.js v0.1.90
  *)
external writeLength :
  t ->
  string ->
  offset:int ->
  ?length:int ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  unit ->
  int = "write" [@@bs.send]


external writeDoubleBE : t -> float -> ?offset:int -> unit -> int = "writeDoubleBE" [@@bs.send]
external writeDoubleLE : t -> float -> ?offset:int -> unit -> int = "writeDoubleLE" [@@bs.send]
external writeFloatBE : t -> float -> ?offset:int -> unit -> int = "writeFloatBE" [@@bs.send]
external writeFloatLE : t -> float -> ?offset:int -> unit -> int = "writeFloatLE" [@@bs.send]

external writeInt8 : t -> int -> ?offset:int -> unit -> int = "writeInt8" [@@bs.send]
external writeInt16BE : t -> int -> ?offset:int -> unit -> int = "writeInt16BE" [@@bs.send]
external writeInt16LE : t -> int -> ?offset:int -> unit -> int = "writeInt16LE" [@@bs.send]
external writeInt32BE : t -> int -> ?offset:int -> unit -> int = "writeInt32BE" [@@bs.send]
external writeInt32LE : t -> int -> ?offset:int -> unit -> int = "writeInt32LE" [@@bs.send]
external writeIntBE : t -> int -> offset:int -> byteLength:int -> int = "writeIntBE" [@@bs.send]
external writeIntLE : t -> int -> offset:int -> byteLength:int -> int = "writeIntLE" [@@bs.send]

external writeUInt8 : t -> int -> ?offset:int -> unit -> int = "writeUInt8" [@@bs.send]
external writeUInt16BE : t -> int -> ?offset:int -> unit -> int = "writeUInt16BE" [@@bs.send]
external writeUInt16LE : t -> int -> ?offset:int -> unit -> int = "writeUInt16LE" [@@bs.send]
external writeUInt32BE : t -> int -> ?offset:int -> unit -> int = "writeUInt32BE" [@@bs.send]
external writeUInt32LE : t -> int -> ?offset:int -> unit -> int = "writeUInt32LE" [@@bs.send]
external writeUIntBE : t -> int -> offset:int -> byteLength:int -> int = "writeUIntBE" [@@bs.send]
external writeUIntLE : t -> int -> offset:int -> byteLength:int -> int = "writeUIntLE" [@@bs.send]

(** Returns the maximum number of bytes that will be returned when [buf.inspect()] is called. 

    @see <https://nodejs.org/api/buffer.html#buffer_buffer_inspect_max_bytes> Node.js API docs
    @since Node.js v0.5.4
  *)
external _INSPECT_MAX_BYTES : int = "INSPECT_MAX_BYTES" [@@bs.module "buffer"]

(** An alias for buffer.constants.MAX_LENGTH. 

    @see <https://nodejs.org/api/buffer.html#buffer_buffer_inspect_max_bytes> Node.js API docs
    @since Node.js v3.0.0
  *)
external kMaxLength : int = "kMaxLength" [@@bs.module "buffer"]

(** Encodes the given [Buffer] or [Uint8Array] instance from one [fromEnc] encoding to [toEnc] encoding.

    @see <https://nodejs.org/api/buffer.html#buffer_buffer_transcode_source_fromenc_toenc> Node.js API docs
    @since Node.js v7.1.0
  *)
external transcode : 
  ([ `Buffer of t | `UInt8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  fromEnc: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  toEnc: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  t = "transcode"
[@@bs.module "buffer"]

(** The largest size allowed for a single [Buffer] instance.

    @see <https://nodejs.org/api/buffer.html#buffer_buffer_constants_max_length> Node.js API docs
    @since Node.js v8.2.0
  *)
external _MAX_LENGTH : int = "MAX_LENGTH" [@@bs.module "buffer"] [@@bs.scope "constants"]

(** The largest length allowed for a single [string] instance.

    @see <https://nodejs.org/api/buffer.html#buffer_buffer_constants_max_string_length> Node.js API docs
    @since Node.js v8.2.0
  *)
external _MAX_STRING_LENGTH : int = "MAX_STRING_LENGTH" [@@bs.module "buffer"] [@@bs.scope "constants"]


