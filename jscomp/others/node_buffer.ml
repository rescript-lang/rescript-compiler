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

(** Allocates a new [Buffer] of [size] bytes. If [fill] is not passed, the 
    [Buffer] will be zero-filled.

    @param size The desired length of the new [Buffer].
    @param fill A value to pre-fill the new [Buffer] with. Default: [0].
    @param encoding If [fill] is a string, this is its encoding.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_alloc_size_fill_encoding> Node.js API docs
    @since Node.js v5.10.0
  *)
external alloc : 
  int ->
  ?fill:([ `String of string | `Integer of int | `Buffer of t | `UInt8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) -> 
  unit -> t = "alloc"
[@@bs.val] [@@bs.scope "Buffer"]

(** Allocates a new [Buffer] of size bytes. If size is larger than
    buffer.constants.MAX_LENGTH or smaller than 0, ERR_INVALID_OPT_VALUE is thrown.
    A zero-length Buffer is created if size is 0.
    The underlying memory for Buffer instances created in this way is not initialized.
    The contents of the newly created Buffer are unknown and may contain sensitive data.
    Use [Buffer.alloc] instead to initialize [Buffer] instances with zeroes.

    @param size The desired length of the new [Buffer].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_allocunsafe_size> Node.js API docs
    @since Node.js v5.10.0
  *)
external allocUnsafe : int -> t = "allocUnsafe" [@@bs.val] [@@bs.scope "Buffer"]

(** Allocates a new Buffer of size bytes. If size is larger than
    buffer.constants.MAX_LENGTH or smaller than 0, ERR_INVALID_OPT_VALUE is thrown.
    A zero-length Buffer is created if size is 0.
    The underlying memory for Buffer instances created in this way is not initialized.
    The contents of the newly created Buffer are unknown and may contain sensitive data.
    Use [Buffer.alloc] to initialize such [Buffer] instances with zeroes.
    When using [Buffer.allocUnsafe] to allocate new [Buffer] instances, allocations
    under 4KB are sliced from a single pre-allocated [Buffer]. This allows applications
    to avoid the garbage collection overhead of creating many individually allocated
    [Buffer] instances. This approach improves both performance and memory usage by 
    eliminating the need to track and clean up as many persistent objects.
    However, in the case where a developer may need to retain a small chunk of memory
    from a pool for an indeterminate amount of time, it may be appropriate to create
    an un-pooled [Buffer] instance using Buffer.allocUnsafeSlow() and then copying out
    the relevant bits.

    @param size The desired length of the new Buffer.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_allocunsafeslow_size> Node.js API docs
    @since Node.js v5.12.0
  *)
external allocUnsafeSlow : int -> t = "allocUnsafeSlow" [@@bs.val] [@@bs.scope "Buffer"]

(** Returns the actual byte length of a thing. 
    For calculating bytelength of strings, see [byteLengthOfString].

    @param value Array, or Buffer-like value to calculate the length of.

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
    This is not the same as String.prototype.length since that returns the number 
    of characters in a string.

    For '`base64' and '`hex', this function assumes valid input. For strings 
    that contain non-Base64/Hex-encoded data (e.g. whitespace), the return value
    might be greater than the length of a Buffer created from the string.

    @param string A value to calculate the length of.
    @param encoding [string]'s encoding. Default is [`utf8].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_bytelength_string_encoding> Node.js API docs
    @since Node.js v0.1.90
  *)
external byteLengthOfString :
    string ->
    ?encoding: ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
    unit ->
    int = "byteLength"
[@@bs.val] [@@bs.scope "Buffer"]

(** Compares buf1 to buf2 typically for the purpose of sorting arrays of [Buffer] instances. 

    @param buf1 
    @param buf2 
    @return [1] if [buf1] sorts after [buf2], [0] if they are the equal, [-1] if [buf2] sorts after [buf1].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_compare_buf1_buf2> Node.js API docs
    @since Node.js v0.11.13
  *)
external compare : 
  ([`Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ([`Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) -> 
  int = "compare"
[@@bs.val] [@@bs.scope "Buffer"]

(** Compares buf with target and returns a number indicating whether buf comes
    before, after, or is the same as target in sort order.
    Comparison is based on the actual sequence of bytes in each [Buffer]. 
    
    @param buf
    @param target
    @param targetStart The offset within [target] at which to begin comparison. Default: [0].
    @param targetEnd  The offset within target at which to end comparison (not inclusive). Default: [length target].
    @param sourceStart  The offset within buf at which to begin comparison. Default: [0].
    @param sourceEnd  The offset within buf at which to end comparison (not inclusive). Default: [length buf].

    @return [1] if [buf] sorts after [target], [0] if they are the equal, [-1] if [target] sorts after [buf].
    
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

    If the [list] has no items, then a new zero-length [Buffer] is returned.

    @param list List of [Buffer] instances to concat.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_concat_list_totallength> Node.js API docs
    @since Node.js v0.7.11
  *)
external concat : t array -> t = "concat" [@@bs.val] [@@bs.scope "Buffer"]

(** Returns a new [Buffer] which is the result of concatenating all the [Buffer]
    instances in the list together.

    If the [list] has no items, or if the totalLength is 0, then a new zero-length
    Buffer is returned.
    If [totalLength] is not provided, it is calculated from the [Buffer] instances
    in [list]. This however causes an additional loop to be executed in order to
    calculate the [totalLength], so it is faster to provide the length explicitly 
    if it is already known.
    If totalLength is provided, it is coerced to an unsigned integer.
    If the combined length of the Buffers in list exceeds [totalLength], the result
    is truncated to [totalLength].

    @param list List of [Buffer] instances to concat.
    @param totalLength Total length of the [Buffer] instances in list when concatenated.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_concat_list_totallength> Node.js API docs
    @since Node.js v0.7.11
  *)
external concatWithLength : t array -> totalLength:int -> t = "concat" [@@bs.val] [@@bs.scope "Buffer"]

(** Allocates a new [Buffer] using an array of octets.

    @param array Array of octets.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_from_array> Node.js API docs
    @since Node.js v5.10.0
  *)
external fromArray : int array -> t = "from" [@@bs.val] [@@bs.scope "Buffer"]

(** This creates a view of the [ArrayBuffer] without copying the underlying memory.
    For example, when passed a reference to the .buffer property of a [TypedArray]
    instance, the newly created [Buffer] will share the same allocated memory as the
    [TypedArray].

    @param arrayBuffer Source ArrayBuffer instance.
    @param byteOffset Index of first byte to expose. Default: [0].
    @param length Number of bytes to expose. Default: [(length arrayBuffer) - byteOffset].

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

    @param buffer An existing Buffer or Uint8Array from which to copy data.

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

(** Creates a new [Buffer] containing a [string]. The [encoding] parameter identifies
    the character encoding of [string].
    
    @param string A string to encode
    @param encoding The encoding of the string. Default is [`utf8]

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_from_string_encoding> Node.js API docs
    @since Node.js v5.10.0
  *)
external fromStringWithEncoding : string ->
  ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary | `hex ] [@bs.string]) ->
  t = "from"
[@@bs.val] [@@bs.scope "Buffer"]


(** Returns true if obj is a Buffer, false otherwise.

    @param obj Value to test.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_class_method_buffer_isbuffer_obj> Node.js API docs
    @since Node.js v0.1.101
*)
external isBuffer : 'a -> bool = "isBuffer" [@@bs.val] [@@bs.scope "Buffer"]

(** Returns [true] if [encoding] contains a supported character encoding, or [false] otherwise.

    @param encoding String that is tested for supported encoding value.

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

    @param buffer Buffer to get value from.
    @param index Index to read from.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_index> Node.js API docs
  *)
external unsafe_get : t -> int -> int = "" [@@bs.get_index]

(** Unsafe byte writing by its [index].
    Underneath it uses index access syntax in Javascript.

    @param buffer Buffer to write value to.
    @param index Index to write to.
    @param value Value to write. The values refer to individual bytes, so the legal value range is between 0x00 and 0xFF (hex) or 0 and 255 (decimal).

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_index> Node.js API docs
  *)
external unsafe_set : t -> index:int -> int -> unit = "" [@@bs.set_index]

(** Returns the underlying ArrayBuffer object based on which this Buffer object is created.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_buffer> Node.js API docs
  *)
external buffer : t -> Js_typed_array2.ArrayBuffer.t = "buffer" [@@bs.get]

(** Returns [byteOffset] on the underlying [ArrayBuffer] object based on which this
    [Buffer] object is created.

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_byteoffset> Node.js API docs
  *)
external byteOffset : t -> int = "byteOffset" [@@bs.get]

(** Copies data from a region of [buf] to a region in [target] even if the [target]
    memory region overlaps with [buf].

    @param buf
    @param target
    @param targetStart The offset within [target] at which to begin writing. Default: [0].
    @param sourceStart The offset within [buf] from which to begin copying. Default: [0].
    @param sourceEnd The offset within [buf] at which to stop copying (not inclusive). Default: [length buf].

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

(** Returns [true] if both [buf] and [otherBuffer] have exactly the same bytes,
    [false] otherwise.

    @param buf
    @param otherBuffer

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_equals_otherbuffer> Node.js API docs
    @since Node.js v0.11.13
  *)
external equals : t -> t -> bool = "equals" [@@bs.send]

(** Fills [buf] with the specified [value]. If the [offset] and [end_] are not given, 
    the entire [buf] will be filled.

    @param buf
    @param value The value with which to fill [buf].
    @param offset Number of bytes to skip before starting to fill [buf]. Default: [0].
    @param end_ Where to stop filling buf (not inclusive). Default: [length buf].

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

(** Fills [buf] with the specified [value]. If the [offset] and [end_] are not given, 
    the entire [buf] will be filled.

    @param buf
    @param value The value with which to fill [buf].
    @param offset Number of bytes to skip before starting to fill [buf]. Default: [0].
    @param end_ Where to stop filling buf (not inclusive). Default: [length buf].
    @param encoding The encoding for value. Default: [`utf8].

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

(** Tests, if [buf] has provided [value] looking from [byteOffset].

    @param buf
    @param value What to search for.
    @param byteOffset Where to begin searching in [buf]. If negative, then offset is calculated from the end of [buf]. Default: [0].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_includes_value_byteoffset_encoding> Node.js API docs
    @since Node.js v5.3.0
  *)
external includes :
  t -> 
  ([ `Integer of int | `Buffer of t | `Uint8Array of Js_typed_array2.Uint8Array.t ] [@bs.unwrap]) ->
  ?byteOffset:int ->
  unit -> bool = "includes"
[@@bs.send]

(** Tests, if [buf] has provided [value] looking from [byteOffset].

    @param buf
    @param string What to search for.
    @param byteOffset Where to begin searching in [buf]. If negative, then offset is calculated from the end of [buf]. Default: [0].
    @param encoding Encoding of the [string]. Default is [`utf8].

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

    @param buf
    @param value What to search for.
    @param byteOffset Where to begin searching in [buf]. If negative, then offset is calculated from the end of [buf]. Default: [0].

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

    @param buf
    @param string What to search for.
    @param byteOffset Where to begin searching in [buf]. If negative, then offset is calculated from the end of [buf]. Default: [0].
    @param encoding Encoding of the [string]. Default is [`utf8].

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

    @param buf
    @param value What to search for.
    @param byteOffset Where to begin searching in [buf]. If negative, then offset is calculated from the end of [buf]. Default: [(length buf) - 1].

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

    @param buf
    @param string What to search for.
    @param byteOffset Where to begin searching in [buf]. If negative, then offset is calculated from the end of [buf]. Default: [(length buf) - 1].
    @param encoding Encoding of the [string]. Default is [`utf8].

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
    This does not necessarily reflect the amount of "usable" data within [buf].

    @param buf

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

    @param buf
    @param start Where the new [Buffer] will start. Default: [0].
    @param end Where the new [Buffer] will end (not inclusive). Default: [length buf].

    @see <https://nodejs.org/dist/latest-v13.x/docs/api/buffer.html#buffer_buf_subarray_start_end> Node.js API docs
    @since Node.js v3.0.0
  *)
external subarray : t -> ?start: int -> ?end_: int -> unit -> t = "subarray" [@@bs.send]

(** Returns a new [Buffer] that references the same memory as the original, 
    but offset and cropped by the [start] and [end_] indices.

    @param buf
    @param start Where the new [Buffer] will start. Default: [0].
    @param end Where the new [Buffer] will end (not inclusive). Default: [length buf].

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

(** Serializes [buf] to [string] using [encoding] encoding.

    @param buf
    @param encoding Encoding of the resulting [string]. Default is [`utf8].
    @param start The byte offset to start decoding at. Default: [0].
    @param end_ The byte offset to stop decoding at (not inclusive). Default: [length buf].

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

(** Writes [string] to [buf] at [offset] according to the character encoding in [encoding].

    @param buf
    @param string String to write to [buf].
    @param offset Number of bytes to skip before starting to write [string]. Default: [0].
    @param encoding The character encoding of [string]. Default: [`utf8].

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

(** Writes [string] to [buf] at [offset] according to the character encoding in [encoding].
    The [length] parameter is the number of bytes to write.

    @param buf
    @param string String to write to [buf].
    @param offset Number of bytes to skip before starting to write [string]. Default: [0].
    @param length Number of bytes to write. Default: [(length buf) - offset].
    @param encoding The character encoding of [string]. Default: [`utf8].

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

(** Re-encodes the given [Buffer] or [Uint8Array] instance from one character encoding to another.
    Returns a new [Buffer] instance.

    @param buffer [Buffer] or [UInt8Array] to transcode.
    @param fromEnc The current encoding.
    @param toEnc To target encoding.

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

    On 32-bit architectures, this value is (2^30)-1 (~1GB). 
    On 64-bit architectures, this value is (2^31)-1 (~2GB).

    @see <https://nodejs.org/api/buffer.html#buffer_buffer_constants_max_length> Node.js API docs
    @since Node.js v8.2.0
  *)
external _MAX_LENGTH : int = "MAX_LENGTH" [@@bs.module "buffer"] [@@bs.scope "constants"]

(** The largest length allowed for a single [string] instance.

    Represents the largest length that a [string] primitive can have, counted in UTF-16 code units.
    This value may depend on the JS engine that is being used.

    @see <https://nodejs.org/api/buffer.html#buffer_buffer_constants_max_string_length> Node.js API docs
    @since Node.js v8.2.0
  *)
external _MAX_STRING_LENGTH : int = "MAX_STRING_LENGTH" [@@bs.module "buffer"] [@@bs.scope "constants"]


