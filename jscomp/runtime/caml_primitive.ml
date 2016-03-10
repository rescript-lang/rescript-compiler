(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)

open Nativeint 

let caml_bswap16 (x : nativeint) = 
  logor (shift_left (logand x 0x00ffn) 8)
    (shift_right_logical (logand x 0xff00n) 8)

let caml_int32_bswap (x : nativeint) = 
  logor (shift_left (logand x  0x000000FFn) 24)
     (logor (shift_left (logand x  0x0000FF00n)  8)
        (logor (shift_right_logical (logand x  0x00FF0000n)  8) 
      (shift_right_logical (logand x  0xFF000000n)  24)))

[%%bb.unsafe{|

/**
 * Maximum value of #goog.string.hashCode, exclusive. 2^32.
 * @type {number}
 * @private
 */
var HASHCODE_MAX_ = 0x100000000;
/**
 * String hash function similar to java.lang.String.hashCode().
 * The hash code for a string is computed as
 * s[0] * 31 ^ (n - 1) + s[1] * 31 ^ (n - 2) + ... + s[n - 1],
 * where s[i] is the ith character of the string and n is the length of
 * the string. We mod the result to make it between 0 (inclusive) and 2^32
 * (exclusive).
 * @param {string} str A string.
 * @return {number} Hash value for {@code str}, between 0 (inclusive) and 2^32
 *  (exclusive). The empty string returns 0.
 */
function $$hashCode(str) {
    var result = 0;
    for (var i = 0; i < str.length; ++i) {
        result = 31 * result + str.charCodeAt(i);
        // Normalize to 4 byte range, 0 ... 2^32.
        result %= HASHCODE_MAX_;
    }
    return result;
}
;

// Poor man's hash
// Updated later
function $$caml_hash(count, limit, seed, o) {
    return $$hashCode(JSON.stringify(o));
}
|}]


external caml_hash : int -> int -> int -> 'a -> int = "$$caml_hash"
    [@@js.call ] [@@js.local]

let caml_nativeint_bswap = caml_int32_bswap

let caml_sys_getcwd () = "/"

let caml_convert_raw_backtrace_slot : Printexc.raw_backtrace_slot -> Printexc. backtrace_slot 
  =
  function _ -> 
    raise @@ Failure "caml_convert_raw_backtrace_slot unimplemented"
