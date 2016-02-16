// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard, Andy Ray
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//  Copyright (c) 2015 Bloomberg LP. All rights reserved. 
// Hongbo Zhang (hzhang295@bloomberg.net)              

"use strict";

import {caml_failwith, caml_invalid_argument, caml_undef_module}  from './caml_exceptions'
import {Undefined_recursive_module } from './caml_builtin_exceptions'
import {caml_array_sub} from './caml_array'
import {caml_update_dummy} from './caml_obj'





function caml_convert_raw_backtrace_slot(){
  caml_failwith("caml_convert_raw_backtrace_slot");
}


function caml_bswap16(x) {
  return ((((x & 0x00FF) << 8) |
  ((x & 0xFF00) >> 8)));
}

function caml_int32_bswap(x) {
  return (((x & 0x000000FF) << 24) |
  ((x & 0x0000FF00) << 8) |
  ((x & 0x00FF0000) >> 8) |
  ((x & 0xFF000000) >> 24));
}


function caml_int64_bswap(x) {
  return [
    255,
    (((x[3] & 0x0000ff00) >> 8) |
    ((x[3] & 0x000000ff) << 8) |
    ((x[2] & 0x00ff0000))),
    (((x[2] & 0x0000ff00) >> 8) |
    ((x[2] & 0x000000ff) << 8) |
    ((x[1] & 0x00ff0000))),
    (((x[1] & 0x0000ff00) >> 8) |
    ((x[1] & 0x000000ff) << 8))]
}








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
function hashCode(str : string) {
  var result = 0;
  for (var i = 0; i < str.length; ++i) {
    result = 31 * result + str.charCodeAt(i);
    // Normalize to 4 byte range, 0 ... 2^32.
    result %= HASHCODE_MAX_;
  }
  return result;
};


// Poor man's hash
// Updated later
function caml_hash(count,limit, seed, o) {
    return hashCode(JSON.stringify(o));
}


// TODO: Check NodeJS and browser
function caml_sys_getcwd(unit : number) : string  { 
    return "/"; 
}

export {
    caml_sys_getcwd,
    caml_hash,
    caml_convert_raw_backtrace_slot,
    caml_bswap16,
    caml_int32_bswap,
    caml_int32_bswap as caml_nativeint_bswap,
    caml_int64_bswap
}




