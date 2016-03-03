// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

function caml_bswap16(x) {
  return ((x & 255) << 8) | ((x & 65280) >>> 8);
}

function caml_int32_bswap(x) {
  return ((x & 255) << 24) | ((x & 65280) << 8) | ((x & 16711680) >>> 8) | ((x & 4278190080) >>> 24);
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

;

function caml_sys_getcwd() {
  return "/";
}

function caml_convert_raw_backtrace_slot() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_convert_raw_backtrace_slot unimplemented"
      ];
}

function caml_hash(prim, prim$1, prim$2, prim$3) {
  return $$caml_hash(prim, prim$1, prim$2, prim$3);
}

var caml_nativeint_bswap = caml_int32_bswap;

exports.caml_sys_getcwd                 = caml_sys_getcwd;
exports.caml_hash                       = caml_hash;
exports.caml_bswap16                    = caml_bswap16;
exports.caml_int32_bswap                = caml_int32_bswap;
exports.caml_nativeint_bswap            = caml_nativeint_bswap;
exports.caml_convert_raw_backtrace_slot = caml_convert_raw_backtrace_slot;
/*  Not a pure module */
