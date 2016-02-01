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
var caml_exceptions_1 = require('./caml_exceptions');
function caml_update_dummy(x, y) {
    var i = y.length;
    while (i--) {
        x[i] = y[i];
    }
    ;
    return 0;
}
exports.caml_update_dummy = caml_update_dummy;
function caml_compare_val(a, b, total) {
    var stack = [];
    for (;;) {
        if (!(total && a === b)) {
            if (a === b) {
                return 0;
            }
            if (typeof a === 'string') {
                if (a < b) {
                    return -1;
                }
                else {
                    return 1;
                }
            }
            else if (a instanceof Array && a[0] === (a[0] | 0)) {
                var ta = a[0];
                // ignore double_array_tag
                if (ta === 254)
                    ta = 0;
                // Forward object
                if (ta === 250) {
                    a = a[1];
                    continue;
                }
                else if (b instanceof Array && b[0] === (b[0] | 0)) {
                    var tb = b[0];
                    // ignore double_array_tag
                    if (tb === 254)
                        tb = 0;
                    // Forward object
                    if (tb === 250) {
                        b = b[1];
                        continue;
                    }
                    else if (ta != tb) {
                        return (ta < tb) ? -1 : 1;
                    }
                    else {
                        switch (ta) {
                            case 248: {
                                // Object
                                var x = caml_int_compare(a[2], b[2]);
                                if (x != 0)
                                    return x;
                                break;
                            }
                            case 251: {
                                caml_exceptions_1.caml_invalid_argument("equal: abstract value");
                            }
                            /*
                             * TODO: generic comparison
                            case 255: {
                              // Int64
                              var x = caml_int64_compare(a, b);
                              if (x != 0) return x;
                              break;
                            }
                            */
                            default:
                                if (a.length != b.length)
                                    return (a.length < b.length) ? -1 : 1;
                                if (a.length > 1)
                                    stack.push(a, b, 1);
                        }
                    }
                }
                else
                    return 1;
            }
            else if (b instanceof String ||
                (b instanceof Array && b[0] === (b[0] | 0))) {
                return -1;
            }
            else if (typeof a != "number" && a && a.compare) {
                return a.compare(b, total);
            }
            else {
                if (a < b)
                    return -1;
                if (a > b)
                    return 1;
                if (a != b) {
                    if (!total)
                        return NaN;
                    if (a == a)
                        return 1;
                    if (b == b)
                        return -1;
                }
            }
        }
        if (stack.length == 0)
            return 0;
        var i = stack.pop();
        b = stack.pop();
        a = stack.pop();
        if (i + 1 < a.length)
            stack.push(a, b, i + 1);
        a = a[i];
        b = b[i];
    }
}
function caml_compare(a, b) { return caml_compare_val(a, b, 1); }
exports.caml_compare = caml_compare;
/**
 * TODO: shall we Inline it?
 * @param a
 * @param b
 * @returns {number}
 */
function caml_int_compare(a, b) {
    if (a < b)
        return (-1);
    if (a === b)
        return 0;
    return 1;
}
exports.caml_int_compare = caml_int_compare;
exports.caml_int32_compare = caml_int_compare;
exports.caml_nativeint_compare = caml_int_compare;
/**
 * @returns {number}
 * Currently ocaml only has number not boolean, and we can not
 * use boolean instead of number directly, since a boolean maybe
 * used in pattern match and generate code such as `x!==0`
 * Note we use `+boolean` to convert
 * +true -> 1
 * +false -> 0
 */
function caml_equal(x, y) { return +(caml_compare_val(x, y, false) == 0); }
exports.caml_equal = caml_equal;
function caml_notequal(x, y) {
    return +(caml_compare_val(x, y, false) != 0);
}
exports.caml_notequal = caml_notequal;
function caml_greaterequal(x, y) {
    return +(caml_compare_val(x, y, false) >= 0);
}
exports.caml_greaterequal = caml_greaterequal;
function caml_greaterthan(x, y) {
    return +(caml_compare_val(x, y, false) > 0);
}
exports.caml_greaterthan = caml_greaterthan;
function caml_lessequal(x, y) {
    return +(caml_compare_val(x, y, false) <= 0);
}
exports.caml_lessequal = caml_lessequal;
function caml_lessthan(x, y) {
    return +(caml_compare_val(x, y, false) < 0);
}
exports.caml_lessthan = caml_lessthan;
function caml_convert_raw_backtrace_slot() {
    caml_exceptions_1.caml_failwith("caml_convert_raw_backtrace_slot");
}
exports.caml_convert_raw_backtrace_slot = caml_convert_raw_backtrace_slot;
function caml_bswap16(x) {
    return ((((x & 0x00FF) << 8) |
        ((x & 0xFF00) >> 8)));
}
exports.caml_bswap16 = caml_bswap16;
function caml_int32_bswap(x) {
    return (((x & 0x000000FF) << 24) |
        ((x & 0x0000FF00) << 8) |
        ((x & 0x00FF0000) >> 8) |
        ((x & 0xFF000000) >> 24));
}
exports.caml_int32_bswap = caml_int32_bswap;
exports.caml_nativeint_bswap = caml_int32_bswap;
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
            ((x[1] & 0x000000ff) << 8))];
}
exports.caml_int64_bswap = caml_int64_bswap;
function caml_CamlinternalMod_init_mod(loc, shape) {
    function undef_module(_x) {
        caml_exceptions_1.caml_raise_with_arg(caml_exceptions_1.Undefined_recursive_module, loc);
    }
    function loop(shape, struct, idx) {
        if (typeof shape === "number")
            switch (shape) {
                case 0:
                    struct[idx] = { fun: undef_module };
                    break;
                case 1:
                    struct[idx] = [246, undef_module];
                    break;
                default:
                    struct[idx] = [];
            }
        else
            switch (shape[0]) {
                case 0:
                    struct[idx] = [0];
                    for (var i = 1; i < shape[1].length; i++)
                        loop(shape[1][i], struct[idx], i);
                    break;
                default:
                    struct[idx] = shape[1];
            }
    }
    var res = [];
    loop(shape, res, 0);
    return res[0];
}
function caml_CamlinternalMod_update_mod(shape, real, x) {
    if (typeof shape === "number")
        switch (shape) {
            case 0:
                real.fun = x;
                break;
            case 1: //lazy
            default:
                caml_update_dummy(real, x);
        }
    else
        switch (shape[0]) {
            case 0:
                for (var i = 1; i < shape[1].length; i++)
                    caml_CamlinternalMod_update_mod(shape[1][i], real[i], x[i]);
                break;
            //case 1://Value
            default:
        }
    ;
    return 0;
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
function hashCode(str) {
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
function caml_hash(count, limit, seed, o) {
    return hashCode(JSON.stringify(o));
}
exports.caml_hash = caml_hash;
