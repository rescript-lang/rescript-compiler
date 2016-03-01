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
import {repeat, } from './caml_utils'
import {caml_obj_dup} from './caml_obj_runtime'
import {parse_format, finish_formatting, parse_sign_and_base, parse_digit } from './caml_format'
import {caml_raise_zero_divide, caml_failwith} from './caml_exceptions'

var caml_int64_offset = Math.pow(2, -24);


function caml_int64_ucompare(x,y) {
    if (x[3] > y[3]) return 1;
    if (x[3] < y[3]) return -1;
    if (x[2] > y[2]) return 1;
    if (x[2] < y[2]) return -1;
    if (x[1] > y[1]) return 1;
    if (x[1] < y[1]) return -1;
    return 0;
}


function caml_int64_ult(x,y) { return caml_int64_ucompare(x,y) < 0; }

//Provides: caml_int64_compare const
export function caml_int64_compare(x,y) {
    var x3 = x[3] << 16;
    var y3 = y[3] << 16;
    if (x3 > y3) return 1;
    if (x3 < y3) return -1;
    if (x[2] > y[2]) return 1;
    if (x[2] < y[2]) return -1;
    if (x[1] > y[1]) return 1;
    if (x[1] < y[1]) return -1;
    return 0;
}

//Provides: caml_int64_neg const
export function caml_int64_neg (x) {
    var y1 = - x[1];
    var y2 = - x[2] + (y1 >> 24);
    var y3 = - x[3] + (y2 >> 24);
    return [255, y1 & 0xffffff, y2 & 0xffffff, y3 & 0xffff];
}

//Provides: caml_int64_add const
export function caml_int64_add (x  , y  ) {
    var z1 = x[1] + y[1];
    var z2 = x[2] + y[2] + (z1 >> 24);
    var z3 = x[3] + y[3] + (z2 >> 24);
    return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}

//Provides: caml_int64_sub const
export function caml_int64_sub (x, y) {
    var z1 = x[1] - y[1];
    var z2 = x[2] - y[2] + (z1 >> 24);
    var z3 = x[3] - y[3] + (z2 >> 24);
    return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}

//Provides: caml_int64_mul const
//Requires: caml_int64_offset
export function caml_int64_mul(x,y) {
    var z1 = x[1] * y[1];
    var z2 = ((z1 * caml_int64_offset) | 0) + x[2] * y[1] + x[1] * y[2];
    var z3 = ((z2 * caml_int64_offset) | 0) + x[3] * y[1] + x[2] * y[2] + x[1] * y[3];
    return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}

//Provides: caml_int64_is_zero const
function caml_int64_is_zero(x) {
    return (x[3]|x[2]|x[1]) == 0;
}

//Provides: caml_int64_is_negative const
function caml_int64_is_negative(x) {
    return (x[3] << 16) < 0;
}

//Provides: caml_int64_is_min_int const
function caml_int64_is_min_int(x) {
    return x[3] == 0x8000 && (x[1]|x[2]) == 0;
}

//Provides: caml_int64_is_minus_one const
function caml_int64_is_minus_one(x) {
    return x[3] == 0xffff && (x[1]&x[2]) == 0xffffff;
}

//Provides: caml_int64_and const
export function caml_int64_and (x, y) {
    return [255, x[1]&y[1], x[2]&y[2], x[3]&y[3]];
}

//Provides: caml_int64_or const
export function caml_int64_or (x, y) {
    return [255, x[1]|y[1], x[2]|y[2], x[3]|y[3]];
}

//Provides: caml_int64_xor const
export function caml_int64_xor (x, y) {
    return [255, x[1]^y[1], x[2]^y[2], x[3]^y[3]];
}

//Provides: caml_int64_shift_left const
export function caml_int64_shift_left (x, s) {
    s = s & 63;
    if (s == 0) return x;
    if (s < 24)
        return [255,
            (x[1] << s) & 0xffffff,
            ((x[2] << s) | (x[1] >> (24 - s))) & 0xffffff,
            ((x[3] << s) | (x[2] >> (24 - s))) & 0xffff];
    if (s < 48)
        return [255, 0,
            (x[1] << (s - 24)) & 0xffffff,
            ((x[2] << (s - 24)) | (x[1] >> (48 - s))) & 0xffff];
    return [255, 0, 0, (x[1] << (s - 48)) & 0xffff];
}

//Provides: caml_int64_shift_right_unsigned const
export function caml_int64_shift_right_unsigned (x, s) {
    s = s & 63;
    if (s == 0) return x;
    if (s < 24)
        return [255,
            ((x[1] >> s) | (x[2] << (24 - s))) & 0xffffff,
            ((x[2] >> s) | (x[3] << (24 - s))) & 0xffffff,
            (x[3] >> s)];
    if (s < 48)
        return [255,
            ((x[2] >> (s - 24)) | (x[3] << (48 - s))) & 0xffffff,
            (x[3] >> (s - 24)),
            0];
    return [255, (x[3] >> (s - 48)), 0, 0];
}

//Provides: caml_int64_shift_right const
export function caml_int64_shift_right (x, s) {
    s = s & 63;
    if (s == 0) return x;
    var h = (x[3] << 16) >> 16;
    if (s < 24)
        return [255,
            ((x[1] >> s) | (x[2] << (24 - s))) & 0xffffff,
            ((x[2] >> s) | (h << (24 - s))) & 0xffffff,
            ((x[3] << 16) >> s) >>> 16];
    var sign = (x[3] << 16) >> 31;
    if (s < 48)
        return [255,
            ((x[2] >> (s - 24)) | (x[3] << (48 - s))) & 0xffffff,
            ((x[3] << 16) >> (s - 24) >> 16) & 0xffffff,
            sign & 0xffff];
    return [255,
        ((x[3] << 16) >> (s - 32)) & 0xffffff,
        sign & 0xffffff, sign & 0xffff];
}

//Provides: caml_int64_lsl1 const
function caml_int64_lsl1 (x) {
    x[3] = (x[3] << 1) | (x[2] >> 23);
    x[2] = ((x[2] << 1) | (x[1] >> 23)) & 0xffffff;
    x[1] = (x[1] << 1) & 0xffffff;
}

//Provides: caml_int64_lsr1 const
function caml_int64_lsr1 (x) {
    x[1] = ((x[1] >>> 1) | (x[2] << 23)) & 0xffffff;
    x[2] = ((x[2] >>> 1) | (x[3] << 23)) & 0xffffff;
    x[3] = x[3] >>> 1;
}

//Provides: caml_int64_udivmod const
//Requires: caml_int64_ucompare, caml_int64_lsl1, caml_int64_lsr1
//Requires: caml_int64_sub
//Requires: caml_obj_dup
function caml_int64_udivmod (x, y) {
    var offset = 0;
    var modulus = caml_obj_dup(x);
    var divisor = caml_obj_dup(y);
    var quotient = [255, 0, 0, 0];
    while (caml_int64_ucompare (modulus, divisor) > 0) {
        offset++;
        caml_int64_lsl1 (divisor);
    }
    while (offset >= 0) {
        offset --;
        caml_int64_lsl1 (quotient);
        if (caml_int64_ucompare (modulus, divisor) >= 0) {
            quotient[1] ++;
            modulus = caml_int64_sub (modulus, divisor);
        }
        caml_int64_lsr1 (divisor);
    }
    return [0,quotient, modulus];
}

//Provides: caml_int64_div const
//Requires: caml_int64_is_zero, caml_raise_zero_divide
//Requires: caml_int64_neg, caml_int64_udivmod
export function caml_int64_div (x, y)
{
    if (caml_int64_is_zero (y)) caml_raise_zero_divide (0);
    var sign = x[3] ^ y[3];
    if (x[3] && 0x8000) x = caml_int64_neg(x);
    if (y[3] && 0x8000) y = caml_int64_neg(y);
    var q = caml_int64_udivmod(x, y)[1];
    if (sign && 0x8000) q = caml_int64_neg(q);
    return q;
}

//Provides: caml_int64_mod const
//Requires: caml_int64_is_zero, caml_raise_zero_divide
//Requires: caml_int64_neg, caml_int64_udivmod
export function caml_int64_mod (x, y)
{
    if (caml_int64_is_zero (y)) caml_raise_zero_divide (0);
    var sign = x[3] ^ y[3];
    if (x[3] && 0x8000) x = caml_int64_neg(x);
    if (y[3] && 0x8000) y = caml_int64_neg(y);
    var r = caml_int64_udivmod(x, y)[2];
    if (sign && 0x8000) r = caml_int64_neg(r);
    return r;
}

//Provides: caml_int64_of_int32 const
export function caml_int64_of_int32 (x) {
    return [255, x & 0xffffff, (x >> 24) & 0xffffff, (x >> 31) & 0xffff]
}

export var caml_int64_of_int = caml_int64_of_int32;
export var caml_int64_of_nativeint = caml_int64_of_int32;
//Provides: caml_int64_to_int32 const
export function caml_int64_to_int32 (x) {
    return x[1] | (x[2] << 24);
}

export var caml_int64_to_int = caml_int64_to_int32;
export var caml_int64_to_nativeint = caml_int64_to_int32;
//Provides: caml_int64_to_float const
export function caml_int64_to_float (x) {
    return ((x[3] << 16) * Math.pow(2, 32) + x[2] * Math.pow(2, 24)) + x[1];
}

export function caml_int64_of_float (x) {
    if (x < 0) x = Math.ceil(x);
    return [255,
        x & 0xffffff,
        Math.floor(x * caml_int64_offset) & 0xffffff,
        Math.floor(x * caml_int64_offset * caml_int64_offset) & 0xffff];
}

export function caml_int64_format (fmt, x) {
    var f = parse_format(fmt);
    if (f.signedconv && caml_int64_is_negative(x)) {
        f.sign = -1; x = caml_int64_neg(x);
    }
    var buffer = "";
    var wbase = caml_int64_of_int32(f.base);
    var cvtbl = "0123456789abcdef";
    do {
        var p = caml_int64_udivmod(x, wbase);
        x = p[1];
        buffer = cvtbl.charAt(caml_int64_to_int32(p[2])) + buffer;
    } while (! caml_int64_is_zero(x));
    if (f.prec >= 0) {
        f.filler = ' ';
        var n = f.prec - buffer.length;
        if (n > 0) buffer = repeat(n,'0') + buffer;
    }
    return finish_formatting(f, buffer);
}


export function caml_int64_of_string(s) {
    var r = parse_sign_and_base (s);
    var i = r[0], sign = r[1], base = r[2];
    var base64 = caml_int64_of_int32(base);
    var threshold =
        caml_int64_udivmod([255, 0xffffff, 0xfffffff, 0xffff], base64)[1];
    var c = s.charCodeAt(i);
    var d = parse_digit(c);
    if (d < 0 || d >= base) caml_failwith("int_of_string");
    var res = caml_int64_of_int32(d);
    for (;;) {
        i++;
        c = s.charCodeAt(i);
        if (c == 95) continue;
        d = parse_digit(c);
        if (d < 0 || d >= base) break;
        /* Detect overflow in multiplication base * res */
        if (caml_int64_ult(threshold, res)) caml_failwith("int_of_string");
        var d0 = caml_int64_of_int32(d);
        res = caml_int64_add(caml_int64_mul(base64, res), d0);
        /* Detect overflow in addition (base * res) + d */
        if (caml_int64_ult(res, d0)) caml_failwith("int_of_string");
    }
    if (i != s.length) caml_failwith("int_of_string");
    if (r[2] == 10 && caml_int64_ult([255, 0, 0, 0x8000], res))
        caml_failwith("int_of_string");
    if (sign < 0) res = caml_int64_neg(res);
    return res;
}


function caml_int64_of_bytes(a) {
    return [255, a[7] | (a[6] << 8) | (a[5] << 16),
        a[4] | (a[3] << 8) | (a[2] << 16), a[1] | (a[0] << 8)];
}

function caml_int64_to_bytes(x) {
    return [x[3] >> 8, x[3] & 0xff, x[2] >> 16, (x[2] >> 8) & 0xff, x[2] & 0xff,
        x[1] >> 16, (x[1] >> 8) & 0xff, x[1] & 0xff];
}

