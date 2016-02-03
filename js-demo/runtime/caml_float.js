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
define(["require", "exports"], function (require, exports) {
    function caml_int64_bits_of_float(x) {
        if (!isFinite(x)) {
            if (isNaN(x))
                return [255, 1, 0, 0xfff0];
            return (x > 0) ? [255, 0, 0, 0x7ff0] : [255, 0, 0, 0xfff0];
        }
        var sign = (x >= 0) ? 0 : 0x8000;
        if (sign)
            x = -x;
        var exp = Math.floor(Math.LOG2E * Math.log(x)) + 1023;
        if (exp <= 0) {
            exp = 0;
            x /= Math.pow(2, -1026);
        }
        else {
            x /= Math.pow(2, exp - 1027);
            if (x < 16) {
                x *= 2;
                exp -= 1;
            }
            if (exp == 0) {
                x /= 2;
            }
        }
        var k = Math.pow(2, 24);
        var r3 = x | 0;
        x = (x - r3) * k;
        var r2 = x | 0;
        x = (x - r2) * k;
        var r1 = x | 0;
        r3 = (r3 & 0xf) | sign | exp << 4;
        return [255, r1, r2, r3];
    }
    exports.caml_int64_bits_of_float = caml_int64_bits_of_float;
    function caml_int64_float_of_bits(x) {
        var exp = (x[3] & 0x7fff) >> 4;
        if (exp == 2047) {
            if ((x[1] | x[2] | (x[3] & 0xf)) == 0)
                return (x[3] & 0x8000) ? (-Infinity) : Infinity;
            else
                return NaN;
        }
        var k = Math.pow(2, -24);
        var res = (x[1] * k + x[2]) * k + (x[3] & 0xf);
        if (exp > 0) {
            res += 16;
            res *= Math.pow(2, exp - 1027);
        }
        else
            res *= Math.pow(2, -1026);
        if (x[3] && 0x8000)
            res = -res;
        return res;
    }
    exports.caml_int64_float_of_bits = caml_int64_float_of_bits;
    function caml_classify_float(x) {
        if (isFinite(x)) {
            if (Math.abs(x) >= 2.2250738585072014e-308)
                return 0;
            if (x != 0)
                return 1;
            return 2;
        }
        return isNaN(x) ? 4 : 3;
    }
    exports.caml_classify_float = caml_classify_float;
    function caml_modf_float(x) {
        if (isFinite(x)) {
            var neg = (1 / x) < 0;
            x = Math.abs(x);
            var i = Math.floor(x);
            var f = x - i;
            if (neg) {
                i = -i;
                f = -f;
            }
            return [0, f, i];
        }
        if (isNaN(x))
            return [0, NaN, NaN];
        return [0, 1 / x, x];
    }
    exports.caml_modf_float = caml_modf_float;
    function caml_ldexp_float(x, exp) {
        exp |= 0;
        if (exp > 1023) {
            exp -= 1023;
            x *= Math.pow(2, 1023);
            if (exp > 1023) {
                exp -= 1023;
                x *= Math.pow(2, 1023);
            }
        }
        if (exp < -1023) {
            exp += 1023;
            x *= Math.pow(2, -1023);
        }
        x *= Math.pow(2, exp);
        return x;
    }
    exports.caml_ldexp_float = caml_ldexp_float;
    function caml_frexp_float(x) {
        if ((x == 0) || !isFinite(x))
            return [0, x, 0];
        var neg = x < 0;
        if (neg)
            x = -x;
        var exp = Math.floor(Math.LOG2E * Math.log(x)) + 1;
        x *= Math.pow(2, -exp);
        if (x < 0.5) {
            x *= 2;
            exp -= 1;
        }
        if (neg)
            x = -x;
        return [0, x, exp];
    }
    exports.caml_frexp_float = caml_frexp_float;
    function caml_float_compare(x, y) {
        if (x === y)
            return 0;
        if (x < y)
            return -1;
        if (x > y)
            return 1;
        if (x === x)
            return 1;
        if (y === y)
            return -1;
        return 0;
    }
    exports.caml_float_compare = caml_float_compare;
    function caml_copysign_float(x, y) {
        if (y == 0)
            y = 1 / y;
        x = Math.abs(x);
        return (y < 0) ? (-x) : x;
    }
    exports.caml_copysign_float = caml_copysign_float;
    function caml_expm1_float(x) {
        var y = Math.exp(x), z = y - 1;
        return (Math.abs(x) > 1 ? z : (z == 0 ? x : x * z / Math.log(y)));
    }
    exports.caml_expm1_float = caml_expm1_float;
    function caml_log1p_float(x) {
        var y = 1 + x, z = y - 1;
        return (z == 0 ? x : x * Math.log(y) / z);
    }
    function caml_hypot_float(x, y) {
        var x0 = Math.abs(x), y0 = Math.abs(y);
        var a = Math.max(x0, y0), b = Math.min(x0, y0) / (a ? a : 1);
        return (a * Math.sqrt(1 + b * b));
    }
    exports.caml_hypot_float = caml_hypot_float;
    /*
     * TODO: these five functions only give approximate results.
     * */
    function caml_log10_float(x) { return Math.LOG10E * Math.log(x); }
    function caml_cosh_float(x) { return (Math.exp(x) + Math.exp(-x)) / 2; }
    function caml_sinh_float(x) { return (Math.exp(x) - Math.exp(-x)) / 2; }
    function caml_tanh_float(x) {
        var y = Math.exp(x), z = Math.exp(-x);
        return (y + z) / (y - z);
    }
});
