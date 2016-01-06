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
var caml_utils_1 = require('./caml_utils');
var caml_exceptions_1 = require('./caml_exceptions');
/**
 *
 * @param {string} s
 * @returns {number[]}
 */
function parse_sign_and_base(s) {
    var i = 0;
    var len = s.length;
    var base = 10;
    var sign = (len > 0 && s.charCodeAt(0) == 45) ? (i++, -1) : 1;
    if (i + 1 < len && s.charCodeAt(i) == 48)
        switch (s.charCodeAt(i + 1)) {
            case 120:
            case 88:
                base = 16;
                i += 2;
                break;
            case 111:
            case 79:
                base = 8;
                i += 2;
                break;
            case 98:
            case 66:
                base = 2;
                i += 2;
                break;
        }
    return [i, sign, base];
}
exports.parse_sign_and_base = parse_sign_and_base;
function parse_digit(c) {
    if (c >= 48 && c <= 57)
        return c - 48;
    if (c >= 65 && c <= 90)
        return c - 55;
    if (c >= 97 && c <= 122)
        return c - 87;
    return -1;
}
exports.parse_digit = parse_digit;
/**
 *
 * @param {string} s
 * @returns {number}
 */
function caml_int_of_string(s) {
    var _a = parse_sign_and_base(s), i = _a[0], sign = _a[1], base = _a[2];
    var len = s.length;
    var threshold = -1 >>> 0;
    var c = (i < len) ? s.charCodeAt(i) : 0;
    var d = parse_digit(c);
    if (d < 0 || d >= base)
        caml_exceptions_1.caml_failwith("int_of_string");
    var res = d;
    for (i++; i < len; i++) {
        c = s.charCodeAt(i);
        if (c == 95)
            continue;
        d = parse_digit(c);
        if (d < 0 || d >= base)
            break;
        res = base * res + d;
        if (res > threshold)
            caml_exceptions_1.caml_failwith("int_of_string");
    }
    if (i != len)
        caml_exceptions_1.caml_failwith("int_of_string");
    // For base different from 10, we expect an unsigned representation,
    // hence any value of 'res' (less than 'threshold') is acceptable.
    // But we have to convert the result back to a signed integer.
    res = sign * res;
    if ((base == 10) && ((res | 0) != res))
        /* Signed representation expected, allow -2^(nbits-1) to 2^(nbits-1) - 1 */
        caml_exceptions_1.caml_failwith("int_of_string");
    return res | 0;
}
exports.caml_int_of_string = caml_int_of_string;
exports.caml_int32_of_string = caml_int_of_string;
exports.caml_nativeint_of_string = caml_int_of_string;
/**
 * external float_of_string : string -> float = "caml_float_of_string"
 * pervasives.ml
 * Semantics is slightly different from javascript :
 * console.assert(caml_float_of_string('infinity')===Infinity)
 * console.assert(caml_float_of_string('Infinity')===Infinity
 *
 * parseFloat('Infinity') === Infinity
 * parseFloat('infinity') === Nan
 * */
function caml_float_of_string(s) {
    //s = caml_bytes_of_string (s);
    var res = +s;
    if ((s.length > 0) && (res === res))
        return res;
    s = s.replace(/_/g, "");
    res = +s;
    if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) {
        return res;
    }
    ;
    if (/^ *0x[0-9a-f_]+p[+-]?[0-9_]+/i.test(s)) {
        var pidx = s.indexOf('p');
        pidx = (pidx == -1) ? s.indexOf('P') : pidx;
        var exp = +s.substring(pidx + 1);
        res = +s.substring(0, pidx);
        return res * Math.pow(2, exp);
    }
    if (/^\+?inf(inity)?$/i.test(s))
        return Infinity;
    if (/^-inf(inity)?$/i.test(s))
        return -Infinity;
    caml_exceptions_1.caml_failwith("float_of_string");
}
exports.caml_float_of_string = caml_float_of_string;
/**
 * TODO: Check out how it compares with nodejs util.format
 * */
function parse_format(fmt) {
    //fmt = caml_bytes_of_string(fmt);
    var len = fmt.length;
    if (len > 31)
        caml_exceptions_1.caml_invalid_argument("format_int: format too long");
    var f = { justify: '+',
        signstyle: '-',
        filler: ' ',
        alternate: false,
        base: 0,
        signedconv: false,
        width: 0, uppercase: false,
        sign: 1,
        prec: -1,
        conv: 'f' };
    for (var i = 0; i < len; i++) {
        var c = fmt.charAt(i);
        switch (c) {
            case '-':
                f.justify = '-';
                break;
            case '+':
            case ' ':
                f.signstyle = c;
                break;
            case '#':
                f.alternate = true;
                break;
            case '0':
                f.filler = '0';
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                f.width = 0;
                var w;
                while (w = fmt.charCodeAt(i) - 48, w >= 0 && w <= 9) {
                    f.width = f.width * 10 + w;
                    i++;
                }
                i--;
                break;
            case '.':
                f.prec = 0;
                i++;
                var c1;
                while (c1 = fmt.charCodeAt(i) - 48, c1 >= 0 && c1 <= 9) {
                    f.prec = f.prec * 10 + c1;
                    i++;
                }
                i--;
            case 'd':
            case 'i':
                f.signedconv = true; // fall through to have f.base = 10
            case 'u':
                f.base = 10;
                break;
            case 'x':
                f.base = 16;
                break;
            case 'X':
                f.base = 16;
                f.uppercase = true;
                break;
            case 'o':
                f.base = 8;
                break;
            case 'e':
            case 'f':
            case 'g':
                f.signedconv = true;
                f.conv = c;
                break;
            case 'E':
            case 'F':
            case 'G':
                f.signedconv = true;
                f.uppercase = true;
                f.conv = c.toLowerCase();
                break;
        }
    }
    return f;
}
exports.parse_format = parse_format;
function finish_formatting(f, rawbuffer) {
    var len = rawbuffer.length;
    //if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
    /* Adjust len to reflect additional chars (sign, etc) */
    if (f.signedconv && (f.sign < 0 || f.signstyle != '-'))
        len++;
    if (f.alternate) {
        if (f.base == 8)
            len += 1;
        if (f.base == 16)
            len += 2;
    }
    /* Do the formatting */
    var buffer = "";
    if (f.justify == '+' && f.filler == ' ') {
        for (var i = len; i < f.width; i++)
            buffer += ' ';
    }
    if (f.signedconv) {
        if (f.sign < 0)
            buffer += '-';
        else if (f.signstyle != '-')
            buffer += f.signstyle;
    }
    if (f.alternate && f.base == 8) {
        buffer += '0';
    }
    if (f.alternate && f.base == 16) {
        buffer += "0x";
    }
    if (f.justify == '+' && f.filler == '0') {
        for (var i = len; i < f.width; i++)
            buffer += '0';
    }
    if (f.uppercase) {
        buffer += rawbuffer.toUpperCase();
    }
    else {
        buffer += rawbuffer;
    }
    //buffer += rawbuffer;
    if (f.justify == '-')
        for (var i = len; i < f.width; i++)
            buffer += ' ';
    return buffer;
}
exports.finish_formatting = finish_formatting;
/**
 * external format_int : string -> int -> string = "caml_format_int"
 * pervasives.ml
 * */
function caml_format_int(fmt, i) {
    if (fmt == "%d")
        return "" + i;
    var f = parse_format(fmt);
    if (i < 0) {
        if (f.signedconv) {
            f.sign = -1;
            i = -i;
        }
        else
            i >>>= 0;
    }
    var s = i.toString(f.base);
    if (f.prec >= 0) {
        f.filler = ' ';
        var n = f.prec - s.length;
        if (n > 0)
            s = caml_utils_1.repeat(n, '0') + s;
    }
    return finish_formatting(f, s);
}
exports.caml_format_int = caml_format_int;
exports.caml_nativeint_format = caml_format_int;
exports.caml_int32_format = caml_format_int;
/**
 * Noat that for {!Pervasives.float_of_string} the first parameter is fixed,
 * We should do code specialization for it
 * @param fmt
 * @param x
 * @returns {string}
 */
function caml_format_float(fmt, x) {
    var s;
    var f = parse_format(fmt);
    var prec = (f.prec < 0) ? 6 : f.prec;
    if (x < 0) {
        f.sign = -1;
        x = -x;
    }
    if (isNaN(x)) {
        s = "nan";
        f.filler = ' ';
    }
    else if (!isFinite(x)) {
        s = "inf";
        f.filler = ' ';
    }
    else {
        switch (f.conv) {
            case 'e':
                var s = x.toExponential(prec);
                // exponent should be at least two digits
                var i = s.length;
                if (s.charAt(i - 3) == 'e')
                    s = s.slice(0, i - 1) + '0' + s.slice(i - 1);
                break;
            case 'f':
                s = x.toFixed(prec);
                break;
            case 'g':
                prec = prec ? prec : 1;
                s = x.toExponential(prec - 1);
                var j = s.indexOf('e');
                var exp = +s.slice(j + 1);
                if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
                    // remove trailing zeroes
                    var i = j - 1;
                    while (s.charAt(i) == '0')
                        i--;
                    if (s.charAt(i) == '.')
                        i--;
                    s = s.slice(0, i + 1) + s.slice(j);
                    i = s.length;
                    if (s.charAt(i - 3) == 'e')
                        s = s.slice(0, i - 1) + '0' + s.slice(i - 1);
                    break;
                }
                else {
                    var p = prec;
                    if (exp < 0) {
                        p -= exp + 1;
                        s = x.toFixed(p);
                    }
                    else
                        while (s = x.toFixed(p), s.length > prec + 1)
                            p--;
                    if (p) {
                        // remove trailing zeroes
                        var i = s.length - 1;
                        while (s.charAt(i) == '0')
                            i--;
                        if (s.charAt(i) == '.')
                            i--;
                        s = s.slice(0, i + 1);
                    }
                }
                break;
        }
    }
    return finish_formatting(f, s);
}
exports.caml_format_float = caml_format_float;
