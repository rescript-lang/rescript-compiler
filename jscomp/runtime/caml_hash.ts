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

///////////// Hashtbl
//Provides: caml_hash_univ_param mutable
//Requires: String, caml_convert_string_to_bytes
//Requires: caml_int64_to_bytes, caml_int64_bits_of_float
export function caml_hash_univ_param (count, limit, obj) {
    var hash_accu = 0;
    function hash_aux (obj) {
        limit --;
        if (count < 0 || limit < 0) return;
        if (obj instanceof Array && obj[0] === (obj[0]|0)) {
            switch (obj[0]) {
                case 248:
                    // Object
                    count --;
                    hash_accu = (hash_accu * 65599 + obj[2]) | 0;
                    break;
                case 250:
                    // Forward
                    limit++; hash_aux(obj); break;
                case 255:
                    // Int64
                    count --;
                    hash_accu = (hash_accu * 65599 + obj[1] + (obj[2] << 24)) | 0;
                    break;
                default:
                    count --;
                    hash_accu = (hash_accu * 19 + obj[0]) | 0;
                    for (var i = obj.length - 1; i > 0; i--) hash_aux (obj[i]);
            }
        } else if (obj instanceof String) {
            count --;
            switch (obj.t & 6) {
                default: /* PARTIAL */
                    caml_convert_string_to_bytes(obj);
                case 0: /* BYTES */
                    for (var b = obj.c, l = obj.l, i = 0; i < l; i++)
                        hash_accu = (hash_accu * 19 + b.charCodeAt(i)) | 0;
                    break;
                case 2: /* ARRAY */
                    for (var a = obj.c, l = obj.l, i = 0; i < l; i++)
                        hash_accu = (hash_accu * 19 + a[i]) | 0;
            }
        } else if (obj === (obj|0)) {
            // Integer
            count --;
            hash_accu = (hash_accu * 65599 + obj) | 0;
        } else if (obj === +obj) {
            // Float
            count--;
            var p = caml_int64_to_bytes (caml_int64_bits_of_float (obj));
            for (var i = 7; i >= 0; i--) hash_accu = (hash_accu * 19 + p[i]) | 0;
        }
    }
    hash_aux (obj);
    return hash_accu & 0x3FFFFFFF;
}

//Provides: caml_hash mutable
//Requires: String, caml_convert_string_to_bytes
//Requires: caml_int64_bits_of_float, i32mul
export var caml_hash = function () {
    var HASH_QUEUE_SIZE = 256;
    function rotl32(x,n) { return ((x << n) | (x >>> (32-n))); }
    function mix(h,d) {
        d = caml_mul(d, 0xcc9e2d51|0);
        d = rotl32(d, 15);
        d = caml_mul(d, 0x1b873593);
        h ^= d;
        h = rotl32(h, 13);
        return (((h + (h << 2))|0) + (0xe6546b64|0))|0;
    }
    function final_mix(h) {
        h ^= h >>> 16;
        h = caml_mul (h, 0x85ebca6b|0);
        h ^= h >>> 13;
        h = caml_mul (h, 0xc2b2ae35|0);
        h ^= h >>> 16;
        return h;
    }
    function caml_hash_mix_int64 (h, v) {
        var lo = v[1] | (v[2] << 24);
        var hi = (v[2] >>> 8) | (v[3] << 16);
        h = mix(h, lo);
        h = mix(h, hi);
        return h;
    }
    function caml_hash_mix_int64_2 (h, v) {
        var lo = v[1] | (v[2] << 24);
        var hi = (v[2] >>> 8) | (v[3] << 16);
        h = mix(h, hi ^ lo);
        return h;
    }
    function caml_hash_mix_string_str(h, s) {
        var len = s.length, i, w;
        for (i = 0; i + 4 <= len; i += 4) {
            w = s.charCodeAt(i)
                | (s.charCodeAt(i+1) << 8)
                | (s.charCodeAt(i+2) << 16)
                | (s.charCodeAt(i+3) << 24);
            h = mix(h, w);
        }
        w = 0;
        switch (len & 3) {
            case 3: w  = s.charCodeAt(i+2) << 16;
            case 2: w |= s.charCodeAt(i+1) << 8;
            case 1: w |= s.charCodeAt(i);
                h = mix(h, w);
            default:
        }
        h ^= len;
        return h;
    }
    function caml_hash_mix_string_arr(h, s) {
        var len = s.length, i, w;
        for (i = 0; i + 4 <= len; i += 4) {
            w = s[i]
                | (s[i+1] << 8)
                | (s[i+2] << 16)
                | (s[i+3] << 24);
            h = mix(h, w);
        }
        w = 0;
        switch (len & 3) {
            case 3: w  = s[i+2] << 16;
            case 2: w |= s[i+1] << 8;
            case 1: w |= s[i];
                h = mix(h, w);
            default:
        }
        h ^= len;
        return h;
    }
    return function (count, limit, seed, obj) {
        var queue, rd, wr, sz, num, h, v, i, len;
        sz = limit;
        if (sz < 0 || sz > HASH_QUEUE_SIZE) sz = HASH_QUEUE_SIZE;
        num = count;
        h = seed;
        queue = [obj]; rd = 0; wr = 1;
        while (rd < wr && num > 0) {
            v = queue[rd++];
            if (v instanceof Array && v[0] === (v[0]|0)) {
                switch (v[0]) {
                    case 248:
                        // Object
                        h = mix(h, v[2]);
                        num--;
                        break;
                    case 250:
                        // Forward
                        queue[--rd] = v[1];
                        break;
                    case 255:
                        // Int64
                        h = caml_hash_mix_int64_2 (h, v);
                        num --;
                        break;
                    default:
                        var tag = ((v.length - 1) << 10) | v[0];
                        h = mix(h, tag);
                        for (i = 1, len = v.length; i < len; i++) {
                            if (wr >= sz) break;
                            queue[wr++] = v[i];
                        }
                        break;
                }
            } else if (v instanceof String) {
                switch (v.t & 6) {
                    default:
                        caml_convert_string_to_bytes (v);
                    case 0: /* BYTES */
                        h = caml_hash_mix_string_str(h, v.c);
                        break;
                    case 2: /* ARRAY */
                        h = caml_hash_mix_string_arr(h, v.c);
                }
                num--;
            } else if (v === (v|0)) {
                // Integer
                h = mix(h, v+v+1);
                num--;
            } else if (v === +v) {
                // Float
                h = caml_hash_mix_int64(h, caml_int64_bits_of_float (v));
                num--;
            }
        }
        h = final_mix(h);
        return h & 0x3FFFFFFF;
    }
} ();

