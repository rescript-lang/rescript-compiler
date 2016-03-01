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

 // Start md5 support
 export function caml_md5_chan(chan,len){
     if(len<0){
         len = chan.file.data.length - chan.offset;
     }
     return caml_md5_string(chan.file.data,chan.offset,len);
 }

 //Provides: caml_md5_string
 //Requires: caml_string_of_array, caml_convert_string_to_bytes
 export var caml_md5_string =
     function () {
         function add (x, y) { return (x + y) | 0; }
         function xx(q,a,b,x,s,t) {
             a = add(add(a, q), add(x, t));
             return add((a << s) | (a >>> (32 - s)), b);
         }
         function ff(a,b,c,d,x,s,t) {
             return xx((b & c) | ((~b) & d), a, b, x, s, t);
         }
         function gg(a,b,c,d,x,s,t) {
             return xx((b & d) | (c & (~d)), a, b, x, s, t);
         }
         function hh(a,b,c,d,x,s,t) { return xx(b ^ c ^ d, a, b, x, s, t); }
         function ii(a,b,c,d,x,s,t) { return xx(c ^ (b | (~d)), a, b, x, s, t); }

         function md5(buffer, length : number) {
             var i = length;
             buffer[i >> 2] |= 0x80 << (8 * (i & 3));
             for (i = (i & ~0x3) + 8;(i & 0x3F) < 60 ;i += 4)
                 buffer[(i >> 2) - 1] = 0;
             buffer[(i >> 2) -1] = length << 3;
             buffer[i >> 2] = (length >> 29) & 0x1FFFFFFF;

             var w = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476];

             for(i = 0; i < buffer.length; i += 16) {
                 var a = w[0], b = w[1], c = w[2], d = w[3];

                 a = ff(a, b, c, d, buffer[i+ 0], 7, 0xD76AA478);
                 d = ff(d, a, b, c, buffer[i+ 1], 12, 0xE8C7B756);
                 c = ff(c, d, a, b, buffer[i+ 2], 17, 0x242070DB);
                 b = ff(b, c, d, a, buffer[i+ 3], 22, 0xC1BDCEEE);
                 a = ff(a, b, c, d, buffer[i+ 4], 7, 0xF57C0FAF);
                 d = ff(d, a, b, c, buffer[i+ 5], 12, 0x4787C62A);
                 c = ff(c, d, a, b, buffer[i+ 6], 17, 0xA8304613);
                 b = ff(b, c, d, a, buffer[i+ 7], 22, 0xFD469501);
                 a = ff(a, b, c, d, buffer[i+ 8], 7, 0x698098D8);
                 d = ff(d, a, b, c, buffer[i+ 9], 12, 0x8B44F7AF);
                 c = ff(c, d, a, b, buffer[i+10], 17, 0xFFFF5BB1);
                 b = ff(b, c, d, a, buffer[i+11], 22, 0x895CD7BE);
                 a = ff(a, b, c, d, buffer[i+12], 7, 0x6B901122);
                 d = ff(d, a, b, c, buffer[i+13], 12, 0xFD987193);
                 c = ff(c, d, a, b, buffer[i+14], 17, 0xA679438E);
                 b = ff(b, c, d, a, buffer[i+15], 22, 0x49B40821);

                 a = gg(a, b, c, d, buffer[i+ 1], 5, 0xF61E2562);
                 d = gg(d, a, b, c, buffer[i+ 6], 9, 0xC040B340);
                 c = gg(c, d, a, b, buffer[i+11], 14, 0x265E5A51);
                 b = gg(b, c, d, a, buffer[i+ 0], 20, 0xE9B6C7AA);
                 a = gg(a, b, c, d, buffer[i+ 5], 5, 0xD62F105D);
                 d = gg(d, a, b, c, buffer[i+10], 9, 0x02441453);
                 c = gg(c, d, a, b, buffer[i+15], 14, 0xD8A1E681);
                 b = gg(b, c, d, a, buffer[i+ 4], 20, 0xE7D3FBC8);
                 a = gg(a, b, c, d, buffer[i+ 9], 5, 0x21E1CDE6);
                 d = gg(d, a, b, c, buffer[i+14], 9, 0xC33707D6);
                 c = gg(c, d, a, b, buffer[i+ 3], 14, 0xF4D50D87);
                 b = gg(b, c, d, a, buffer[i+ 8], 20, 0x455A14ED);
                 a = gg(a, b, c, d, buffer[i+13], 5, 0xA9E3E905);
                 d = gg(d, a, b, c, buffer[i+ 2], 9, 0xFCEFA3F8);
                 c = gg(c, d, a, b, buffer[i+ 7], 14, 0x676F02D9);
                 b = gg(b, c, d, a, buffer[i+12], 20, 0x8D2A4C8A);

                 a = hh(a, b, c, d, buffer[i+ 5], 4, 0xFFFA3942);
                 d = hh(d, a, b, c, buffer[i+ 8], 11, 0x8771F681);
                 c = hh(c, d, a, b, buffer[i+11], 16, 0x6D9D6122);
                 b = hh(b, c, d, a, buffer[i+14], 23, 0xFDE5380C);
                 a = hh(a, b, c, d, buffer[i+ 1], 4, 0xA4BEEA44);
                 d = hh(d, a, b, c, buffer[i+ 4], 11, 0x4BDECFA9);
                 c = hh(c, d, a, b, buffer[i+ 7], 16, 0xF6BB4B60);
                 b = hh(b, c, d, a, buffer[i+10], 23, 0xBEBFBC70);
                 a = hh(a, b, c, d, buffer[i+13], 4, 0x289B7EC6);
                 d = hh(d, a, b, c, buffer[i+ 0], 11, 0xEAA127FA);
                 c = hh(c, d, a, b, buffer[i+ 3], 16, 0xD4EF3085);
                 b = hh(b, c, d, a, buffer[i+ 6], 23, 0x04881D05);
                 a = hh(a, b, c, d, buffer[i+ 9], 4, 0xD9D4D039);
                 d = hh(d, a, b, c, buffer[i+12], 11, 0xE6DB99E5);
                 c = hh(c, d, a, b, buffer[i+15], 16, 0x1FA27CF8);
                 b = hh(b, c, d, a, buffer[i+ 2], 23, 0xC4AC5665);

                 a = ii(a, b, c, d, buffer[i+ 0], 6, 0xF4292244);
                 d = ii(d, a, b, c, buffer[i+ 7], 10, 0x432AFF97);
                 c = ii(c, d, a, b, buffer[i+14], 15, 0xAB9423A7);
                 b = ii(b, c, d, a, buffer[i+ 5], 21, 0xFC93A039);
                 a = ii(a, b, c, d, buffer[i+12], 6, 0x655B59C3);
                 d = ii(d, a, b, c, buffer[i+ 3], 10, 0x8F0CCC92);
                 c = ii(c, d, a, b, buffer[i+10], 15, 0xFFEFF47D);
                 b = ii(b, c, d, a, buffer[i+ 1], 21, 0x85845DD1);
                 a = ii(a, b, c, d, buffer[i+ 8], 6, 0x6FA87E4F);
                 d = ii(d, a, b, c, buffer[i+15], 10, 0xFE2CE6E0);
                 c = ii(c, d, a, b, buffer[i+ 6], 15, 0xA3014314);
                 b = ii(b, c, d, a, buffer[i+13], 21, 0x4E0811A1);
                 a = ii(a, b, c, d, buffer[i+ 4], 6, 0xF7537E82);
                 d = ii(d, a, b, c, buffer[i+11], 10, 0xBD3AF235);
                 c = ii(c, d, a, b, buffer[i+ 2], 15, 0x2AD7D2BB);
                 b = ii(b, c, d, a, buffer[i+ 9], 21, 0xEB86D391);

                 w[0] = add(a, w[0]);
                 w[1] = add(b, w[1]);
                 w[2] = add(c, w[2]);
                 w[3] = add(d, w[3]);
             }

             var t = new Array(16);

             for (i = 0; i < 4; i++)
                 for (var j = 0; j < 4; j++)
                     t[i * 4 + j] = (w[i] >> (8 * j)) & 0xFF;
             return t;
         }

         return function (s, ofs, len) {
             // FIX: maybe we should perform the computation by chunk of 64 bytes
             // as in http://www.myersdaily.org/joseph/javascript/md5.js
             var buf = [];
             switch (s.t & 6) {
                 default:
                     caml_convert_string_to_bytes(s);
                 case 0: /* BYTES */
                     var b = s.c;
                     for (var i = 0; i < len; i+=4) {
                         var j = i + ofs;
                         buf[i>>2] =
                             b.charCodeAt(j) | (b.charCodeAt(j+1) << 8) |
                             (b.charCodeAt(j+2) << 16) | (b.charCodeAt(j+3) << 24);
                     }
                     for (; i < len; i++) buf[i>>2] |= b.charCodeAt(i + ofs) << (8 * (i & 3));
                     break;
                 case 4: /* ARRAY */
                     var a = s.c;
                     for (var i = 0; i < len; i+=4) {
                         var j = i + ofs;
                         buf[i>>2] = a[j] | (a[j+1] << 8) | (a[j+2] << 16) | (a[j+3] << 24);
                     }
                     for (; i < len; i++) buf[i>>2] |= a[i + ofs] << (8 * (i & 3));
             }
             return caml_string_of_array(md5(buf, len));
         }
     } ();
