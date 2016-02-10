 // OCamlScript compiler
 // Copyright (C) 2015 Bloomberg Finance L.P.


 // This program is free software; you can redistribute it and/or modify
 // it under the terms of the GNU Lesser General Public License as published by
 // the Free Software Foundation, with linking exception;
 // either version 2.1 of the License, or (at your option) any later version.

 // This program is distributed in the hope that it will be useful,
 // but WITHOUT ANY WARRANTY; without even the implied warranty of
 // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 // GNU Lesser General Public License for more details.

 // You should have received a copy of the GNU Lesser General Public License
 // along with this program; if not, write to the Free Software
 // Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 // Author: Hongbo Zhang (hzhang295@bloomberg.net)

'use strict';
import {caml_raise_zero_divide} from './caml_exceptions'

/* String.prototype.repeat polyfill
 * If it already exists using [String.prototype.repeat], otherwise provide our own
 * Note our own version does not do lots of error checking, since it's used in a controlled environment
 * TODO: By ignoring error handling we can make an even faster version
 * TODO: Provide a static flag so that Google Closure Compiler can shake the code by defintions
 * TODO: Figure out how polyfills work with Google Closure Compiler and Typescript
 * It's not correct to have code below

 * var str_repeat
 * str_repeat = 3
 * export {str_repeat}
 *
 * It will be compiled to
 *
 * var str_repeat
 * exports.str_repeat
 * str_repeat = 3
 *
 * Here exports.str_repeat is `undefined`
 *
 * The work around is to use

 * export var str_repeat;
 * str_repeat = 3
 *
 *
 * Links: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/repeat
 */
export var repeat : (num : number, self : string) => string;
if (!String.prototype["repeat"]) {
    repeat = (count : number, self) => {
        if (self.length == 0 || count == 0) {
            return '';
        }
        // Ensuring count is a 31-bit integer allows us to heavily optimize the
        // main part. But anyway, most current (August 2014) browsers can't handle
        // strings 1 << 28 chars or longer, so:
        if (self.length * count >= 1 << 28) {
            throw new RangeError('repeat count must not overflow maximum string size');
        }
        var rpt = '';
        for (;;) {
            if ((count & 1) == 1) {
                rpt += self;
            }
            count >>>= 1;
            if (count == 0) {
                break;
            }
            self += self;
        }
        return rpt;
    }
} else {
    repeat = (count, self: string) => {
        // Make typescript compiler happy
        return self["repeat"](count);
    }
}


// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
var i32mul : (x: number, y:number)=> number;
if (!Math["imul"])
    i32mul =
        function (x,y)
        { y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; };
else{
    i32mul = Math["imul"];
}

function i32div(x,y) {
    if (y == 0) caml_raise_zero_divide (0);
    return (x/y)|0;
}

function i32mod(x,y) {
    if (y == 0) caml_raise_zero_divide (0);
    return x%y;
}




export {i32mul, i32div, i32mod}