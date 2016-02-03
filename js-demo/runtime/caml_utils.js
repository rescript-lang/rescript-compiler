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
define(["require", "exports", './caml_exceptions'], function (require, exports, caml_exceptions_1) {
    if (!String.prototype["repeat"]) {
        exports.repeat = function (count, self) {
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
        };
    }
    else {
        exports.repeat = function (count, self) {
            // Make typescript compiler happy
            return self["repeat"](count);
        };
    }
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
    var i32mul;
    exports.i32mul = i32mul;
    if (!Math["imul"])
        i32mul =
            function (x, y) { y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y) | 0; };
    else {
        i32mul = Math["imul"];
    }
    function i32div(x, y) {
        if (y == 0)
            caml_exceptions_1.caml_raise_zero_divide();
        return (x / y) | 0;
    }
    exports.i32div = i32div;
    function i32mod(x, y) {
        if (y == 0)
            caml_exceptions_1.caml_raise_zero_divide();
        return x % y;
    }
    exports.i32mod = i32mod;
});
