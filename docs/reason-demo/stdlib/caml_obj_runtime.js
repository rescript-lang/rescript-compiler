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
// Hongbo Zhang (bobzhang1988@gmail.com)              
'use strict';
define(["require", "exports", './caml_exceptions'], function (require, exports, caml_exceptions_1) {
    /**
     * external is_block : t -> bool = "caml_obj_is_block"
     * obj.ml
     * TODO: figure out the semantics
     * @param x
     * @returns {number}
     */
    function caml_obj_is_block(x) { return +(x instanceof Array); }
    exports.caml_obj_is_block = caml_obj_is_block;
    /**
     * TODO: Semantics
     * @param x
     * @returns {number}
     */
    function caml_obj_tag(x) { return (x instanceof Array) ? x[0] : (x instanceof String) ? 252 : 1000; }
    exports.caml_obj_tag = caml_obj_tag;
    /**
     * TODO: semantics
     * @param x
     * @param tag
     * @returns {number}
     */
    function caml_obj_set_tag(x, tag) { x[0] = tag; return 0; }
    exports.caml_obj_set_tag = caml_obj_set_tag;
    /**
     * TODO: we should inline when tag, size is known at compile time and size is not very big
     * TODO: Semantics
     * Question: Do we need intialize the block? seems not necessary
     * @param tag
     * @param size
     * @returns {any[]}
     */
    function caml_obj_block(tag, size) {
        var o = new Array(size + 1);
        o[0] = tag;
        // TODO: intialization seems unnecessary
        for (var i = 1; i <= size; i++)
            o[i] = 0;
        return o;
    }
    exports.caml_obj_block = caml_obj_block;
    /**
     * TODO: semantics
     * @param x
     * @returns {any[]}
     */
    function caml_obj_dup(x) {
        // According to http://jsperf.com/new-array-vs-splice-vs-slice/31
        return x.slice();
    }
    exports.caml_obj_dup = caml_obj_dup;
    /**
     * TODO: semantics
     * @param x
     * @param s
     * @returns {number}
     */
    function caml_obj_truncate(x, s) {
        if (s <= 0 || s + 1 > x.length)
            caml_exceptions_1.caml_invalid_argument("Obj.truncate");
        if (x.length != s + 1)
            x.length = s + 1;
        return 0;
    }
    exports.caml_obj_truncate = caml_obj_truncate;
    function caml_lazy_make_forward(v) { return [250, v]; }
    exports.caml_lazy_make_forward = caml_lazy_make_forward;
});
