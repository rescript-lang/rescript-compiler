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
define(["require", "exports"], function (require, exports) {
    var caml_named_values = {};
    /**
     * external register_named_value : string -> 'a -> unit
     * stdlib/printexc.ml
     * @param nm
     * @param v
     * @returns {number}
     */
    function caml_register_named_value(nm, v) {
        caml_named_values[nm] = v;
        return 0;
    }
    exports.caml_register_named_value = caml_register_named_value;
    /**
     * byterun/callback.c :
     * CAMLExport value * caml_named_value(char const *name)
     * In OCaml world, [caml_named_value] and [caml_register_named_value] is only useful
     * When FFI with C functions
     * @param nm
     * @returns {any}
     */
    function caml_named_value(nm) {
        return caml_named_values[nm];
    }
    exports.caml_named_value = caml_named_value;
    // export var caml_global_data = [0];
    //function caml_register_global (n, v, name_opt) {
    //  caml_global_data[n + 1] = v;
    //  if(name_opt) caml_global_data[name_opt] = v;
    //}
});
