 // BuckleScript compiler
 // Copyright (C) 2015-2016 Bloomberg Finance L.P.

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


 // Author: Hongbo Zhang  


'use strict';

var Out_of_memory = [248, "Out_of_memory", 0];
var Sys_error = [248, "Sys_error", -1];
var Failure = [248, "Failure", -2];
var Invalid_argument = [248, "Invalid_argument", -3];
var End_of_file = [248, "End_of_file",-4];
var Division_by_zero = [248, "Division_by_zero", -5];
var Not_found = [248, "Not_found", -6];
var Match_failure = [248, "Match_failure", -7];
var Stack_overflow = [248, "Stack_overflow",-8];
var Sys_blocked_io = [248, "Sys_blocked_io", -9];
var Assert_failure = [248, "Assert_failure", -10];
var Undefined_recursive_module = [248, "Undefined_recursive_module", -11];


/**
 * Exported for better inlining
 * It's common that we have <code> a = caml_set_oo_id([248,"string",0])</code>
 * This can be inlined as <code> a = caml_set_oo_id([248,"tag", caml_oo_last_id++])</code>
 * @type {number}
 */
var caml_oo_last_id = 0;


function caml_set_oo_id (b) {
    b[2]=caml_oo_last_id++;
    return b;
}

export {
    Out_of_memory, Sys_error, Failure,
    Invalid_argument, End_of_file, Division_by_zero,
    Not_found, Match_failure, Stack_overflow, Sys_blocked_io,
    Assert_failure, Undefined_recursive_module,
    caml_oo_last_id,
    caml_set_oo_id
}
