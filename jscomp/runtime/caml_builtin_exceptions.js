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
exports.Out_of_memory = Out_of_memory;
var Sys_error = [248, "Sys_error", -1];
exports.Sys_error = Sys_error;
var Failure = [248, "Failure", -2];
exports.Failure = Failure;
var Invalid_argument = [248, "Invalid_argument", -3];
exports.Invalid_argument = Invalid_argument;
var End_of_file = [248, "End_of_file", -4];
exports.End_of_file = End_of_file;
var Division_by_zero = [248, "Division_by_zero", -5];
exports.Division_by_zero = Division_by_zero;
var Not_found = [248, "Not_found", -6];
exports.Not_found = Not_found;
var Match_failure = [248, "Match_failure", -7];
exports.Match_failure = Match_failure;
var Stack_overflow = [248, "Stack_overflow", -8];
exports.Stack_overflow = Stack_overflow;
var Sys_blocked_io = [248, "Sys_blocked_io", -9];
exports.Sys_blocked_io = Sys_blocked_io;
var Assert_failure = [248, "Assert_failure", -10];
exports.Assert_failure = Assert_failure;
var Undefined_recursive_module = [248, "Undefined_recursive_module", -11];
exports.Undefined_recursive_module = Undefined_recursive_module;
/**
 * Exported for better inlining
 * It's common that we have <code> a = caml_set_oo_id([248,"string",0])</code>
 * This can be inlined as <code> a = caml_set_oo_id([248,"tag", caml_oo_last_id++])</code>
 * @type {number}
 */
var caml_oo_last_id = 0;
exports.caml_oo_last_id = caml_oo_last_id;
function caml_set_oo_id(b) {
    b[2] = caml_oo_last_id++;
    return b;
}
exports.caml_set_oo_id = caml_set_oo_id;
