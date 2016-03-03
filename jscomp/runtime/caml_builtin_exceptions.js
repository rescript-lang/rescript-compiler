// Generated CODE, PLEASE EDIT WITH CARE
'use strict';


var id = [0];

function caml_set_oo_id(b) {
  b[2] = id[0];
  id[0] += 1;
  return b;
}

function get_id() {
  id[0] += 1;
  return id[0];
}

var out_of_memory = /* tuple */[
  248,
  "Out_of_memory",
  0
];

var sys_error = /* tuple */[
  248,
  "Sys_error",
  -1
];

var failure = /* tuple */[
  248,
  "Failure",
  -2
];

var invalid_argument = /* tuple */[
  248,
  "Invalid_argument",
  -3
];

var end_of_file = /* tuple */[
  248,
  "End_of_file",
  -4
];

var division_by_zero = /* tuple */[
  248,
  "Division_by_zero",
  -5
];

var not_found = /* tuple */[
  248,
  "Not_found",
  -6
];

var match_failure = /* tuple */[
  248,
  "Match_failure",
  -7
];

var stack_overflow = /* tuple */[
  248,
  "Stack_overflow",
  -8
];

var sys_blocked_io = /* tuple */[
  248,
  "Sys_blocked_io",
  -9
];

var assert_failure = /* tuple */[
  248,
  "Assert_failure",
  -10
];

var undefined_recursive_module = /* tuple */[
  248,
  "Undefined_recursive_module",
  -11
];

exports.out_of_memory              = out_of_memory;
exports.sys_error                  = sys_error;
exports.failure                    = failure;
exports.invalid_argument           = invalid_argument;
exports.end_of_file                = end_of_file;
exports.division_by_zero           = division_by_zero;
exports.not_found                  = not_found;
exports.match_failure              = match_failure;
exports.stack_overflow             = stack_overflow;
exports.sys_blocked_io             = sys_blocked_io;
exports.assert_failure             = assert_failure;
exports.undefined_recursive_module = undefined_recursive_module;
exports.caml_set_oo_id             = caml_set_oo_id;
exports.get_id                     = get_id;
/* No side effect */
