'use strict';


var out_of_memory = {
  ExceptionID: 0,
  Debug: "Out_of_memory"
};

var sys_error = {
  ExceptionID: -1,
  Debug: "Sys_error"
};

var failure = {
  ExceptionID: -2,
  Debug: "Failure"
};

var invalid_argument = {
  ExceptionID: -3,
  Debug: "Invalid_argument"
};

var end_of_file = {
  ExceptionID: -4,
  Debug: "End_of_file"
};

var division_by_zero = {
  ExceptionID: -5,
  Debug: "Division_by_zero"
};

var not_found = {
  ExceptionID: -6,
  Debug: "Not_found"
};

var match_failure = {
  ExceptionID: -7,
  Debug: "Match_failure"
};

var stack_overflow = {
  ExceptionID: -8,
  Debug: "Stack_overflow"
};

var sys_blocked_io = {
  ExceptionID: -9,
  Debug: "Sys_blocked_io"
};

var assert_failure = {
  ExceptionID: -10,
  Debug: "Assert_failure"
};

var undefined_recursive_module = {
  ExceptionID: -11,
  Debug: "Undefined_recursive_module"
};

exports.out_of_memory = out_of_memory;
exports.sys_error = sys_error;
exports.failure = failure;
exports.invalid_argument = invalid_argument;
exports.end_of_file = end_of_file;
exports.division_by_zero = division_by_zero;
exports.not_found = not_found;
exports.match_failure = match_failure;
exports.stack_overflow = stack_overflow;
exports.sys_blocked_io = sys_blocked_io;
exports.assert_failure = assert_failure;
exports.undefined_recursive_module = undefined_recursive_module;
/* No side effect */
