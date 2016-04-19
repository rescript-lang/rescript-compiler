// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

function caml_bswap16(x) {
  return ((x & 255) << 8) | ((x & 65280) >>> 8);
}

function caml_int32_bswap(x) {
  return ((x & 255) << 24) | ((x & 65280) << 8) | ((x & 16711680) >>> 8) | ((x & 4278190080) >>> 24);
}

function caml_sys_getcwd() {
  return "/";
}

function caml_convert_raw_backtrace_slot() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_convert_raw_backtrace_slot unimplemented"
      ];
}

var imul = ( Math.imul || function (x,y) {
  y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; 
}
);

function caml_string_get16(s, i) {
  return s.charCodeAt(i) + (s.charCodeAt(i + 1 | 0) << 8) | 0;
}

function caml_string_get32(s, i) {
  return ((s.charCodeAt(i) + (s.charCodeAt(i + 1 | 0) << 8) | 0) + (s.charCodeAt(i + 2 | 0) << 16) | 0) + (s.charCodeAt(i + 3 | 0) << 24) | 0;
}

var caml_nativeint_bswap = caml_int32_bswap;

exports.caml_sys_getcwd                 = caml_sys_getcwd;
exports.caml_bswap16                    = caml_bswap16;
exports.caml_int32_bswap                = caml_int32_bswap;
exports.caml_nativeint_bswap            = caml_nativeint_bswap;
exports.caml_convert_raw_backtrace_slot = caml_convert_raw_backtrace_slot;
exports.imul                            = imul;
exports.caml_string_get16               = caml_string_get16;
exports.caml_string_get32               = caml_string_get32;
/* imul Not a pure module */
