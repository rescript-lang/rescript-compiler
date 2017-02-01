'use strict';

import * as Caml_string             from "./caml_string";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions";

function to_buffer(buff, ofs, len, _, _$1) {
  if (ofs < 0 || len < 0 || ofs > (buff.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Marshal.to_buffer: substring out of bounds"
        ];
  }
  else {
    return function () {
              throw "caml_output_value_to_buffer not implemented by bucklescript yet\n";
            }();
  }
}

function data_size(buff, ofs) {
  if (ofs < 0 || ofs > (buff.length - 20 | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Marshal.data_size"
        ];
  }
  else {
    return function () {
              throw "caml_marshal_data_size not implemented by bucklescript yet\n";
            }();
  }
}

function total_size(buff, ofs) {
  return 20 + data_size(buff, ofs) | 0;
}

function from_bytes(buff, ofs) {
  if (ofs < 0 || ofs > (buff.length - 20 | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Marshal.from_bytes"
        ];
  }
  else {
    var len = function () {
        throw "caml_marshal_data_size not implemented by bucklescript yet\n";
      }();
    if (ofs > (buff.length - (20 + len | 0) | 0)) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Marshal.from_bytes"
          ];
    }
    else {
      return function () {
                throw "caml_input_value_from_string not implemented by bucklescript yet\n";
              }();
    }
  }
}

function from_string(buff, ofs) {
  return from_bytes(Caml_string.bytes_of_string(buff), ofs);
}

function to_channel(_, _$1, _$2) {
  return function () {
            throw "caml_output_value not implemented by bucklescript yet\n";
          }();
}

function from_channel() {
  return function () {
            throw "caml_input_value not implemented by bucklescript yet\n";
          }();
}

var header_size = 20;

export {
  to_channel   ,
  to_buffer    ,
  from_channel ,
  from_bytes   ,
  from_string  ,
  header_size  ,
  data_size    ,
  total_size   ,
  
}
/* No side effect */
