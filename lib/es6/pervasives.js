'use strict';

import * as Curry                    from "./curry";
import * as Caml_io                  from "./caml_io";
import * as Caml_obj                 from "./caml_obj";
import * as Caml_format              from "./caml_format";
import * as Caml_string              from "./caml_string";
import * as Caml_exceptions          from "./caml_exceptions";
import * as Caml_builtin_exceptions  from "./caml_builtin_exceptions";
import * as CamlinternalFormatBasics from "./camlinternalFormatBasics";

function failwith(s) {
  throw [
        Caml_builtin_exceptions.failure,
        s
      ];
}

function invalid_arg(s) {
  throw [
        Caml_builtin_exceptions.invalid_argument,
        s
      ];
}

var Exit = Caml_exceptions.create("Pervasives.Exit");

function min(x, y) {
  if (Caml_obj.caml_lessequal(x, y)) {
    return x;
  }
  else {
    return y;
  }
}

function max(x, y) {
  if (Caml_obj.caml_greaterequal(x, y)) {
    return x;
  }
  else {
    return y;
  }
}

function abs(x) {
  if (x >= 0) {
    return x;
  }
  else {
    return -x | 0;
  }
}

function lnot(x) {
  return x ^ -1;
}

var min_int = -2147483648;

function $caret(a, b) {
  return a + b;
}

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "char_of_int"
        ];
  }
  else {
    return n;
  }
}

function string_of_bool(b) {
  if (b) {
    return "true";
  }
  else {
    return "false";
  }
}

function bool_of_string(param) {
  switch (param) {
    case "false" : 
        return /* false */0;
    case "true" : 
        return /* true */1;
    default:
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "bool_of_string"
          ];
  }
}

function string_of_int(param) {
  return "" + param;
}

function valid_float_lexem(s) {
  var l = s.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= l) {
      return $caret(s, ".");
    }
    else {
      var match = Caml_string.get(s, i);
      if (match >= 48) {
        if (match >= 58) {
          return s;
        }
        else {
          _i = i + 1 | 0;
          continue ;
          
        }
      }
      else if (match !== 45) {
        return s;
      }
      else {
        _i = i + 1 | 0;
        continue ;
        
      }
    }
  };
}

function string_of_float(f) {
  return valid_float_lexem(Caml_format.caml_format_float("%.12g", f));
}

function $at(l1, l2) {
  if (l1) {
    return /* :: */[
            l1[0],
            $at(l1[1], l2)
          ];
  }
  else {
    return l2;
  }
}

var stdin = Caml_io.stdin;

var stdout = Caml_io.stdout;

var stderr = Caml_io.stderr;

function open_out_gen(_, _$1, _$2) {
  return Caml_io.caml_ml_open_descriptor_out(function () {
                throw "caml_sys_open not implemented by bucklescript yet\n";
              }());
}

function open_out(name) {
  return open_out_gen(/* :: */[
              /* Open_wronly */1,
              /* :: */[
                /* Open_creat */3,
                /* :: */[
                  /* Open_trunc */4,
                  /* :: */[
                    /* Open_text */7,
                    /* [] */0
                  ]
                ]
              ]
            ], 438, name);
}

function open_out_bin(name) {
  return open_out_gen(/* :: */[
              /* Open_wronly */1,
              /* :: */[
                /* Open_creat */3,
                /* :: */[
                  /* Open_trunc */4,
                  /* :: */[
                    /* Open_binary */6,
                    /* [] */0
                  ]
                ]
              ]
            ], 438, name);
}

function flush_all() {
  var _param = Caml_io.caml_ml_out_channels_list(/* () */0);
  while(true) {
    var param = _param;
    if (param) {
      try {
        Caml_io.caml_ml_flush(param[0]);
      }
      catch (exn){
        
      }
      _param = param[1];
      continue ;
      
    }
    else {
      return /* () */0;
    }
  };
}

function output_bytes(oc, s) {
  return Caml_io.caml_ml_output(oc, s, 0, s.length);
}

function output_string(oc, s) {
  return Caml_io.caml_ml_output(oc, s, 0, s.length);
}

function output(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "output"
        ];
  }
  else {
    return Caml_io.caml_ml_output(oc, s, ofs, len);
  }
}

function output_substring(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "output_substring"
        ];
  }
  else {
    return Caml_io.caml_ml_output(oc, s, ofs, len);
  }
}

function output_value(_, _$1) {
  return function () {
            throw "caml_output_value not implemented by bucklescript yet\n";
          }();
}

function close_out(oc) {
  Caml_io.caml_ml_flush(oc);
  return function () {
            throw "caml_ml_close_channel not implemented by bucklescript yet\n";
          }();
}

function close_out_noerr(oc) {
  try {
    Caml_io.caml_ml_flush(oc);
  }
  catch (exn){
    
  }
  try {
    return function () {
              throw "caml_ml_close_channel not implemented by bucklescript yet\n";
            }();
  }
  catch (exn$1){
    return /* () */0;
  }
}

function open_in_gen(_, _$1, _$2) {
  return Caml_io.caml_ml_open_descriptor_in(function () {
                throw "caml_sys_open not implemented by bucklescript yet\n";
              }());
}

function open_in(name) {
  return open_in_gen(/* :: */[
              /* Open_rdonly */0,
              /* :: */[
                /* Open_text */7,
                /* [] */0
              ]
            ], 0, name);
}

function open_in_bin(name) {
  return open_in_gen(/* :: */[
              /* Open_rdonly */0,
              /* :: */[
                /* Open_binary */6,
                /* [] */0
              ]
            ], 0, name);
}

function input(_, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "input"
        ];
  }
  else {
    return function () {
              throw "caml_ml_input not implemented by bucklescript yet\n";
            }();
  }
}

function unsafe_really_input(_, _$1, _ofs, _len) {
  while(true) {
    var len = _len;
    var ofs = _ofs;
    if (len <= 0) {
      return /* () */0;
    }
    else {
      var r = function () {
          throw "caml_ml_input not implemented by bucklescript yet\n";
        }();
      if (r) {
        _len = len - r | 0;
        _ofs = ofs + r | 0;
        continue ;
        
      }
      else {
        throw Caml_builtin_exceptions.end_of_file;
      }
    }
  };
}

function really_input(ic, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "really_input"
        ];
  }
  else {
    return unsafe_really_input(ic, s, ofs, len);
  }
}

function really_input_string(ic, len) {
  var s = Caml_string.caml_create_string(len);
  really_input(ic, s, 0, len);
  return Caml_string.bytes_to_string(s);
}

function input_line(chan) {
  var build_result = function (buf, _pos, _param) {
    while(true) {
      var param = _param;
      var pos = _pos;
      if (param) {
        var hd = param[0];
        var len = hd.length;
        Caml_string.caml_blit_bytes(hd, 0, buf, pos - len | 0, len);
        _param = param[1];
        _pos = pos - len | 0;
        continue ;
        
      }
      else {
        return buf;
      }
    };
  };
  var scan = function (_accu, _len) {
    while(true) {
      var len = _len;
      var accu = _accu;
      var n = function () {
          throw "caml_ml_input_scan_line not implemented by bucklescript yet\n";
        }();
      if (n) {
        if (n > 0) {
          var res = Caml_string.caml_create_string(n - 1 | 0);
          (function () {
                throw "caml_ml_input not implemented by bucklescript yet\n";
              }());
          Caml_io.caml_ml_input_char(chan);
          if (accu) {
            var len$1 = (len + n | 0) - 1 | 0;
            return build_result(Caml_string.caml_create_string(len$1), len$1, /* :: */[
                        res,
                        accu
                      ]);
          }
          else {
            return res;
          }
        }
        else {
          var beg = Caml_string.caml_create_string(-n | 0);
          (function () {
                throw "caml_ml_input not implemented by bucklescript yet\n";
              }());
          _len = len - n | 0;
          _accu = /* :: */[
            beg,
            accu
          ];
          continue ;
          
        }
      }
      else if (accu) {
        return build_result(Caml_string.caml_create_string(len), len, accu);
      }
      else {
        throw Caml_builtin_exceptions.end_of_file;
      }
    };
  };
  return Caml_string.bytes_to_string(scan(/* [] */0, 0));
}

function close_in_noerr() {
  try {
    return function () {
              throw "caml_ml_close_channel not implemented by bucklescript yet\n";
            }();
  }
  catch (exn){
    return /* () */0;
  }
}

function print_char(c) {
  return Caml_io.caml_ml_output_char(stdout, c);
}

function print_string(s) {
  return output_string(stdout, s);
}

function print_bytes(s) {
  return output_bytes(stdout, s);
}

function print_int(i) {
  return output_string(stdout, "" + i);
}

function print_float(f) {
  return output_string(stdout, valid_float_lexem(Caml_format.caml_format_float("%.12g", f)));
}

function print_endline(param) {
  console.log(param);
  return 0;
}

function print_newline() {
  Caml_io.caml_ml_output_char(stdout, /* "\n" */10);
  return Caml_io.caml_ml_flush(stdout);
}

function prerr_char(c) {
  return Caml_io.caml_ml_output_char(stderr, c);
}

function prerr_string(s) {
  return output_string(stderr, s);
}

function prerr_bytes(s) {
  return output_bytes(stderr, s);
}

function prerr_int(i) {
  return output_string(stderr, "" + i);
}

function prerr_float(f) {
  return output_string(stderr, valid_float_lexem(Caml_format.caml_format_float("%.12g", f)));
}

function prerr_endline(param) {
  console.error(param);
  return 0;
}

function prerr_newline() {
  Caml_io.caml_ml_output_char(stderr, /* "\n" */10);
  return Caml_io.caml_ml_flush(stderr);
}

function read_line() {
  Caml_io.caml_ml_flush(stdout);
  return input_line(stdin);
}

function read_int() {
  return Caml_format.caml_int_of_string((Caml_io.caml_ml_flush(stdout), input_line(stdin)));
}

function read_float() {
  return Caml_format.caml_float_of_string((Caml_io.caml_ml_flush(stdout), input_line(stdin)));
}

function string_of_format(param) {
  return param[1];
}

function $caret$caret(param, param$1) {
  return /* Format */[
          CamlinternalFormatBasics.concat_fmt(param[0], param$1[0]),
          $caret(param[1], $caret("%,", param$1[1]))
        ];
}

var exit_function = [flush_all];

function at_exit(f) {
  var g = exit_function[0];
  exit_function[0] = function () {
    Curry._1(f, /* () */0);
    return Curry._1(g, /* () */0);
  };
  return /* () */0;
}

function do_at_exit() {
  return Curry._1(exit_function[0], /* () */0);
}

function exit() {
  do_at_exit(/* () */0);
  return function () {
            throw "caml_sys_exit not implemented by bucklescript yet\n";
          }();
}

var max_int = 2147483647;

var infinity = Infinity;

var neg_infinity = -Infinity;

var nan = NaN;

var max_float = Number.MAX_VALUE;

var min_float = Number.MIN_VALUE;

var epsilon_float = 2.220446049250313e-16;

var flush = Caml_io.caml_ml_flush;

var output_char = Caml_io.caml_ml_output_char;

var output_byte = Caml_io.caml_ml_output_char;

function output_binary_int(_, _$1) {
  return function () {
            throw "caml_ml_output_int not implemented by bucklescript yet\n";
          }();
}

function seek_out(_, _$1) {
  return function () {
            throw "caml_ml_seek_out not implemented by bucklescript yet\n";
          }();
}

function pos_out() {
  return function () {
            throw "caml_ml_pos_out not implemented by bucklescript yet\n";
          }();
}

function out_channel_length() {
  return function () {
            throw "caml_ml_channel_size not implemented by bucklescript yet\n";
          }();
}

function set_binary_mode_out(_, _$1) {
  return function () {
            throw "caml_ml_set_binary_mode not implemented by bucklescript yet\n";
          }();
}

var input_char = Caml_io.caml_ml_input_char;

var input_byte = Caml_io.caml_ml_input_char;

function input_binary_int() {
  return function () {
            throw "caml_ml_input_int not implemented by bucklescript yet\n";
          }();
}

function input_value() {
  return function () {
            throw "caml_input_value not implemented by bucklescript yet\n";
          }();
}

function seek_in(_, _$1) {
  return function () {
            throw "caml_ml_seek_in not implemented by bucklescript yet\n";
          }();
}

function pos_in() {
  return function () {
            throw "caml_ml_pos_in not implemented by bucklescript yet\n";
          }();
}

function in_channel_length() {
  return function () {
            throw "caml_ml_channel_size not implemented by bucklescript yet\n";
          }();
}

function close_in() {
  return function () {
            throw "caml_ml_close_channel not implemented by bucklescript yet\n";
          }();
}

function set_binary_mode_in(_, _$1) {
  return function () {
            throw "caml_ml_set_binary_mode not implemented by bucklescript yet\n";
          }();
}

function LargeFile_000(_, _$1) {
  return function () {
            throw "caml_ml_seek_out_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_001() {
  return function () {
            throw "caml_ml_pos_out_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_002() {
  return function () {
            throw "caml_ml_channel_size_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_003(_, _$1) {
  return function () {
            throw "caml_ml_seek_in_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_004() {
  return function () {
            throw "caml_ml_pos_in_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_005() {
  return function () {
            throw "caml_ml_channel_size_64 not implemented by bucklescript yet\n";
          }();
}

var LargeFile = [
  LargeFile_000,
  LargeFile_001,
  LargeFile_002,
  LargeFile_003,
  LargeFile_004,
  LargeFile_005
];

export {
  invalid_arg         ,
  failwith            ,
  Exit                ,
  min                 ,
  max                 ,
  abs                 ,
  max_int             ,
  min_int             ,
  lnot                ,
  infinity            ,
  neg_infinity        ,
  nan                 ,
  max_float           ,
  min_float           ,
  epsilon_float       ,
  $caret              ,
  char_of_int         ,
  string_of_bool      ,
  bool_of_string      ,
  string_of_int       ,
  string_of_float     ,
  $at                 ,
  stdin               ,
  stdout              ,
  stderr              ,
  print_char          ,
  print_string        ,
  print_bytes         ,
  print_int           ,
  print_float         ,
  print_endline       ,
  print_newline       ,
  prerr_char          ,
  prerr_string        ,
  prerr_bytes         ,
  prerr_int           ,
  prerr_float         ,
  prerr_endline       ,
  prerr_newline       ,
  read_line           ,
  read_int            ,
  read_float          ,
  open_out            ,
  open_out_bin        ,
  open_out_gen        ,
  flush               ,
  flush_all           ,
  output_char         ,
  output_string       ,
  output_bytes        ,
  output              ,
  output_substring    ,
  output_byte         ,
  output_binary_int   ,
  output_value        ,
  seek_out            ,
  pos_out             ,
  out_channel_length  ,
  close_out           ,
  close_out_noerr     ,
  set_binary_mode_out ,
  open_in             ,
  open_in_bin         ,
  open_in_gen         ,
  input_char          ,
  input_line          ,
  input               ,
  really_input        ,
  really_input_string ,
  input_byte          ,
  input_binary_int    ,
  input_value         ,
  seek_in             ,
  pos_in              ,
  in_channel_length   ,
  close_in            ,
  close_in_noerr      ,
  set_binary_mode_in  ,
  LargeFile           ,
  string_of_format    ,
  $caret$caret        ,
  exit                ,
  at_exit             ,
  valid_float_lexem   ,
  unsafe_really_input ,
  do_at_exit          ,
  
}
/* No side effect */
