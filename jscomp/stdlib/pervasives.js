// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_io                  = require("../runtime/caml_io");
var Caml_exceptions          = require("../runtime/caml_exceptions");
var Caml_format              = require("../runtime/caml_format");
var Caml_primitive           = require("../runtime/caml_primitive");
var CamlinternalFormatBasics = require("./camlinternalFormatBasics");
var Caml_string              = require("../runtime/caml_string");

function failwith(s) {
  throw [
        0,
        Caml_exceptions.Failure,
        s
      ];
}

function invalid_arg(s) {
  throw [
        0,
        Caml_exceptions.Invalid_argument,
        s
      ];
}

var Exit = [
  248,
  "Pervasives.Exit",
  ++ Caml_exceptions.caml_oo_last_id
];

function min(x, y) {
  if (Caml_primitive.caml_lessequal(x, y)) {
    return x;
  }
  else {
    return y;
  }
}

function max(x, y) {
  if (Caml_primitive.caml_greaterequal(x, y)) {
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
    return -x;
  }
}

function lnot(x) {
  return x ^ -1;
}

var max_int = 2147483647;

var min_int = max_int + 1;

var infinity = Infinity;

var neg_infinity = -Infinity;

var nan = NaN;

var max_float = Number.MAX_VALUE;

var min_float = Number.MIN_VALUE;

var epsilon_float = Number.EPSILON;

var $caret = Caml_string.add;

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw [
          0,
          Caml_exceptions.Invalid_argument,
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
            0,
            Caml_exceptions.Invalid_argument,
            "bool_of_string"
          ];
  }
}

var string_of_int = Caml_primitive.string_of_int;

function valid_float_lexem(s) {
  var l = s.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= l) {
      return $caret(s, ".");
    }
    else {
      var match = s.charCodeAt(i);
      if (match >= 48) {
        if (match >= 58) {
          return s;
        }
        else {
          _i = i + 1;
        }
      }
      else if (match !== 45) {
        return s;
      }
      else {
        _i = i + 1;
      }
    }
  };
}

function string_of_float(f) {
  return valid_float_lexem(Caml_format.caml_format_float("%.12g", f));
}

function $at(l1, l2) {
  if (l1) {
    return [
            /* :: */0,
            l1[1],
            $at(l1[2], l2)
          ];
  }
  else {
    return l2;
  }
}

var stdin = Caml_io.stdin;

var stdout = Caml_io.stdout;

var stderr = Caml_io.stderr;

function open_out_gen(mode, perm, name) {
  return Caml_io.caml_ml_open_descriptor_out(Caml_primitive.caml_sys_open(name, mode, perm));
}

function open_out(name) {
  return open_out_gen([
              /* :: */0,
              /* Open_wronly */1,
              [
                /* :: */0,
                /* Open_creat */3,
                [
                  /* :: */0,
                  /* Open_trunc */4,
                  [
                    /* :: */0,
                    /* Open_text */7,
                    /* [] */0
                  ]
                ]
              ]
            ], 438, name);
}

function open_out_bin(name) {
  return open_out_gen([
              /* :: */0,
              /* Open_wronly */1,
              [
                /* :: */0,
                /* Open_creat */3,
                [
                  /* :: */0,
                  /* Open_trunc */4,
                  [
                    /* :: */0,
                    /* Open_binary */6,
                    /* [] */0
                  ]
                ]
              ]
            ], 438, name);
}

function flush_all() {
  var _param = Caml_primitive.caml_ml_out_channels_list(/* () */0);
  while(true) {
    var param = _param;
    if (param) {
      try {
        Caml_primitive.caml_ml_flush(param[1]);
      }
      catch (exn){
        
      }
      _param = param[2];
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
  if (ofs < 0 || len < 0 || ofs > s.length - len) {
    throw [
          0,
          Caml_exceptions.Invalid_argument,
          "output"
        ];
  }
  else {
    return Caml_io.caml_ml_output(oc, s, ofs, len);
  }
}

function output_substring(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > s.length - len) {
    throw [
          0,
          Caml_exceptions.Invalid_argument,
          "output_substring"
        ];
  }
  else {
    return Caml_io.caml_ml_output(oc, s, ofs, len);
  }
}

function output_value(chan, v) {
  return Caml_primitive.caml_output_value(chan, v, /* [] */0);
}

function close_out(oc) {
  Caml_primitive.caml_ml_flush(oc);
  return Caml_primitive.caml_ml_close_channel(oc);
}

function close_out_noerr(oc) {
  try {
    Caml_primitive.caml_ml_flush(oc);
  }
  catch (exn){
    
  }
  try {
    return Caml_primitive.caml_ml_close_channel(oc);
  }
  catch (exn$1){
    return /* () */0;
  }
}

function open_in_gen(mode, perm, name) {
  return Caml_io.caml_ml_open_descriptor_in(Caml_primitive.caml_sys_open(name, mode, perm));
}

function open_in(name) {
  return open_in_gen([
              /* :: */0,
              /* Open_rdonly */0,
              [
                /* :: */0,
                /* Open_text */7,
                /* [] */0
              ]
            ], 0, name);
}

function open_in_bin(name) {
  return open_in_gen([
              /* :: */0,
              /* Open_rdonly */0,
              [
                /* :: */0,
                /* Open_binary */6,
                /* [] */0
              ]
            ], 0, name);
}

function input(ic, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > s.length - len) {
    throw [
          0,
          Caml_exceptions.Invalid_argument,
          "input"
        ];
  }
  else {
    return Caml_primitive.caml_ml_input(ic, s, ofs, len);
  }
}

function unsafe_really_input(ic, s, _ofs, _len) {
  while(true) {
    var len = _len;
    var ofs = _ofs;
    if (len <= 0) {
      return /* () */0;
    }
    else {
      var r = Caml_primitive.caml_ml_input(ic, s, ofs, len);
      if (r) {
        _len = len - r;
        _ofs = ofs + r;
      }
      else {
        throw Caml_exceptions.End_of_file;
      }
    }
  };
}

function really_input(ic, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > s.length - len) {
    throw [
          0,
          Caml_exceptions.Invalid_argument,
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
        var hd = param[1];
        var len = hd.length;
        Caml_string.caml_blit_bytes(hd, 0, buf, pos - len, len);
        _param = param[2];
        _pos = pos - len;
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
      var n = Caml_primitive.caml_ml_input_scan_line(chan);
      if (n) {
        if (n > 0) {
          var res = Caml_string.caml_create_string(n - 1);
          Caml_primitive.caml_ml_input(chan, res, 0, n - 1);
          Caml_io.caml_ml_input_char(chan);
          if (accu) {
            var len$1 = len + n - 1;
            return build_result(Caml_string.caml_create_string(len$1), len$1, [
                        /* :: */0,
                        res,
                        accu
                      ]);
          }
          else {
            return res;
          }
        }
        else {
          var beg = Caml_string.caml_create_string(-n);
          Caml_primitive.caml_ml_input(chan, beg, 0, -n);
          _len = len - n;
          _accu = [
            /* :: */0,
            beg,
            accu
          ];
        }
      }
      else if (accu) {
        return build_result(Caml_string.caml_create_string(len), len, accu);
      }
      else {
        throw Caml_exceptions.End_of_file;
      }
    };
  };
  return Caml_string.bytes_to_string(scan(/* [] */0, 0));
}

function close_in_noerr(ic) {
  try {
    return Caml_primitive.caml_ml_close_channel(ic);
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
  return output_string(stdout, Caml_format.caml_format_int("%d", i));
}

function print_float(f) {
  return output_string(stdout, valid_float_lexem(Caml_format.caml_format_float("%.12g", f)));
}

var print_endline = console.log;

function print_newline() {
  Caml_io.caml_ml_output_char(stdout, /* "\n" */10);
  return Caml_primitive.caml_ml_flush(stdout);
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
  return output_string(stderr, Caml_format.caml_format_int("%d", i));
}

function prerr_float(f) {
  return output_string(stderr, valid_float_lexem(Caml_format.caml_format_float("%.12g", f)));
}

var prerr_endline = console.error;

function prerr_newline() {
  Caml_io.caml_ml_output_char(stderr, /* "\n" */10);
  return Caml_primitive.caml_ml_flush(stderr);
}

function read_line() {
  Caml_primitive.caml_ml_flush(stdout);
  return input_line(stdin);
}

function read_int() {
  return Caml_format.caml_int_of_string((Caml_primitive.caml_ml_flush(stdout), input_line(stdin)));
}

function read_float() {
  return Caml_format.caml_float_of_string((Caml_primitive.caml_ml_flush(stdout), input_line(stdin)));
}

function string_of_format(param) {
  return param[2];
}

function $caret$caret(param, param$1) {
  return [
          /* Format */0,
          CamlinternalFormatBasics.concat_fmt(param[1], param$1[1]),
          $caret(param[2], $caret("%,", param$1[2]))
        ];
}

var exit_function = [
  0,
  flush_all
];

function at_exit(f) {
  var g = exit_function[1];
  exit_function[1] = function () {
    f(/* () */0);
    return g(/* () */0);
  };
  return /* () */0;
}

function do_at_exit() {
  return exit_function[1](/* () */0);
}

function exit(retcode) {
  do_at_exit(/* () */0);
  return Caml_primitive.caml_sys_exit(retcode);
}

function flush(prim) {
  return Caml_primitive.caml_ml_flush(prim);
}

function output_char(prim, prim$1) {
  return Caml_io.caml_ml_output_char(prim, prim$1);
}

function output_byte(prim, prim$1) {
  return Caml_io.caml_ml_output_char(prim, prim$1);
}

function output_binary_int(prim, prim$1) {
  return Caml_primitive.caml_ml_output_int(prim, prim$1);
}

function seek_out(prim, prim$1) {
  return Caml_primitive.caml_ml_seek_out(prim, prim$1);
}

function pos_out(prim) {
  return Caml_primitive.caml_ml_pos_out(prim);
}

function out_channel_length(prim) {
  return Caml_primitive.caml_ml_channel_size(prim);
}

function set_binary_mode_out(prim, prim$1) {
  return Caml_primitive.caml_ml_set_binary_mode(prim, prim$1);
}

function input_char(prim) {
  return Caml_io.caml_ml_input_char(prim);
}

function input_byte(prim) {
  return Caml_io.caml_ml_input_char(prim);
}

function input_binary_int(prim) {
  return Caml_primitive.caml_ml_input_int(prim);
}

function input_value(prim) {
  return Caml_primitive.caml_input_value(prim);
}

function seek_in(prim, prim$1) {
  return Caml_primitive.caml_ml_seek_in(prim, prim$1);
}

function pos_in(prim) {
  return Caml_primitive.caml_ml_pos_in(prim);
}

function in_channel_length(prim) {
  return Caml_primitive.caml_ml_channel_size(prim);
}

function close_in(prim) {
  return Caml_primitive.caml_ml_close_channel(prim);
}

function set_binary_mode_in(prim, prim$1) {
  return Caml_primitive.caml_ml_set_binary_mode(prim, prim$1);
}

function LargeFile_001(prim, prim$1) {
  return Caml_primitive.caml_ml_seek_out_64(prim, prim$1);
}

function LargeFile_002(prim) {
  return Caml_primitive.caml_ml_pos_out_64(prim);
}

function LargeFile_003(prim) {
  return Caml_primitive.caml_ml_channel_size_64(prim);
}

function LargeFile_004(prim, prim$1) {
  return Caml_primitive.caml_ml_seek_in_64(prim, prim$1);
}

function LargeFile_005(prim) {
  return Caml_primitive.caml_ml_pos_in_64(prim);
}

function LargeFile_006(prim) {
  return Caml_primitive.caml_ml_channel_size_64(prim);
}

var LargeFile = [
  0,
  LargeFile_001,
  LargeFile_002,
  LargeFile_003,
  LargeFile_004,
  LargeFile_005,
  LargeFile_006
];

exports.invalid_arg         = invalid_arg;
exports.failwith            = failwith;
exports.Exit                = Exit;
exports.min                 = min;
exports.max                 = max;
exports.abs                 = abs;
exports.max_int             = max_int;
exports.min_int             = min_int;
exports.lnot                = lnot;
exports.infinity            = infinity;
exports.neg_infinity        = neg_infinity;
exports.nan                 = nan;
exports.max_float           = max_float;
exports.min_float           = min_float;
exports.epsilon_float       = epsilon_float;
exports.$caret              = $caret;
exports.char_of_int         = char_of_int;
exports.string_of_bool      = string_of_bool;
exports.bool_of_string      = bool_of_string;
exports.string_of_int       = string_of_int;
exports.string_of_float     = string_of_float;
exports.$at                 = $at;
exports.stdin               = stdin;
exports.stdout              = stdout;
exports.stderr              = stderr;
exports.print_char          = print_char;
exports.print_string        = print_string;
exports.print_bytes         = print_bytes;
exports.print_int           = print_int;
exports.print_float         = print_float;
exports.print_endline       = print_endline;
exports.print_newline       = print_newline;
exports.prerr_char          = prerr_char;
exports.prerr_string        = prerr_string;
exports.prerr_bytes         = prerr_bytes;
exports.prerr_int           = prerr_int;
exports.prerr_float         = prerr_float;
exports.prerr_endline       = prerr_endline;
exports.prerr_newline       = prerr_newline;
exports.read_line           = read_line;
exports.read_int            = read_int;
exports.read_float          = read_float;
exports.open_out            = open_out;
exports.open_out_bin        = open_out_bin;
exports.open_out_gen        = open_out_gen;
exports.flush               = flush;
exports.flush_all           = flush_all;
exports.output_char         = output_char;
exports.output_string       = output_string;
exports.output_bytes        = output_bytes;
exports.output              = output;
exports.output_substring    = output_substring;
exports.output_byte         = output_byte;
exports.output_binary_int   = output_binary_int;
exports.output_value        = output_value;
exports.seek_out            = seek_out;
exports.pos_out             = pos_out;
exports.out_channel_length  = out_channel_length;
exports.close_out           = close_out;
exports.close_out_noerr     = close_out_noerr;
exports.set_binary_mode_out = set_binary_mode_out;
exports.open_in             = open_in;
exports.open_in_bin         = open_in_bin;
exports.open_in_gen         = open_in_gen;
exports.input_char          = input_char;
exports.input_line          = input_line;
exports.input               = input;
exports.really_input        = really_input;
exports.really_input_string = really_input_string;
exports.input_byte          = input_byte;
exports.input_binary_int    = input_binary_int;
exports.input_value         = input_value;
exports.seek_in             = seek_in;
exports.pos_in              = pos_in;
exports.in_channel_length   = in_channel_length;
exports.close_in            = close_in;
exports.close_in_noerr      = close_in_noerr;
exports.set_binary_mode_in  = set_binary_mode_in;
exports.LargeFile           = LargeFile;
exports.string_of_format    = string_of_format;
exports.$caret$caret        = $caret$caret;
exports.exit                = exit;
exports.at_exit             = at_exit;
exports.valid_float_lexem   = valid_float_lexem;
exports.unsafe_really_input = unsafe_really_input;
exports.do_at_exit          = do_at_exit;
/* No side effect */
