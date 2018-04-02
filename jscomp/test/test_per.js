'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_sys = require("../../lib/js/caml_sys.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_missing_polyfill = require("../../lib/js/caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");
var CamlinternalFormatBasics = require("../../lib/js/camlinternalFormatBasics.js");

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

var Exit = Caml_exceptions.create("Test_per.Exit");

function min(x, y) {
  if (Caml_obj.caml_lessequal(x, y)) {
    return x;
  } else {
    return y;
  }
}

function max(x, y) {
  if (Caml_obj.caml_greaterequal(x, y)) {
    return x;
  } else {
    return y;
  }
}

function abs(x) {
  if (x >= 0) {
    return x;
  } else {
    return -x | 0;
  }
}

function lnot(x) {
  return x ^ -1;
}

var min_int = -2147483648;

var infinity = Caml_int64.float_of_bits(/* int64 */[
      /* hi */2146435072,
      /* lo */0
    ]);

var neg_infinity = Caml_int64.float_of_bits(/* int64 */[
      /* hi */-1048576,
      /* lo */0
    ]);

var nan = Caml_int64.float_of_bits(/* int64 */[
      /* hi */2146435072,
      /* lo */1
    ]);

var max_float = Caml_int64.float_of_bits(/* int64 */[
      /* hi */2146435071,
      /* lo */4294967295
    ]);

var min_float = Caml_int64.float_of_bits(/* int64 */[
      /* hi */1048576,
      /* lo */0
    ]);

var epsilon_float = Caml_int64.float_of_bits(/* int64 */[
      /* hi */1018167296,
      /* lo */0
    ]);

function $caret(s1, s2) {
  var l1 = s1.length;
  var l2 = s2.length;
  var s = Caml_string.caml_create_string(l1 + l2 | 0);
  Caml_string.caml_blit_string(s1, 0, s, 0, l1);
  Caml_string.caml_blit_string(s2, 0, s, l1, l2);
  return s;
}

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "char_of_int"
        ];
  } else {
    return n;
  }
}

function string_of_bool(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function bool_of_string(param) {
  switch (param) {
    case "false" : 
        return false;
    case "true" : 
        return true;
    default:
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "bool_of_string"
          ];
  }
}

function string_of_int(n) {
  return "" + n;
}

function valid_float_lexem(s) {
  var l = s.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= l) {
      return $caret(s, ".");
    } else {
      var match = Caml_string.get(s, i);
      if (match >= 48) {
        if (match >= 58) {
          return s;
        } else {
          _i = i + 1 | 0;
          continue ;
        }
      } else if (match !== 45) {
        return s;
      } else {
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
  } else {
    return l2;
  }
}

var stdin = Caml_io.caml_ml_open_descriptor_in(0);

var stdout = Caml_io.caml_ml_open_descriptor_out(1);

var stderr = Caml_io.caml_ml_open_descriptor_out(2);

function open_out_gen(_, _$1, _$2) {
  return Caml_io.caml_ml_open_descriptor_out(Caml_missing_polyfill.not_implemented("caml_sys_open not implemented by bucklescript yet\n"));
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
    } else {
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
  } else {
    return Caml_io.caml_ml_output(oc, s, ofs, len);
  }
}

function output_substring(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "output_substring"
        ];
  } else {
    return Caml_io.caml_ml_output(oc, s, ofs, len);
  }
}

function output_value(_, _$1) {
  return Caml_missing_polyfill.not_implemented("caml_output_value not implemented by bucklescript yet\n");
}

function close_out(oc) {
  Caml_io.caml_ml_flush(oc);
  return Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
}

function close_out_noerr(oc) {
  try {
    Caml_io.caml_ml_flush(oc);
  }
  catch (exn){
    
  }
  try {
    return Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
  }
  catch (exn$1){
    return /* () */0;
  }
}

function open_in_gen(_, _$1, _$2) {
  return Caml_io.caml_ml_open_descriptor_in(Caml_missing_polyfill.not_implemented("caml_sys_open not implemented by bucklescript yet\n"));
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
  } else {
    return Caml_missing_polyfill.not_implemented("caml_ml_input not implemented by bucklescript yet\n");
  }
}

function unsafe_really_input(_, _$1, _ofs, _len) {
  while(true) {
    var len = _len;
    var ofs = _ofs;
    if (len <= 0) {
      return /* () */0;
    } else {
      var r = Caml_missing_polyfill.not_implemented("caml_ml_input not implemented by bucklescript yet\n");
      if (r === 0) {
        throw Caml_builtin_exceptions.end_of_file;
      } else {
        _len = len - r | 0;
        _ofs = ofs + r | 0;
        continue ;
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
  } else {
    return unsafe_really_input(ic, s, ofs, len);
  }
}

function really_input_string(ic, len) {
  var s = Caml_string.caml_create_string(len);
  really_input(ic, s, 0, len);
  return s;
}

function input_line(chan) {
  var build_result = function (buf, _pos, _param) {
    while(true) {
      var param = _param;
      var pos = _pos;
      if (param) {
        var hd = param[0];
        var len = hd.length;
        Caml_string.caml_blit_string(hd, 0, buf, pos - len | 0, len);
        _param = param[1];
        _pos = pos - len | 0;
        continue ;
      } else {
        return buf;
      }
    };
  };
  var _accu = /* [] */0;
  var _len = 0;
  while(true) {
    var len = _len;
    var accu = _accu;
    var n = Caml_missing_polyfill.not_implemented("caml_ml_input_scan_line not implemented by bucklescript yet\n");
    if (n === 0) {
      if (accu) {
        return build_result(Caml_string.caml_create_string(len), len, accu);
      } else {
        throw Caml_builtin_exceptions.end_of_file;
      }
    } else if (n > 0) {
      var res = Caml_string.caml_create_string(n - 1 | 0);
      Caml_missing_polyfill.not_implemented("caml_ml_input not implemented by bucklescript yet\n");
      Caml_io.caml_ml_input_char(chan);
      if (accu) {
        var len$1 = (len + n | 0) - 1 | 0;
        return build_result(Caml_string.caml_create_string(len$1), len$1, /* :: */[
                    res,
                    accu
                  ]);
      } else {
        return res;
      }
    } else {
      var beg = Caml_string.caml_create_string(-n | 0);
      Caml_missing_polyfill.not_implemented("caml_ml_input not implemented by bucklescript yet\n");
      _len = len - n | 0;
      _accu = /* :: */[
        beg,
        accu
      ];
      continue ;
    }
  };
}

function close_in_noerr() {
  try {
    return Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
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

function print_endline(s) {
  output_string(stdout, s);
  Caml_io.caml_ml_output_char(stdout, /* "\n" */10);
  return Caml_io.caml_ml_flush(stdout);
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

function prerr_endline(s) {
  output_string(stderr, s);
  Caml_io.caml_ml_output_char(stderr, /* "\n" */10);
  return Caml_io.caml_ml_flush(stderr);
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

var LargeFile = /* module */[];

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
  exit_function[0] = (function () {
      Curry._1(f, /* () */0);
      return Curry._1(g, /* () */0);
    });
  return /* () */0;
}

function do_at_exit() {
  return Curry._1(exit_function[0], /* () */0);
}

function exit(retcode) {
  Curry._1(exit_function[0], /* () */0);
  return Caml_sys.caml_sys_exit(retcode);
}

var max_int = 2147483647;

exports.failwith = failwith;
exports.invalid_arg = invalid_arg;
exports.Exit = Exit;
exports.min = min;
exports.max = max;
exports.abs = abs;
exports.lnot = lnot;
exports.max_int = max_int;
exports.min_int = min_int;
exports.infinity = infinity;
exports.neg_infinity = neg_infinity;
exports.nan = nan;
exports.max_float = max_float;
exports.min_float = min_float;
exports.epsilon_float = epsilon_float;
exports.$caret = $caret;
exports.char_of_int = char_of_int;
exports.string_of_bool = string_of_bool;
exports.bool_of_string = bool_of_string;
exports.string_of_int = string_of_int;
exports.valid_float_lexem = valid_float_lexem;
exports.string_of_float = string_of_float;
exports.$at = $at;
exports.stdin = stdin;
exports.stdout = stdout;
exports.stderr = stderr;
exports.open_out_gen = open_out_gen;
exports.open_out = open_out;
exports.open_out_bin = open_out_bin;
exports.flush_all = flush_all;
exports.output_bytes = output_bytes;
exports.output_string = output_string;
exports.output = output;
exports.output_substring = output_substring;
exports.output_value = output_value;
exports.close_out = close_out;
exports.close_out_noerr = close_out_noerr;
exports.open_in_gen = open_in_gen;
exports.open_in = open_in;
exports.open_in_bin = open_in_bin;
exports.input = input;
exports.unsafe_really_input = unsafe_really_input;
exports.really_input = really_input;
exports.really_input_string = really_input_string;
exports.input_line = input_line;
exports.close_in_noerr = close_in_noerr;
exports.print_char = print_char;
exports.print_string = print_string;
exports.print_bytes = print_bytes;
exports.print_int = print_int;
exports.print_float = print_float;
exports.print_endline = print_endline;
exports.print_newline = print_newline;
exports.prerr_char = prerr_char;
exports.prerr_string = prerr_string;
exports.prerr_bytes = prerr_bytes;
exports.prerr_int = prerr_int;
exports.prerr_float = prerr_float;
exports.prerr_endline = prerr_endline;
exports.prerr_newline = prerr_newline;
exports.read_line = read_line;
exports.read_int = read_int;
exports.read_float = read_float;
exports.LargeFile = LargeFile;
exports.string_of_format = string_of_format;
exports.$caret$caret = $caret$caret;
exports.exit_function = exit_function;
exports.at_exit = at_exit;
exports.do_at_exit = do_at_exit;
exports.exit = exit;
/* No side effect */
