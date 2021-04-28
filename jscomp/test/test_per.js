'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_sys = require("../../lib/js/caml_sys.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");

function failwith(s) {
  throw {
        RE_EXN_ID: "Failure",
        _1: s,
        Error: new Error()
      };
}

function invalid_arg(s) {
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: s,
        Error: new Error()
      };
}

var Exit = /* @__PURE__ */Caml_exceptions.create("Test_per.Exit");

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

var infinity = Caml_int64.float_of_bits([
      2146435072,
      0
    ]);

var neg_infinity = Caml_int64.float_of_bits([
      -1048576,
      0
    ]);

var nan = Caml_int64.float_of_bits([
      2146435072,
      1
    ]);

var max_float = Caml_int64.float_of_bits([
      2146435071,
      4294967295
    ]);

var min_float = Caml_int64.float_of_bits([
      1048576,
      0
    ]);

var epsilon_float = Caml_int64.float_of_bits([
      1018167296,
      0
    ]);

function $caret(s1, s2) {
  var l1 = s1.length;
  var l2 = s2.length;
  var s = Caml_bytes.caml_create_bytes(l1 + l2 | 0);
  Caml_bytes.caml_blit_string(s1, 0, s, 0, l1);
  Caml_bytes.caml_blit_string(s2, 0, s, l1, l2);
  return s;
}

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "char_of_int",
          Error: new Error()
        };
  }
  return n;
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
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "bool_of_string",
            Error: new Error()
          };
  }
}

function string_of_int(n) {
  return Caml_format.caml_format_int("%d", n);
}

function valid_float_lexem(s) {
  var l = s.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= l) {
      return $caret(s, ".");
    }
    var match = Caml_string.get(s, i);
    if (match >= 48) {
      if (match >= 58) {
        return s;
      }
      _i = i + 1 | 0;
      continue ;
    }
    if (match !== 45) {
      return s;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function string_of_float(f) {
  return valid_float_lexem(Caml_format.caml_format_float("%.12g", f));
}

function $at(l1, l2) {
  if (l1) {
    return {
            hd: l1.hd,
            tl: $at(l1.tl, l2)
          };
  } else {
    return l2;
  }
}

function open_out_gen(mode, perm, name) {
  return Caml_external_polyfill.resolve("caml_ml_open_descriptor_out")(Caml_external_polyfill.resolve("caml_sys_open")(name, mode, perm));
}

function open_out(name) {
  return open_out_gen({
              hd: /* Open_wronly */1,
              tl: {
                hd: /* Open_creat */3,
                tl: {
                  hd: /* Open_trunc */4,
                  tl: {
                    hd: /* Open_text */7,
                    tl: /* [] */0
                  }
                }
              }
            }, 438, name);
}

function open_out_bin(name) {
  return open_out_gen({
              hd: /* Open_wronly */1,
              tl: {
                hd: /* Open_creat */3,
                tl: {
                  hd: /* Open_trunc */4,
                  tl: {
                    hd: /* Open_binary */6,
                    tl: /* [] */0
                  }
                }
              }
            }, 438, name);
}

function flush_all(param) {
  var _param = Caml_external_polyfill.resolve("caml_ml_out_channels_list")(undefined);
  while(true) {
    var param$1 = _param;
    if (!param$1) {
      return ;
    }
    try {
      Caml_external_polyfill.resolve("caml_ml_flush")(param$1.hd);
    }
    catch (exn){
      
    }
    _param = param$1.tl;
    continue ;
  };
}

function output_bytes(oc, s) {
  return Caml_external_polyfill.resolve("caml_ml_output")(oc, s, 0, s.length);
}

function output_string(oc, s) {
  return Caml_external_polyfill.resolve("caml_ml_output")(oc, s, 0, s.length);
}

function output(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "output",
          Error: new Error()
        };
  }
  return Caml_external_polyfill.resolve("caml_ml_output")(oc, s, ofs, len);
}

function output_substring(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "output_substring",
          Error: new Error()
        };
  }
  return Caml_external_polyfill.resolve("caml_ml_output")(oc, s, ofs, len);
}

function output_value(chan, v) {
  return Caml_external_polyfill.resolve("caml_output_value")(chan, v, /* [] */0);
}

function close_out(oc) {
  Caml_external_polyfill.resolve("caml_ml_flush")(oc);
  return Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
}

function close_out_noerr(oc) {
  try {
    Caml_external_polyfill.resolve("caml_ml_flush")(oc);
  }
  catch (exn){
    
  }
  try {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
  }
  catch (exn$1){
    return ;
  }
}

function open_in_gen(mode, perm, name) {
  return Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(Caml_external_polyfill.resolve("caml_sys_open")(name, mode, perm));
}

function open_in(name) {
  return open_in_gen({
              hd: /* Open_rdonly */0,
              tl: {
                hd: /* Open_text */7,
                tl: /* [] */0
              }
            }, 0, name);
}

function open_in_bin(name) {
  return open_in_gen({
              hd: /* Open_rdonly */0,
              tl: {
                hd: /* Open_binary */6,
                tl: /* [] */0
              }
            }, 0, name);
}

function input(ic, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "input",
          Error: new Error()
        };
  }
  return Caml_external_polyfill.resolve("caml_ml_input")(ic, s, ofs, len);
}

function unsafe_really_input(ic, s, _ofs, _len) {
  while(true) {
    var len = _len;
    var ofs = _ofs;
    if (len <= 0) {
      return ;
    }
    var r = Caml_external_polyfill.resolve("caml_ml_input")(ic, s, ofs, len);
    if (r === 0) {
      throw {
            RE_EXN_ID: "End_of_file",
            Error: new Error()
          };
    }
    _len = len - r | 0;
    _ofs = ofs + r | 0;
    continue ;
  };
}

function really_input(ic, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "really_input",
          Error: new Error()
        };
  }
  return unsafe_really_input(ic, s, ofs, len);
}

function really_input_string(ic, len) {
  var s = Caml_bytes.caml_create_bytes(len);
  really_input(ic, s, 0, len);
  return s;
}

function input_line(chan) {
  var build_result = function (buf, _pos, _param) {
    while(true) {
      var param = _param;
      var pos = _pos;
      if (!param) {
        return buf;
      }
      var hd = param.hd;
      var len = hd.length;
      Caml_bytes.caml_blit_string(hd, 0, buf, pos - len | 0, len);
      _param = param.tl;
      _pos = pos - len | 0;
      continue ;
    };
  };
  var _accu = /* [] */0;
  var _len = 0;
  while(true) {
    var len = _len;
    var accu = _accu;
    var n = Caml_external_polyfill.resolve("caml_ml_input_scan_line")(chan);
    if (n === 0) {
      if (accu) {
        return build_result(Caml_bytes.caml_create_bytes(len), len, accu);
      }
      throw {
            RE_EXN_ID: "End_of_file",
            Error: new Error()
          };
    }
    if (n > 0) {
      var res = Caml_bytes.caml_create_bytes(n - 1 | 0);
      Caml_external_polyfill.resolve("caml_ml_input")(chan, res, 0, n - 1 | 0);
      Caml_external_polyfill.resolve("caml_ml_input_char")(chan);
      if (!accu) {
        return res;
      }
      var len$1 = (len + n | 0) - 1 | 0;
      return build_result(Caml_bytes.caml_create_bytes(len$1), len$1, {
                  hd: res,
                  tl: accu
                });
    }
    var beg = Caml_bytes.caml_create_bytes(-n | 0);
    Caml_external_polyfill.resolve("caml_ml_input")(chan, beg, 0, -n | 0);
    _len = len - n | 0;
    _accu = {
      hd: beg,
      tl: accu
    };
    continue ;
  };
}

function close_in_noerr(ic) {
  try {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(ic);
  }
  catch (exn){
    return ;
  }
}

var LargeFile = {};

var exit_function = {
  contents: flush_all
};

function at_exit(f) {
  var g = exit_function.contents;
  exit_function.contents = (function (param) {
      Curry._1(f, undefined);
      return Curry._1(g, undefined);
    });
  
}

function do_at_exit(param) {
  return Curry._1(exit_function.contents, undefined);
}

function exit(retcode) {
  Curry._1(exit_function.contents, undefined);
  return Caml_sys.caml_sys_exit(retcode);
}

Caml_external_polyfill.resolve("caml_register_named_value")("Pervasives.do_at_exit", do_at_exit);

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
exports.LargeFile = LargeFile;
exports.exit_function = exit_function;
exports.at_exit = at_exit;
exports.do_at_exit = do_at_exit;
exports.exit = exit;
/* No side effect */
