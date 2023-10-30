'use strict';

var Sys = require("./sys.js");
var Caml = require("./caml.js");
var List = require("./list.js");
var $$Array = require("./array.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var $$String = require("./string.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_string = require("./caml_string.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");

var Bad = /* @__PURE__ */Caml_exceptions.create("Arg.Bad");

var Help = /* @__PURE__ */Caml_exceptions.create("Arg.Help");

var Stop = /* @__PURE__ */Caml_exceptions.create("Arg.Stop");

function assoc3(x, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var match = l.hd;
      if (Caml_obj.equal(match[0], x)) {
        return match[1];
      }
      _l = l.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function split(s) {
  var i = $$String.index(s, /* '=' */61);
  var len = s.length;
  return [
          $$String.sub(s, 0, i),
          $$String.sub(s, i + 1 | 0, len - (i + 1 | 0) | 0)
        ];
}

function make_symlist(prefix, sep, suffix, l) {
  if (l) {
    return List.fold_left((function (x, y) {
                  return x + (sep + y);
                }), prefix + l.hd, l.tl) + suffix;
  } else {
    return "<none>";
  }
}

function help_action(param) {
  throw {
        RE_EXN_ID: Stop,
        _1: {
          TAG: "Unknown",
          _0: "-help"
        },
        Error: new Error()
      };
}

function add_help(speclist) {
  var add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      add1 = {
        hd: [
          "-help",
          {
            TAG: "Unit",
            _0: help_action
          },
          " Display this list of options"
        ],
        tl: /* [] */0
      };
    } else {
      throw exn;
    }
  }
  var add2;
  try {
    assoc3("--help", speclist);
    add2 = /* [] */0;
  }
  catch (raw_exn$1){
    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
    if (exn$1.RE_EXN_ID === "Not_found") {
      add2 = {
        hd: [
          "--help",
          {
            TAG: "Unit",
            _0: help_action
          },
          " Display this list of options"
        ],
        tl: /* [] */0
      };
    } else {
      throw exn$1;
    }
  }
  return Pervasives.$at(speclist, Pervasives.$at(add1, add2));
}

function usage_b(buf, speclist, errmsg) {
  $$Buffer.add_string(buf, errmsg + "\n");
  List.iter((function (param) {
          var doc = param[2];
          if (doc.length === 0) {
            return ;
          }
          var spec = param[1];
          var key = param[0];
          if (spec.TAG !== "Symbol") {
            return $$Buffer.add_string(buf, "  " + key + " " + doc + "\n");
          }
          var sym = make_symlist("{", "|", "}", spec._0);
          return $$Buffer.add_string(buf, "  " + key + " " + sym + doc + "\n");
        }), add_help(speclist));
}

function usage_string(speclist, errmsg) {
  var b = $$Buffer.create(200);
  usage_b(b, speclist, errmsg);
  return $$Buffer.contents(b);
}

function usage(speclist, errmsg) {
  console.log(usage_string(speclist, errmsg));
}

var current = {
  contents: 0
};

function bool_of_string_opt(x) {
  try {
    return Pervasives.bool_of_string(x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Invalid_argument") {
      return ;
    }
    throw exn;
  }
}

function int_of_string_opt(x) {
  try {
    return Caml_format.int_of_string(x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return ;
    }
    throw exn;
  }
}

function float_of_string_opt(x) {
  try {
    return Caml_format.float_of_string(x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return ;
    }
    throw exn;
  }
}

function parse_and_expand_argv_dynamic_aux(allow_expand, current, argv, speclist, anonfun, errmsg) {
  var initpos = current.contents;
  var convert_error = function (error) {
    var b = $$Buffer.create(200);
    var progname = initpos < argv.contents.length ? Caml_array.get(argv.contents, initpos) : "(?)";
    switch (error.TAG) {
      case "Unknown" :
          var s = error._0;
          switch (s) {
            case "--help" :
            case "-help" :
                break;
            default:
              $$Buffer.add_string(b, progname + ": unknown option '" + s + "'.\n");
          }
          break;
      case "Wrong" :
          $$Buffer.add_string(b, progname + ": wrong argument '" + error._1 + "'; option '" + error._0 + "' expects " + error._2 + ".\n");
          break;
      case "Missing" :
          $$Buffer.add_string(b, progname + ": option '" + error._0 + "' needs an argument.\n");
          break;
      case "Message" :
          $$Buffer.add_string(b, progname + ": " + error._0 + ".\n");
          break;
      
    }
    usage_b(b, speclist.contents, errmsg);
    if (Caml_obj.equal(error, {
            TAG: "Unknown",
            _0: "-help"
          }) || Caml_obj.equal(error, {
            TAG: "Unknown",
            _0: "--help"
          })) {
      return {
              RE_EXN_ID: Help,
              _1: $$Buffer.contents(b)
            };
    } else {
      return {
              RE_EXN_ID: Bad,
              _1: $$Buffer.contents(b)
            };
    }
  };
  current.contents = current.contents + 1 | 0;
  while(current.contents < argv.contents.length) {
    try {
      var s = Caml_array.get(argv.contents, current.contents);
      if (s.length >= 1 && Caml_string.get(s, 0) === /* '-' */45) {
        var match;
        try {
          match = [
            assoc3(s, speclist.contents),
            undefined
          ];
        }
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn.RE_EXN_ID === "Not_found") {
            try {
              var match$1 = split(s);
              match = [
                assoc3(match$1[0], speclist.contents),
                match$1[1]
              ];
            }
            catch (raw_exn$1){
              var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
              if (exn$1.RE_EXN_ID === "Not_found") {
                throw {
                      RE_EXN_ID: Stop,
                      _1: {
                        TAG: "Unknown",
                        _0: s
                      },
                      Error: new Error()
                    };
              }
              throw exn$1;
            }
          } else {
            throw exn;
          }
        }
        var follow = match[1];
        var no_arg = (function(s,follow){
        return function no_arg(param) {
          if (follow === undefined) {
            return ;
          }
          throw {
                RE_EXN_ID: Stop,
                _1: {
                  TAG: "Wrong",
                  _0: s,
                  _1: follow,
                  _2: "no argument"
                },
                Error: new Error()
              };
        }
        }(s,follow));
        var get_arg = (function(s,follow){
        return function get_arg(param) {
          if (follow !== undefined) {
            return follow;
          }
          if ((current.contents + 1 | 0) < argv.contents.length) {
            return Caml_array.get(argv.contents, current.contents + 1 | 0);
          }
          throw {
                RE_EXN_ID: Stop,
                _1: {
                  TAG: "Missing",
                  _0: s
                },
                Error: new Error()
              };
        }
        }(s,follow));
        var consume_arg = (function(follow){
        return function consume_arg(param) {
          if (follow !== undefined) {
            return ;
          } else {
            current.contents = current.contents + 1 | 0;
            return ;
          }
        }
        }(follow));
        var treat_action = (function(s){
        return function treat_action(f) {
          switch (f.TAG) {
            case "Unit" :
                return Curry._1(f._0, undefined);
            case "Bool" :
                var arg = get_arg();
                var s$1 = bool_of_string_opt(arg);
                if (s$1 !== undefined) {
                  Curry._1(f._0, s$1);
                } else {
                  throw {
                        RE_EXN_ID: Stop,
                        _1: {
                          TAG: "Wrong",
                          _0: s,
                          _1: arg,
                          _2: "a boolean"
                        },
                        Error: new Error()
                      };
                }
                return consume_arg();
            case "Set" :
                no_arg();
                f._0.contents = true;
                return ;
            case "Clear" :
                no_arg();
                f._0.contents = false;
                return ;
            case "String" :
                var arg$1 = get_arg();
                Curry._1(f._0, arg$1);
                return consume_arg();
            case "Set_string" :
                f._0.contents = get_arg();
                return consume_arg();
            case "Int" :
                var arg$2 = get_arg();
                var x = int_of_string_opt(arg$2);
                if (x !== undefined) {
                  Curry._1(f._0, x);
                } else {
                  throw {
                        RE_EXN_ID: Stop,
                        _1: {
                          TAG: "Wrong",
                          _0: s,
                          _1: arg$2,
                          _2: "an integer"
                        },
                        Error: new Error()
                      };
                }
                return consume_arg();
            case "Set_int" :
                var arg$3 = get_arg();
                var x$1 = int_of_string_opt(arg$3);
                if (x$1 !== undefined) {
                  f._0.contents = x$1;
                } else {
                  throw {
                        RE_EXN_ID: Stop,
                        _1: {
                          TAG: "Wrong",
                          _0: s,
                          _1: arg$3,
                          _2: "an integer"
                        },
                        Error: new Error()
                      };
                }
                return consume_arg();
            case "Float" :
                var arg$4 = get_arg();
                var x$2 = float_of_string_opt(arg$4);
                if (x$2 !== undefined) {
                  Curry._1(f._0, x$2);
                } else {
                  throw {
                        RE_EXN_ID: Stop,
                        _1: {
                          TAG: "Wrong",
                          _0: s,
                          _1: arg$4,
                          _2: "a float"
                        },
                        Error: new Error()
                      };
                }
                return consume_arg();
            case "Set_float" :
                var arg$5 = get_arg();
                var x$3 = float_of_string_opt(arg$5);
                if (x$3 !== undefined) {
                  f._0.contents = x$3;
                } else {
                  throw {
                        RE_EXN_ID: Stop,
                        _1: {
                          TAG: "Wrong",
                          _0: s,
                          _1: arg$5,
                          _2: "a float"
                        },
                        Error: new Error()
                      };
                }
                return consume_arg();
            case "Tuple" :
                return List.iter(treat_action, f._0);
            case "Symbol" :
                var symb = f._0;
                var arg$6 = get_arg();
                if (List.mem(arg$6, symb)) {
                  Curry._1(f._1, arg$6);
                  return consume_arg();
                }
                throw {
                      RE_EXN_ID: Stop,
                      _1: {
                        TAG: "Wrong",
                        _0: s,
                        _1: arg$6,
                        _2: "one of: " + make_symlist("", " ", "", symb)
                      },
                      Error: new Error()
                    };
            case "Rest" :
                var f$1 = f._0;
                while(current.contents < (argv.contents.length - 1 | 0)) {
                  Curry._1(f$1, Caml_array.get(argv.contents, current.contents + 1 | 0));
                  consume_arg();
                };
                return ;
            case "Expand" :
                if (!allow_expand) {
                  throw {
                        RE_EXN_ID: "Invalid_argument",
                        _1: "Arg.Expand is is only allowed with Arg.parse_and_expand_argv_dynamic",
                        Error: new Error()
                      };
                }
                var arg$7 = get_arg();
                var newarg = Curry._1(f._0, arg$7);
                consume_arg();
                var before = $$Array.sub(argv.contents, 0, current.contents + 1 | 0);
                var after = $$Array.sub(argv.contents, current.contents + 1 | 0, (argv.contents.length - current.contents | 0) - 1 | 0);
                argv.contents = Caml_array.concat({
                      hd: before,
                      tl: {
                        hd: newarg,
                        tl: {
                          hd: after,
                          tl: /* [] */0
                        }
                      }
                    });
                return ;
            
          }
        }
        }(s));
        treat_action(match[0]);
      } else {
        Curry._1(anonfun, s);
      }
    }
    catch (raw_m){
      var m = Caml_js_exceptions.internalToOCamlException(raw_m);
      if (m.RE_EXN_ID === Bad) {
        throw convert_error({
                  TAG: "Message",
                  _0: m._1
                });
      }
      if (m.RE_EXN_ID === Stop) {
        throw convert_error(m._1);
      }
      throw m;
    }
    current.contents = current.contents + 1 | 0;
  };
}

function parse_and_expand_argv_dynamic(current, argv, speclist, anonfun, errmsg) {
  parse_and_expand_argv_dynamic_aux(true, current, argv, speclist, anonfun, errmsg);
}

function parse_argv_dynamic(currentOpt, argv, speclist, anonfun, errmsg) {
  var current$1 = currentOpt !== undefined ? currentOpt : current;
  parse_and_expand_argv_dynamic_aux(false, current$1, {
        contents: argv
      }, speclist, anonfun, errmsg);
}

function parse_argv(currentOpt, argv, speclist, anonfun, errmsg) {
  var current$1 = currentOpt !== undefined ? currentOpt : current;
  parse_argv_dynamic(current$1, argv, {
        contents: speclist
      }, anonfun, errmsg);
}

function parse(l, f, msg) {
  try {
    return parse_argv(undefined, Sys.argv, l, f, msg);
  }
  catch (raw_msg){
    var msg$1 = Caml_js_exceptions.internalToOCamlException(raw_msg);
    if (msg$1.RE_EXN_ID === Bad) {
      console.log(msg$1._1);
      return Pervasives.exit(2);
    }
    if (msg$1.RE_EXN_ID === Help) {
      console.log(msg$1._1);
      return Pervasives.exit(0);
    }
    throw msg$1;
  }
}

function parse_dynamic(l, f, msg) {
  try {
    return parse_argv_dynamic(undefined, Sys.argv, l, f, msg);
  }
  catch (raw_msg){
    var msg$1 = Caml_js_exceptions.internalToOCamlException(raw_msg);
    if (msg$1.RE_EXN_ID === Bad) {
      console.log(msg$1._1);
      return Pervasives.exit(2);
    }
    if (msg$1.RE_EXN_ID === Help) {
      console.log(msg$1._1);
      return Pervasives.exit(0);
    }
    throw msg$1;
  }
}

function parse_expand(l, f, msg) {
  try {
    var argv = {
      contents: Sys.argv
    };
    var spec = {
      contents: l
    };
    var current$1 = {
      contents: current.contents
    };
    return parse_and_expand_argv_dynamic(current$1, argv, spec, f, msg);
  }
  catch (raw_msg){
    var msg$1 = Caml_js_exceptions.internalToOCamlException(raw_msg);
    if (msg$1.RE_EXN_ID === Bad) {
      console.log(msg$1._1);
      return Pervasives.exit(2);
    }
    if (msg$1.RE_EXN_ID === Help) {
      console.log(msg$1._1);
      return Pervasives.exit(0);
    }
    throw msg$1;
  }
}

function second_word(s) {
  var len = s.length;
  var loop = function (_n) {
    while(true) {
      var n = _n;
      if (n >= len) {
        return len;
      }
      if (Caml_string.get(s, n) !== /* ' ' */32) {
        return n;
      }
      _n = n + 1 | 0;
      continue ;
    };
  };
  var n;
  try {
    n = $$String.index(s, /* '\t' */9);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      var exit = 0;
      var n$1;
      try {
        n$1 = $$String.index(s, /* ' ' */32);
        exit = 2;
      }
      catch (raw_exn$1){
        var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
        if (exn$1.RE_EXN_ID === "Not_found") {
          return len;
        }
        throw exn$1;
      }
      if (exit === 2) {
        return loop(n$1 + 1 | 0);
      }
      
    } else {
      throw exn;
    }
  }
  return loop(n + 1 | 0);
}

function max_arg_len(cur, param) {
  var kwd = param[0];
  if (param[1].TAG === "Symbol") {
    return Caml.int_max(cur, kwd.length);
  } else {
    return Caml.int_max(cur, kwd.length + second_word(param[2]) | 0);
  }
}

function replace_leading_tab(s) {
  var seen = {
    contents: false
  };
  return $$String.map((function (c) {
                if (c !== 9 || seen.contents) {
                  return c;
                } else {
                  seen.contents = true;
                  return /* ' ' */32;
                }
              }), s);
}

function align(limitOpt, speclist) {
  var limit = limitOpt !== undefined ? limitOpt : Pervasives.max_int;
  var completed = add_help(speclist);
  var len = List.fold_left(max_arg_len, 0, completed);
  var len$1 = len < limit ? len : limit;
  return List.map((function (param) {
                var spec = param[1];
                var kwd = param[0];
                if (param[2] === "") {
                  return param;
                }
                if (spec.TAG === "Symbol") {
                  var msg = param[2];
                  var cutcol = second_word(msg);
                  var spaces = " ".repeat(Caml.int_max(0, len$1 - cutcol | 0) + 3 | 0);
                  return [
                          kwd,
                          spec,
                          "\n" + (spaces + replace_leading_tab(msg))
                        ];
                }
                var msg$1 = param[2];
                var cutcol$1 = second_word(msg$1);
                var kwd_len = kwd.length;
                var diff = (len$1 - kwd_len | 0) - cutcol$1 | 0;
                if (diff <= 0) {
                  return [
                          kwd,
                          spec,
                          replace_leading_tab(msg$1)
                        ];
                }
                var spaces$1 = " ".repeat(diff);
                var prefix = $$String.sub(replace_leading_tab(msg$1), 0, cutcol$1);
                var suffix = $$String.sub(msg$1, cutcol$1, msg$1.length - cutcol$1 | 0);
                return [
                        kwd,
                        spec,
                        prefix + (spaces$1 + suffix)
                      ];
              }), completed);
}

exports.parse = parse;
exports.parse_dynamic = parse_dynamic;
exports.parse_argv = parse_argv;
exports.parse_argv_dynamic = parse_argv_dynamic;
exports.parse_and_expand_argv_dynamic = parse_and_expand_argv_dynamic;
exports.parse_expand = parse_expand;
exports.Help = Help;
exports.Bad = Bad;
exports.usage = usage;
exports.usage_string = usage_string;
exports.align = align;
exports.current = current;
/* No side effect */
