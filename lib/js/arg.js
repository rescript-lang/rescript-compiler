'use strict';

let Sys = require("./sys.js");
let Caml = require("./caml.js");
let List = require("./list.js");
let $$Array = require("./array.js");
let Buffer = require("./buffer.js");
let $$String = require("./string.js");
let Caml_obj = require("./caml_obj.js");
let Caml_array = require("./caml_array.js");
let Pervasives = require("./pervasives.js");
let Caml_format = require("./caml_format.js");
let Caml_string = require("./caml_string.js");
let Caml_exceptions = require("./caml_exceptions.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

let Bad = /* @__PURE__ */Caml_exceptions.create("Arg.Bad");

let Help = /* @__PURE__ */Caml_exceptions.create("Arg.Help");

let Stop = /* @__PURE__ */Caml_exceptions.create("Arg.Stop");

function assoc3(x, _l) {
  while (true) {
    let l = _l;
    if (l) {
      let match = l.hd;
      if (Caml_obj.equal(match[0], x)) {
        return match[1];
      }
      _l = l.tl;
      continue;
    }
    throw Caml_js_exceptions.internalMakeExn("Not_found");
  };
}

function split(s) {
  let i = $$String.index(s, /* '=' */61);
  let len = s.length;
  return [
    $$String.sub(s, 0, i),
    $$String.sub(s, i + 1 | 0, len - (i + 1 | 0) | 0)
  ];
}

function make_symlist(prefix, sep, suffix, l) {
  if (l) {
    return List.fold_left((x, y) => x + (sep + y), prefix + l.hd, l.tl) + suffix;
  } else {
    return "<none>";
  }
}

function help_action() {
  throw Caml_js_exceptions.internalFromExtension({
    RE_EXN_ID: Stop,
    _1: {
      TAG: "Unknown",
      _0: "-help"
    }
  });
}

function add_help(speclist) {
  let add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
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
  let add2;
  try {
    assoc3("--help", speclist);
    add2 = /* [] */0;
  } catch (raw_exn$1) {
    let exn$1 = Caml_js_exceptions.internalAnyToExn(raw_exn$1);
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
  Buffer.add_string(buf, errmsg + "\n");
  List.iter(x => {
    let doc = x[2];
    if (doc.length === 0) {
      return;
    }
    let spec = x[1];
    let key = x[0];
    if (spec.TAG !== "Symbol") {
      return Buffer.add_string(buf, "  " + key + " " + doc + "\n");
    }
    let sym = make_symlist("{", "|", "}", spec._0);
    Buffer.add_string(buf, "  " + key + " " + sym + doc + "\n");
  }, add_help(speclist));
}

function usage_string(speclist, errmsg) {
  let b = Buffer.create(200);
  usage_b(b, speclist, errmsg);
  return Buffer.contents(b);
}

function usage(speclist, errmsg) {
  console.log(usage_string(speclist, errmsg));
}

let current = {
  contents: 0
};

function bool_of_string_opt(x) {
  try {
    return Pervasives.bool_of_string(x);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
    if (exn.RE_EXN_ID === "Invalid_argument") {
      return;
    }
    throw exn;
  }
}

function int_of_string_opt(x) {
  try {
    return Caml_format.int_of_string(x);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return;
    }
    throw exn;
  }
}

function float_of_string_opt(x) {
  try {
    return Caml_format.float_of_string(x);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return;
    }
    throw exn;
  }
}

function parse_and_expand_argv_dynamic_aux(allow_expand, current, argv, speclist, anonfun, errmsg) {
  let initpos = current.contents;
  let convert_error = error => {
    let b = Buffer.create(200);
    let progname = initpos < argv.contents.length ? Caml_array.get(argv.contents, initpos) : "(?)";
    switch (error.TAG) {
      case "Unknown" :
        let s = error._0;
        switch (s) {
          case "--help" :
          case "-help" :
            break;
          default:
            Buffer.add_string(b, progname + ": unknown option '" + s + "'.\n");
        }
        break;
      case "Wrong" :
        Buffer.add_string(b, progname + ": wrong argument '" + error._1 + "'; option '" + error._0 + "' expects " + error._2 + ".\n");
        break;
      case "Missing" :
        Buffer.add_string(b, progname + ": option '" + error._0 + "' needs an argument.\n");
        break;
      case "Message" :
        Buffer.add_string(b, progname + ": " + error._0 + ".\n");
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
      return Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: Help,
        _1: Buffer.contents(b)
      });
    } else {
      return Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: Bad,
        _1: Buffer.contents(b)
      });
    }
  };
  current.contents = current.contents + 1 | 0;
  while (current.contents < argv.contents.length) {
    try {
      let s = Caml_array.get(argv.contents, current.contents);
      if (s.length >= 1 && Caml_string.get(s, 0) === /* '-' */45) {
        let match;
        try {
          match = [
            assoc3(s, speclist.contents),
            undefined
          ];
        } catch (raw_exn) {
          let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
          if (exn.RE_EXN_ID === "Not_found") {
            try {
              let match$1 = split(s);
              match = [
                assoc3(match$1[0], speclist.contents),
                match$1[1]
              ];
            } catch (raw_exn$1) {
              let exn$1 = Caml_js_exceptions.internalAnyToExn(raw_exn$1);
              if (exn$1.RE_EXN_ID === "Not_found") {
                throw Caml_js_exceptions.internalFromExtension({
                  RE_EXN_ID: Stop,
                  _1: {
                    TAG: "Unknown",
                    _0: s
                  }
                });
              }
              throw exn$1;
            }
          } else {
            throw exn;
          }
        }
        let follow = match[1];
        let no_arg = () => {
          if (follow === undefined) {
            return;
          }
          throw Caml_js_exceptions.internalFromExtension({
            RE_EXN_ID: Stop,
            _1: {
              TAG: "Wrong",
              _0: s,
              _1: follow,
              _2: "no argument"
            }
          });
        };
        let get_arg = () => {
          if (follow !== undefined) {
            return follow;
          }
          if ((current.contents + 1 | 0) < argv.contents.length) {
            return Caml_array.get(argv.contents, current.contents + 1 | 0);
          }
          throw Caml_js_exceptions.internalFromExtension({
            RE_EXN_ID: Stop,
            _1: {
              TAG: "Missing",
              _0: s
            }
          });
        };
        let consume_arg = () => {
          if (follow !== undefined) {
            return;
          } else {
            current.contents = current.contents + 1 | 0;
            return;
          }
        };
        let treat_action = f => {
          switch (f.TAG) {
            case "Unit" :
              return f._0();
            case "Bool" :
              let arg = get_arg();
              let s$1 = bool_of_string_opt(arg);
              if (s$1 !== undefined) {
                f._0(s$1);
              } else {
                throw Caml_js_exceptions.internalFromExtension({
                  RE_EXN_ID: Stop,
                  _1: {
                    TAG: "Wrong",
                    _0: s,
                    _1: arg,
                    _2: "a boolean"
                  }
                });
              }
              return consume_arg();
            case "Set" :
              no_arg();
              f._0.contents = true;
              return;
            case "Clear" :
              no_arg();
              f._0.contents = false;
              return;
            case "String" :
              let arg$1 = get_arg();
              f._0(arg$1);
              return consume_arg();
            case "Set_string" :
              f._0.contents = get_arg();
              return consume_arg();
            case "Int" :
              let arg$2 = get_arg();
              let x = int_of_string_opt(arg$2);
              if (x !== undefined) {
                f._0(x);
              } else {
                throw Caml_js_exceptions.internalFromExtension({
                  RE_EXN_ID: Stop,
                  _1: {
                    TAG: "Wrong",
                    _0: s,
                    _1: arg$2,
                    _2: "an integer"
                  }
                });
              }
              return consume_arg();
            case "Set_int" :
              let arg$3 = get_arg();
              let x$1 = int_of_string_opt(arg$3);
              if (x$1 !== undefined) {
                f._0.contents = x$1;
              } else {
                throw Caml_js_exceptions.internalFromExtension({
                  RE_EXN_ID: Stop,
                  _1: {
                    TAG: "Wrong",
                    _0: s,
                    _1: arg$3,
                    _2: "an integer"
                  }
                });
              }
              return consume_arg();
            case "Float" :
              let arg$4 = get_arg();
              let x$2 = float_of_string_opt(arg$4);
              if (x$2 !== undefined) {
                f._0(x$2);
              } else {
                throw Caml_js_exceptions.internalFromExtension({
                  RE_EXN_ID: Stop,
                  _1: {
                    TAG: "Wrong",
                    _0: s,
                    _1: arg$4,
                    _2: "a float"
                  }
                });
              }
              return consume_arg();
            case "Set_float" :
              let arg$5 = get_arg();
              let x$3 = float_of_string_opt(arg$5);
              if (x$3 !== undefined) {
                f._0.contents = x$3;
              } else {
                throw Caml_js_exceptions.internalFromExtension({
                  RE_EXN_ID: Stop,
                  _1: {
                    TAG: "Wrong",
                    _0: s,
                    _1: arg$5,
                    _2: "a float"
                  }
                });
              }
              return consume_arg();
            case "Tuple" :
              return List.iter(treat_action, f._0);
            case "Symbol" :
              let symb = f._0;
              let arg$6 = get_arg();
              if (List.mem(arg$6, symb)) {
                f._1(arg$6);
                return consume_arg();
              }
              throw Caml_js_exceptions.internalFromExtension({
                RE_EXN_ID: Stop,
                _1: {
                  TAG: "Wrong",
                  _0: s,
                  _1: arg$6,
                  _2: "one of: " + make_symlist("", " ", "", symb)
                }
              });
            case "Rest" :
              let f$1 = f._0;
              while (current.contents < (argv.contents.length - 1 | 0)) {
                f$1(Caml_array.get(argv.contents, current.contents + 1 | 0));
                consume_arg();
              };
              return;
            case "Expand" :
              if (!allow_expand) {
                throw Caml_js_exceptions.internalFromExtension({
                  RE_EXN_ID: "Invalid_argument",
                  _1: "Arg.Expand is is only allowed with Arg.parse_and_expand_argv_dynamic"
                });
              }
              let arg$7 = get_arg();
              let newarg = f._0(arg$7);
              consume_arg();
              let before = $$Array.sub(argv.contents, 0, current.contents + 1 | 0);
              let after = $$Array.sub(argv.contents, current.contents + 1 | 0, (argv.contents.length - current.contents | 0) - 1 | 0);
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
              return;
          }
        };
        treat_action(match[0]);
      } else {
        anonfun(s);
      }
    } catch (raw_m) {
      let m = Caml_js_exceptions.internalAnyToExn(raw_m);
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
  let current$1 = currentOpt !== undefined ? currentOpt : current;
  parse_and_expand_argv_dynamic_aux(false, current$1, {
    contents: argv
  }, speclist, anonfun, errmsg);
}

function parse_argv(currentOpt, argv, speclist, anonfun, errmsg) {
  let current$1 = currentOpt !== undefined ? currentOpt : current;
  parse_argv_dynamic(current$1, argv, {
    contents: speclist
  }, anonfun, errmsg);
}

function parse(l, f, msg) {
  try {
    return parse_argv(undefined, Sys.argv, l, f, msg);
  } catch (raw_msg) {
    let msg$1 = Caml_js_exceptions.internalAnyToExn(raw_msg);
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
  } catch (raw_msg) {
    let msg$1 = Caml_js_exceptions.internalAnyToExn(raw_msg);
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
    let argv = {
      contents: Sys.argv
    };
    let spec = {
      contents: l
    };
    let current$1 = {
      contents: current.contents
    };
    return parse_and_expand_argv_dynamic(current$1, argv, spec, f, msg);
  } catch (raw_msg) {
    let msg$1 = Caml_js_exceptions.internalAnyToExn(raw_msg);
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
  let len = s.length;
  let loop = _n => {
    while (true) {
      let n = _n;
      if (n >= len) {
        return len;
      }
      if (Caml_string.get(s, n) !== /* ' ' */32) {
        return n;
      }
      _n = n + 1 | 0;
      continue;
    };
  };
  let n;
  try {
    n = $$String.index(s, /* '\t' */9);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      let exit = 0;
      let n$1;
      try {
        n$1 = $$String.index(s, /* ' ' */32);
        exit = 2;
      } catch (raw_exn$1) {
        let exn$1 = Caml_js_exceptions.internalAnyToExn(raw_exn$1);
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
  let kwd = param[0];
  if (param[1].TAG === "Symbol") {
    return Caml.int_max(cur, kwd.length);
  } else {
    return Caml.int_max(cur, kwd.length + second_word(param[2]) | 0);
  }
}

function replace_leading_tab(s) {
  let seen = {
    contents: false
  };
  return $$String.map(c => {
    if (c !== 9 || seen.contents) {
      return c;
    } else {
      seen.contents = true;
      return /* ' ' */32;
    }
  }, s);
}

function align(limitOpt, speclist) {
  let limit = limitOpt !== undefined ? limitOpt : Pervasives.max_int;
  let completed = add_help(speclist);
  let len = List.fold_left(max_arg_len, 0, completed);
  let len$1 = len < limit ? len : limit;
  return List.map(x => {
    let spec = x[1];
    let kwd = x[0];
    if (x[2] === "") {
      return x;
    }
    if (spec.TAG === "Symbol") {
      let msg = x[2];
      let cutcol = second_word(msg);
      let spaces = " ".repeat(Caml.int_max(0, len$1 - cutcol | 0) + 3 | 0);
      return [
        kwd,
        spec,
        "\n" + (spaces + replace_leading_tab(msg))
      ];
    }
    let msg$1 = x[2];
    let cutcol$1 = second_word(msg$1);
    let kwd_len = kwd.length;
    let diff = (len$1 - kwd_len | 0) - cutcol$1 | 0;
    if (diff <= 0) {
      return [
        kwd,
        spec,
        replace_leading_tab(msg$1)
      ];
    }
    let spaces$1 = " ".repeat(diff);
    let prefix = $$String.sub(replace_leading_tab(msg$1), 0, cutcol$1);
    let suffix = $$String.sub(msg$1, cutcol$1, msg$1.length - cutcol$1 | 0);
    return [
      kwd,
      spec,
      prefix + (spaces$1 + suffix)
    ];
  }, completed);
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
