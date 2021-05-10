'use strict';

var Sys = require("../../lib/js/sys.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Caml_sys = require("../../lib/js/caml_sys.js");
var Filename = require("../../lib/js/filename.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Test_literals = require("./test_literals.js");
var Ext_string_test = require("./ext_string_test.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var node_sep = "/";

var node_parent = "..";

var node_current = ".";

var cwd = {
  LAZY_DONE: false,
  VAL: (function () {
      return Caml_sys.sys_getcwd(undefined);
    })
};

function path_as_directory(x) {
  if (x === "" || Ext_string_test.ends_with(x, Filename.dir_sep)) {
    return x;
  } else {
    return x + Filename.dir_sep;
  }
}

function absolute_path(s) {
  var s$1 = Curry._1(Filename.is_relative, s) ? Filename.concat(CamlinternalLazy.force(cwd), s) : s;
  var aux = function (_s) {
    while(true) {
      var s = _s;
      var base = Curry._1(Filename.basename, s);
      var dir = Curry._1(Filename.dirname, s);
      if (dir === s) {
        return dir;
      }
      if (base !== Filename.current_dir_name) {
        if (base === Filename.parent_dir_name) {
          return Curry._1(Filename.dirname, aux(dir));
        } else {
          return Filename.concat(aux(dir), base);
        }
      }
      _s = dir;
      continue ;
    };
  };
  return aux(s$1);
}

function chop_extension(locOpt, name) {
  var loc = locOpt !== undefined ? locOpt : "";
  try {
    return Filename.chop_extension(name);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Invalid_argument") {
      var s = "Filename.chop_extension ( " + loc + " : " + name + " )";
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: s,
            Error: new Error()
          };
    }
    throw exn;
  }
}

function chop_extension_if_any(fname) {
  try {
    return Filename.chop_extension(fname);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Invalid_argument") {
      return fname;
    }
    throw exn;
  }
}

var os_path_separator_char = Filename.dir_sep.charCodeAt(0);

function relative_path(file_or_dir_1, file_or_dir_2) {
  var relevant_dir1 = file_or_dir_1.NAME === "File" ? Curry._1(Filename.dirname, file_or_dir_1.VAL) : file_or_dir_1.VAL;
  var relevant_dir2 = file_or_dir_2.NAME === "File" ? Curry._1(Filename.dirname, file_or_dir_2.VAL) : file_or_dir_2.VAL;
  var dir1 = Ext_string_test.split(undefined, relevant_dir1, os_path_separator_char);
  var dir2 = Ext_string_test.split(undefined, relevant_dir2, os_path_separator_char);
  var go = function (_dir1, _dir2) {
    while(true) {
      var dir2 = _dir2;
      var dir1 = _dir1;
      if (dir1 && dir2 && dir1.hd === dir2.hd) {
        _dir2 = dir2.tl;
        _dir1 = dir1.tl;
        continue ;
      }
      return Pervasives.$at(List.map((function (param) {
                        return node_parent;
                      }), dir2), dir1);
    };
  };
  var ys = go(dir1, dir2);
  if (ys) {
    if (ys.hd === node_parent) {
      return Belt_List.toArray(ys).join(node_sep);
    }
    var xs = {
      hd: node_current,
      tl: ys
    };
    return Belt_List.toArray(xs).join(node_sep);
  }
  var xs$1 = {
    hd: node_current,
    tl: ys
  };
  return Belt_List.toArray(xs$1).join(node_sep);
}

function node_relative_path(node_modules_shorten, file1, dep_file) {
  var file2 = dep_file.VAL;
  var v = Ext_string_test.find(undefined, Test_literals.node_modules, file2);
  var len = file2.length;
  if (!(node_modules_shorten && v >= 0)) {
    return relative_path(dep_file.NAME === "File" ? ({
                    NAME: "File",
                    VAL: absolute_path(dep_file.VAL)
                  }) : ({
                    NAME: "Dir",
                    VAL: absolute_path(dep_file.VAL)
                  }), file1.NAME === "File" ? ({
                    NAME: "File",
                    VAL: absolute_path(file1.VAL)
                  }) : ({
                    NAME: "Dir",
                    VAL: absolute_path(file1.VAL)
                  })) + (node_sep + Curry._1(Filename.basename, file2));
  }
  var skip = function (_i) {
    while(true) {
      var i = _i;
      if (i >= len) {
        var s = "invalid path: " + file2;
        throw {
              RE_EXN_ID: "Failure",
              _1: s,
              Error: new Error()
            };
      }
      var curr_char = file2.charCodeAt(i);
      if (!(curr_char === os_path_separator_char || curr_char === /* '.' */46)) {
        return i;
      }
      _i = i + 1 | 0;
      continue ;
    };
  };
  return Ext_string_test.tail_from(file2, skip(v + Test_literals.node_modules_length | 0));
}

function find_root_filename(_cwd, filename) {
  while(true) {
    var cwd = _cwd;
    if (Caml_sys.sys_file_exists(Filename.concat(cwd, filename))) {
      return cwd;
    }
    var cwd$p = Curry._1(Filename.dirname, cwd);
    if (cwd$p.length < cwd.length) {
      _cwd = cwd$p;
      continue ;
    }
    var s = "" + filename + " not found from " + cwd;
    throw {
          RE_EXN_ID: "Failure",
          _1: s,
          Error: new Error()
        };
  };
}

function find_package_json_dir(cwd) {
  return find_root_filename(cwd, Test_literals.bsconfig_json);
}

var package_dir = {
  LAZY_DONE: false,
  VAL: (function () {
      var cwd$1 = CamlinternalLazy.force(cwd);
      return find_root_filename(cwd$1, Test_literals.bsconfig_json);
    })
};

function module_name_of_file(file) {
  var s = Filename.chop_extension(Curry._1(Filename.basename, file));
  return Bytes.unsafe_to_string(Bytes.capitalize_ascii(Bytes.unsafe_of_string(s)));
}

function module_name_of_file_if_any(file) {
  var s = chop_extension_if_any(Curry._1(Filename.basename, file));
  return Bytes.unsafe_to_string(Bytes.capitalize_ascii(Bytes.unsafe_of_string(s)));
}

function combine(p1, p2) {
  if (p1 === "" || p1 === Filename.current_dir_name) {
    return p2;
  } else if (p2 === "" || p2 === Filename.current_dir_name) {
    return p1;
  } else if (Curry._1(Filename.is_relative, p2)) {
    return Filename.concat(p1, p2);
  } else {
    return p2;
  }
}

function split_aux(p) {
  var _p = p;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var p$1 = _p;
    var dir = Curry._1(Filename.dirname, p$1);
    if (dir === p$1) {
      return [
              dir,
              acc
            ];
    }
    var new_path = Curry._1(Filename.basename, p$1);
    if (new_path === Filename.dir_sep) {
      _p = dir;
      continue ;
    }
    _acc = {
      hd: new_path,
      tl: acc
    };
    _p = dir;
    continue ;
  };
}

function rel_normalized_absolute_path(from, to_) {
  var match = split_aux(from);
  var match$1 = split_aux(to_);
  var root2 = match$1[0];
  if (match[0] !== root2) {
    return root2;
  }
  var _xss = match[1];
  var _yss = match$1[1];
  while(true) {
    var yss = _yss;
    var xss = _xss;
    if (!xss) {
      if (yss) {
        return List.fold_left(Filename.concat, yss.hd, yss.tl);
      } else {
        return Ext_string_test.empty;
      }
    }
    var xs = xss.tl;
    if (!yss) {
      return List.fold_left((function (acc, param) {
                    return Filename.concat(acc, Ext_string_test.parent_dir_lit);
                  }), Ext_string_test.parent_dir_lit, xs);
    }
    if (xss.hd === yss.hd) {
      _yss = yss.tl;
      _xss = xs;
      continue ;
    }
    var start = List.fold_left((function (acc, param) {
            return Filename.concat(acc, Ext_string_test.parent_dir_lit);
          }), Ext_string_test.parent_dir_lit, xs);
    return List.fold_left(Filename.concat, start, yss);
  };
}

function normalize_absolute_path(x) {
  var drop_if_exist = function (xs) {
    if (xs) {
      return xs.tl;
    } else {
      return /* [] */0;
    }
  };
  var normalize_list = function (_acc, _paths) {
    while(true) {
      var paths = _paths;
      var acc = _acc;
      if (!paths) {
        return acc;
      }
      var xs = paths.tl;
      var x = paths.hd;
      if (x === Ext_string_test.current_dir_lit) {
        _paths = xs;
        continue ;
      }
      if (x === Ext_string_test.parent_dir_lit) {
        _paths = xs;
        _acc = drop_if_exist(acc);
        continue ;
      }
      _paths = xs;
      _acc = {
        hd: x,
        tl: acc
      };
      continue ;
    };
  };
  var match = split_aux(x);
  var root = match[0];
  var rev_paths = normalize_list(/* [] */0, match[1]);
  if (rev_paths) {
    var _acc = rev_paths.hd;
    var _rev_paths = rev_paths.tl;
    while(true) {
      var rev_paths$1 = _rev_paths;
      var acc = _acc;
      if (!rev_paths$1) {
        return Filename.concat(root, acc);
      }
      _rev_paths = rev_paths$1.tl;
      _acc = Filename.concat(rev_paths$1.hd, acc);
      continue ;
    };
  } else {
    return root;
  }
}

function get_extension(x) {
  var pos = Ext_string_test.rindex_neg(x, /* '.' */46);
  if (pos < 0) {
    return "";
  } else {
    return Ext_string_test.tail_from(x, pos);
  }
}

var simple_convert_node_path_to_os_path;

if (Sys.unix) {
  simple_convert_node_path_to_os_path = (function (x) {
      return x;
    });
} else if (Sys.win32 || false) {
  simple_convert_node_path_to_os_path = Ext_string_test.replace_slash_backward;
} else {
  var s = "Unknown OS : " + Sys.os_type;
  throw {
        RE_EXN_ID: "Failure",
        _1: s,
        Error: new Error()
      };
}

var $slash$slash = Filename.concat;

exports.node_sep = node_sep;
exports.node_parent = node_parent;
exports.node_current = node_current;
exports.cwd = cwd;
exports.$slash$slash = $slash$slash;
exports.path_as_directory = path_as_directory;
exports.absolute_path = absolute_path;
exports.chop_extension = chop_extension;
exports.chop_extension_if_any = chop_extension_if_any;
exports.os_path_separator_char = os_path_separator_char;
exports.relative_path = relative_path;
exports.node_relative_path = node_relative_path;
exports.find_root_filename = find_root_filename;
exports.find_package_json_dir = find_package_json_dir;
exports.package_dir = package_dir;
exports.module_name_of_file = module_name_of_file;
exports.module_name_of_file_if_any = module_name_of_file_if_any;
exports.combine = combine;
exports.split_aux = split_aux;
exports.rel_normalized_absolute_path = rel_normalized_absolute_path;
exports.normalize_absolute_path = normalize_absolute_path;
exports.get_extension = get_extension;
exports.simple_convert_node_path_to_os_path = simple_convert_node_path_to_os_path;
/* simple_convert_node_path_to_os_path Not a pure module */
