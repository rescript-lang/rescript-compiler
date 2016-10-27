'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Bytes                   = require("../../lib/js/bytes");
var Filename                = require("../../lib/js/filename");
var CamlinternalLazy        = require("../../lib/js/camlinternalLazy");
var Caml_sys                = require("../../lib/js/caml_sys");
var Pervasives              = require("../../lib/js/pervasives");
var Block                   = require("../../lib/js/block");
var Ext_string              = require("./ext_string");
var Curry                   = require("../../lib/js/curry");
var Ext_pervasives          = require("./ext_pervasives");
var $$String                = require("../../lib/js/string");
var Format                  = require("../../lib/js/format");
var Caml_string             = require("../../lib/js/caml_string");
var List                    = require("../../lib/js/list");
var Literals                = require("./literals");

var node_sep = "/";

var node_parent = "..";

var node_current = ".";

var cwd = Block.__(246, [function () {
      return Caml_sys.caml_sys_getcwd(/* () */0);
    }]);

function combine(path1, path2) {
  if (path1 === "") {
    return path2;
  }
  else if (path2 === "") {
    return path1;
  }
  else if (Curry._1(Filename.is_relative, path2)) {
    return Filename.concat(path1, path2);
  }
  else {
    return path2;
  }
}

function path_as_directory(x) {
  if (x === "" || Ext_string.ends_with(x, Filename.dir_sep)) {
    return x;
  }
  else {
    return x + Filename.dir_sep;
  }
}

function absolute_path(s) {
  var s$1 = s;
  var s$2;
  if (Curry._1(Filename.is_relative, s$1)) {
    var tag = cwd.tag | 0;
    s$2 = Filename.concat(tag === 250 ? cwd[0] : (
            tag === 246 ? CamlinternalLazy.force_lazy_block(cwd) : cwd
          ), s$1);
  }
  else {
    s$2 = s$1;
  }
  var aux = function (_s) {
    while(true) {
      var s = _s;
      var match_000 = Curry._1(Filename.basename, s);
      var match_001 = Curry._1(Filename.dirname, s);
      var dir = match_001;
      var base = match_000;
      if (dir === s) {
        return dir;
      }
      else if (base === Filename.current_dir_name) {
        _s = dir;
        continue ;
        
      }
      else if (base === Filename.parent_dir_name) {
        return Curry._1(Filename.dirname, aux(dir));
      }
      else {
        return Filename.concat(aux(dir), base);
      }
    };
  };
  return aux(s$2);
}

function chop_extension($staropt$star, name) {
  var loc = $staropt$star ? $staropt$star[0] : "";
  try {
    return Filename.chop_extension(name);
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      return Curry._2(Format.ksprintf(Pervasives.invalid_arg, /* Format */[
                      /* String_literal */Block.__(11, [
                          "Filename.chop_extension ( ",
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* String_literal */Block.__(11, [
                                  " : ",
                                  /* String */Block.__(2, [
                                      /* No_padding */0,
                                      /* String_literal */Block.__(11, [
                                          " )",
                                          /* End_of_format */0
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                      "Filename.chop_extension ( %s : %s )"
                    ]), loc, name);
    }
    else {
      throw exn;
    }
  }
}

function chop_extension_if_any(fname) {
  try {
    return Filename.chop_extension(fname);
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      return fname;
    }
    else {
      throw exn;
    }
  }
}

function relative_path(file_or_dir_1, file_or_dir_2) {
  var sep_char = Caml_string.get(Filename.dir_sep, 0);
  var relevant_dir1 = file_or_dir_1[0] >= 781515420 ? Curry._1(Filename.dirname, file_or_dir_1[1]) : file_or_dir_1[1];
  var relevant_dir2 = file_or_dir_2[0] >= 781515420 ? Curry._1(Filename.dirname, file_or_dir_2[1]) : file_or_dir_2[1];
  var dir1 = Ext_string.split(/* None */0, relevant_dir1, sep_char);
  var dir2 = Ext_string.split(/* None */0, relevant_dir2, sep_char);
  var go = function (_dir1, _dir2) {
    while(true) {
      var dir2 = _dir2;
      var dir1 = _dir1;
      var exit = 0;
      if (dir1) {
        if (dir2) {
          if (dir1[0] === dir2[0]) {
            _dir2 = dir2[1];
            _dir1 = dir1[1];
            continue ;
            
          }
          else {
            exit = 1;
          }
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
      if (exit === 1) {
        return Pervasives.$at(List.map(function () {
                        return node_parent;
                      }, dir2), dir1);
      }
      
    };
  };
  var ys = go(dir1, dir2);
  if (ys) {
    if (ys[0] === node_parent) {
      return $$String.concat(node_sep, ys);
    }
    else {
      return $$String.concat(node_sep, /* :: */[
                  node_current,
                  ys
                ]);
    }
  }
  else {
    return $$String.concat(node_sep, /* :: */[
                node_current,
                ys
              ]);
  }
}

var os_path_separator_char = Filename.dir_sep.charCodeAt(0);

function node_relative_path(file1, dep_file) {
  var file2 = dep_file[1];
  var v = Ext_string.find(/* None */0, Literals.node_modules, file2);
  var len = file2.length;
  if (v >= 0) {
    var skip = function (_i) {
      while(true) {
        var i = _i;
        if (i >= len) {
          return Curry._1(Ext_pervasives.failwithf('File "ext_filename.ml", line 159, characters 38-45', /* Format */[
                          /* String_literal */Block.__(11, [
                              "invalid path: ",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* End_of_format */0
                                ])
                            ]),
                          "invalid path: %s"
                        ]), file2);
        }
        else {
          var curr_char = file2.charCodeAt(i);
          if (curr_char === os_path_separator_char || curr_char === /* "." */46) {
            _i = i + 1 | 0;
            continue ;
            
          }
          else {
            return i;
          }
        }
      };
    };
    return Ext_string.tail_from(file2, skip(v + Literals.node_modules_length | 0));
  }
  else {
    return relative_path(dep_file[0] >= 781515420 ? /* `File */[
                  781515420,
                  absolute_path(dep_file[1])
                ] : /* `Dir */[
                  3405101,
                  absolute_path(dep_file[1])
                ], file1[0] >= 781515420 ? /* `File */[
                  781515420,
                  absolute_path(file1[1])
                ] : /* `Dir */[
                  3405101,
                  absolute_path(file1[1])
                ]) + (node_sep + chop_extension_if_any(Curry._1(Filename.basename, file2)));
  }
}

function find_package_json_dir(cwd) {
  var _cwd = cwd;
  while(true) {
    var cwd$1 = _cwd;
    if (Caml_sys.caml_sys_file_exists(Filename.concat(cwd$1, Literals.package_json))) {
      return cwd$1;
    }
    else {
      var cwd$prime = Curry._1(Filename.dirname, cwd$1);
      if (cwd$prime.length < cwd$1.length) {
        _cwd = cwd$prime;
        continue ;
        
      }
      else {
        return Curry._1(Ext_pervasives.failwithf('File "ext_filename.ml", line 204, characters 15-22', /* Format */[
                        /* String_literal */Block.__(11, [
                            "package.json not found from ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* End_of_format */0
                              ])
                          ]),
                        "package.json not found from %s"
                      ]), cwd$1);
      }
    }
  };
}

var package_dir = Block.__(246, [function () {
      var tag = cwd.tag | 0;
      return find_package_json_dir(tag === 250 ? cwd[0] : (
                    tag === 246 ? CamlinternalLazy.force_lazy_block(cwd) : cwd
                  ));
    }]);

function replace_backward_slash(x) {
  return $$String.map(function (x) {
              if (x !== 92) {
                return x;
              }
              else {
                return /* "/" */47;
              }
            }, x);
}

function module_name_of_file(file) {
  var s = Filename.chop_extension(Curry._1(Filename.basename, file));
  return Caml_string.bytes_to_string(Bytes.capitalize(Caml_string.bytes_of_string(s)));
}

function module_name_of_file_if_any(file) {
  var s = chop_extension_if_any(Curry._1(Filename.basename, file));
  return Caml_string.bytes_to_string(Bytes.capitalize(Caml_string.bytes_of_string(s)));
}

var $slash$slash = Filename.concat;

exports.node_sep                   = node_sep;
exports.node_parent                = node_parent;
exports.node_current               = node_current;
exports.cwd                        = cwd;
exports.$slash$slash               = $slash$slash;
exports.combine                    = combine;
exports.path_as_directory          = path_as_directory;
exports.absolute_path              = absolute_path;
exports.chop_extension             = chop_extension;
exports.chop_extension_if_any      = chop_extension_if_any;
exports.relative_path              = relative_path;
exports.os_path_separator_char     = os_path_separator_char;
exports.node_relative_path         = node_relative_path;
exports.find_package_json_dir      = find_package_json_dir;
exports.package_dir                = package_dir;
exports.replace_backward_slash     = replace_backward_slash;
exports.module_name_of_file        = module_name_of_file;
exports.module_name_of_file_if_any = module_name_of_file_if_any;
/* Filename Not a pure module */
