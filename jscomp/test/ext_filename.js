// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Filename                = require("../stdlib/filename");
var Caml_sys                = require("../runtime/caml_sys");
var Pervasives              = require("../stdlib/pervasives");
var Block                   = require("../runtime/block");
var Ext_string              = require("./ext_string");
var Curry                   = require("../runtime/curry");
var Ext_pervasives          = require("./ext_pervasives");
var $$String                = require("../stdlib/string");
var List                    = require("../stdlib/list");
var Format                  = require("../stdlib/format");

var node_sep = "/";

var node_parent = "..";

var node_current = ".";

function absolute_path(s) {
  var s$1 = Curry._1(Filename.is_relative, s) ? Filename.concat(Caml_sys.caml_sys_getcwd(/* () */0), s) : s;
  var aux = function (_s) {
    while(true) {
      var s = _s;
      var base = Curry._1(Filename.basename, s);
      var dir = Curry._1(Filename.dirname, s);
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
  return aux(s$1);
}

function chop_extension($staropt$star, name) {
  var loc = $staropt$star ? $staropt$star[0] : "";
  try {
    return Filename.chop_extension(name);
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      var s = "Filename.chop_extension (" + (loc + (":" + (name + ")")));
      throw [
            Caml_builtin_exceptions.invalid_argument,
            s
          ];
    }
    else {
      throw exn;
    }
  }
}

function try_chop_extension(s) {
  try {
    return Filename.chop_extension(s);
  }
  catch (exn){
    return s;
  }
}

function relative_path(file1, file2) {
  var dir1 = Ext_string.split(/* None */0, Curry._1(Filename.dirname, file1), Filename.dir_sep.charCodeAt(0));
  var dir2 = Ext_string.split(/* None */0, Curry._1(Filename.dirname, file2), Filename.dir_sep.charCodeAt(0));
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

var node_modules = "node_modules";

function node_relative_path(path1, path2) {
  var v = Ext_string.find(/* None */0, node_modules, path2);
  var len = path2.length;
  if (v >= 0) {
    var skip = function (_i) {
      while(true) {
        var i = _i;
        if (i >= len) {
          var s = "invalid path: " + path2;
          throw [
                Caml_builtin_exceptions.failure,
                s
              ];
        }
        else {
          var match = path2.charCodeAt(i);
          if (match === 47 || match === 46) {
            _i = i + 1 | 0;
            continue ;
            
          }
          else {
            return i;
          }
        }
      };
    };
    return Ext_string.tail_from(path2, skip(v + 12 | 0));
  }
  else {
    return relative_path(try_chop_extension(absolute_path(path2)), try_chop_extension(absolute_path(path1))) + (node_sep + try_chop_extension(Curry._1(Filename.basename, path2)));
  }
}

function resolve(cwd, module_name) {
  var origin = cwd;
  var _cwd = cwd;
  var module_name$1 = module_name;
  while(true) {
    var cwd$1 = _cwd;
    var v = Filename.concat(Filename.concat(cwd$1, node_modules), module_name$1);
    if (Caml_sys.caml_sys_is_directory(v)) {
      return v;
    }
    else {
      var cwd$prime = Curry._1(Filename.dirname, cwd$1);
      if (cwd$prime.length < cwd$1.length) {
        _cwd = cwd$prime;
        continue ;
        
      }
      else {
        return Curry._2(Format.ksprintf(Pervasives.failwith, /* Format */[
                        /* String */Block.__(2, [
                            /* No_padding */0,
                            /* String_literal */Block.__(11, [
                                " not found in ",
                                /* String */Block.__(2, [
                                    /* No_padding */0,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "%s not found in %s"
                      ]), module_name$1, origin);
      }
    }
  };
}

var node_modules_length = 12;

exports.node_sep            = node_sep;
exports.node_parent         = node_parent;
exports.node_current        = node_current;
exports.absolute_path       = absolute_path;
exports.chop_extension      = chop_extension;
exports.try_chop_extension  = try_chop_extension;
exports.relative_path       = relative_path;
exports.node_modules        = node_modules;
exports.node_modules_length = node_modules_length;
exports.node_relative_path  = node_relative_path;
exports.resolve             = resolve;
/* Filename Not a pure module */
