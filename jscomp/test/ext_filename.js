// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Filename        = require("../stdlib/filename");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives      = require("../stdlib/pervasives");
var Ext_string      = require("./ext_string");
var Caml_primitive  = require("../runtime/caml_primitive");
var Caml_curry      = require("../runtime/caml_curry");
var $$String        = require("../stdlib/string");
var List            = require("../stdlib/list");

var node_sep = "/";

var node_parent = "..";

var node_current = ".";

function absolute_path(s) {
  var s$1 = Caml_curry.app1(Filename.is_relative, s) ? Filename.concat(Caml_primitive.caml_sys_getcwd(/* () */0), s) : s;
  var aux = function (_s) {
    while(true) {
      var s = _s;
      var base = Caml_curry.app1(Filename.basename, s);
      var dir = Caml_curry.app1(Filename.dirname, s);
      if (dir === s) {
        return dir;
      }
      else if (base === Filename.current_dir_name) {
        _s = dir;
        continue ;
        
      }
      else if (base === Filename.parent_dir_name) {
        return Caml_curry.app1(Filename.dirname, aux(dir));
      }
      else {
        return Filename.concat(aux(dir), base);
      }
    };
  };
  return aux(s$1);
}

function chop_extension($staropt$star, name) {
  var loc = $staropt$star ? $staropt$star[1] : "";
  try {
    return Filename.chop_extension(name);
  }
  catch (exn){
    if (exn[1] === Caml_exceptions.Invalid_argument) {
      var s = "Filename.chop_extension (" + (loc + (":" + (name + ")")));
      throw [
            0,
            Caml_exceptions.Invalid_argument,
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
  var dir1 = Ext_string.split(/* None */0, Caml_curry.app1(Filename.dirname, file1), Filename.dir_sep.charCodeAt(0));
  var dir2 = Ext_string.split(/* None */0, Caml_curry.app1(Filename.dirname, file2), Filename.dir_sep.charCodeAt(0));
  var go = function (_dir1, _dir2) {
    while(true) {
      var dir2 = _dir2;
      var dir1 = _dir1;
      var exit = 0;
      if (dir1) {
        if (dir2) {
          if (dir1[1] === dir2[1]) {
            _dir2 = dir2[2];
            _dir1 = dir1[2];
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
    if (ys[1] === node_parent) {
      return $$String.concat(node_sep, ys);
    }
    else {
      return $$String.concat(node_sep, [
                  /* :: */0,
                  node_current,
                  ys
                ]);
    }
  }
  else {
    return $$String.concat(node_sep, [
                /* :: */0,
                node_current,
                ys
              ]);
  }
}

function node_relative_path(path1, path2) {
  return relative_path(try_chop_extension(absolute_path(path2)), try_chop_extension(absolute_path(path1))) + (node_sep + try_chop_extension(Caml_curry.app1(Filename.basename, path2)));
}

exports.node_sep           = node_sep;
exports.node_parent        = node_parent;
exports.node_current       = node_current;
exports.absolute_path      = absolute_path;
exports.chop_extension     = chop_extension;
exports.try_chop_extension = try_chop_extension;
exports.relative_path      = relative_path;
exports.node_relative_path = node_relative_path;
/* Filename Not a pure module */
