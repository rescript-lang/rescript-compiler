// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var Pervasives              = require("../stdlib/pervasives");
var Caml_curry              = require("../runtime/caml_curry");

var Bad = {
  0: "Test_seq.Bad",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

var Help = {
  0: "Test_seq.Help",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

var Stop = {
  0: "Test_seq.Stop",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

function assoc3(x, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var match = l[0];
      if (Caml_obj.caml_equal(match[0], x)) {
        return match[1];
      }
      else {
        _l = l[1];
        continue ;
        
      }
    }
    else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function help_action() {
  throw [
        Stop,
        /* Unknown */{
          0: "-help",
          length: 1,
          tag: 0
        }
      ];
}

function v(speclist) {
  assoc3("-help", speclist);
  return /* [] */0;
}

function f(g, speclist) {
  return Caml_curry.app1(g, assoc3("-help", speclist));
}

function add_help(speclist) {
  var add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      add1 = /* :: */[
        /* tuple */[
          "-help",
          /* Unit */{
            0: help_action,
            length: 1,
            tag: 0
          },
          " Display this list of options"
        ],
        /* [] */0
      ];
    }
    else {
      throw exn;
    }
  }
  var add2;
  try {
    assoc3("--help", speclist);
    add2 = /* [] */0;
  }
  catch (exn$1){
    if (exn$1 === Caml_builtin_exceptions.not_found) {
      add2 = /* :: */[
        /* tuple */[
          "--help",
          /* Unit */{
            0: help_action,
            length: 1,
            tag: 0
          },
          " Display this list of options"
        ],
        /* [] */0
      ];
    }
    else {
      throw exn$1;
    }
  }
  return Pervasives.$at(speclist, Pervasives.$at(add1, add2));
}

exports.Bad         = Bad;
exports.Help        = Help;
exports.Stop        = Stop;
exports.assoc3      = assoc3;
exports.help_action = help_action;
exports.v           = v;
exports.f           = f;
exports.add_help    = add_help;
/* No side effect */
