// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("../stdlib/pervasives");
var Caml_primitive = require("../runtime/caml_primitive");

var Bad = [
  248,
  "Test_seq.Bad",
  ++ Caml_exceptions.caml_oo_last_id
];

var Help = [
  248,
  "Test_seq.Help",
  ++ Caml_exceptions.caml_oo_last_id
];

var Stop = [
  248,
  "Test_seq.Stop",
  ++ Caml_exceptions.caml_oo_last_id
];

function assoc3(x, _l) {
  while(/* true */1) {
    var l = _l;
    if (l) {
      var match = l[1];
      if (Caml_primitive.caml_equal(match[1], x)) {
        return match[2];
      }
      else {
        _l = l[2];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function help_action() {
  throw [
        0,
        Stop,
        [
          /* Unknown */0,
          "-help"
        ]
      ];
}

function v(speclist) {
  assoc3("-help", speclist);
  return /* [] */0;
}

function f(g, speclist) {
  return g(assoc3("-help", speclist));
}

function add_help(speclist) {
  var add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      add1 = [
        /* :: */0,
        [
          /* tuple */0,
          "-help",
          [
            /* Unit */0,
            help_action
          ],
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
    if (exn$1 === Caml_exceptions.Not_found) {
      add2 = [
        /* :: */0,
        [
          /* tuple */0,
          "--help",
          [
            /* Unit */0,
            help_action
          ],
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

exports.Bad = Bad;
exports.Help = Help;
exports.Stop = Stop;
exports.assoc3 = assoc3;
exports.help_action = help_action;
exports.v = v;
exports.f = f;
exports.add_help = add_help;
/* No side effect */
