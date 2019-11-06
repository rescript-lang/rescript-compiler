'use strict';

var Mt = require("./mt.js");
var Lazy = require("../../lib/js/lazy.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_module = require("../../lib/js/caml_module.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var Xx = {
  f: (function (prim, prim$1) {
      return Caml_external_polyfill.resolve("hfiehi")(prim, prim$1);
    })
};

var uuu = Xx.f;

var Int3 = Caml_module.init_mod(/* tuple */[
      "recursive_module.ml",
      27,
      6
    ], /* Module */Block.__(0, [/* array */[/* tuple */[
            0,
            "u"
          ]]]));

Caml_module.update_mod(/* Module */Block.__(0, [/* array */[/* tuple */[
            0,
            "u"
          ]]]), Int3, Int3);

var Inta = Caml_module.init_mod(/* tuple */[
      "recursive_module.ml",
      31,
      6
    ], /* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]));

var Intb = Caml_module.init_mod(/* tuple */[
      "recursive_module.ml",
      36,
      6
    ], /* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]));

var a = Caml_obj.caml_lazy_make((function (param) {
        return CamlinternalLazy.force(Intb.a);
      }));

Caml_module.update_mod(/* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]), Inta, {
      a: a
    });

var a$1 = Caml_obj.caml_lazy_make((function (param) {
        return CamlinternalLazy.force(Inta.a) + 1 | 0;
      }));

Caml_module.update_mod(/* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]), Intb, {
      a: a$1
    });

var tmp;

try {
  tmp = CamlinternalLazy.force(Intb.a);
}
catch (exn){
  if (exn === Lazy.Undefined) {
    tmp = -1;
  } else {
    throw exn;
  }
}

eq("File \"recursive_module.ml\", line 41, characters 3-10", -1, tmp);

var Inta$1 = Caml_module.init_mod(/* tuple */[
      "recursive_module.ml",
      48,
      8
    ], /* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]));

var Intb$1 = Caml_module.init_mod(/* tuple */[
      "recursive_module.ml",
      53,
      8
    ], /* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]));

var a$2 = Caml_obj.caml_lazy_make((function (param) {
        return CamlinternalLazy.force(Intb$1.a) + 1 | 0;
      }));

Caml_module.update_mod(/* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]), Inta$1, {
      a: a$2
    });

Caml_module.update_mod(/* Module */Block.__(0, [/* array */[/* tuple */[
            1,
            "a"
          ]]]), Intb$1, {
      a: 2
    });

var A = {
  Inta: Inta$1,
  Intb: Intb$1
};

eq("File \"recursive_module.ml\", line 58, characters 6-13", CamlinternalLazy.force(Inta$1.a), 3);

var tmp$1;

try {
  Curry._1(Int3.u, 3);
  tmp$1 = 3;
}
catch (raw_exn){
  var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn$1[0] === Caml_builtin_exceptions.undefined_recursive_module) {
    tmp$1 = 4;
  } else {
    throw exn$1;
  }
}

eq("File \"recursive_module.ml\", line 60, characters 6-13", 4, tmp$1);

Mt.from_pair_suites("Recursive_module", suites.contents);

var Int32 = /* () */0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Int32 = Int32;
exports.Xx = Xx;
exports.uuu = uuu;
exports.Int3 = Int3;
exports.Inta = Inta;
exports.Intb = Intb;
exports.A = A;
/* Int3 Not a pure module */
