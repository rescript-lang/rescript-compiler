'use strict';

var $$Array                 = require("../../lib/js/array.js");
var Block                   = require("../../lib/js/block.js");
var Js_json                 = require("../../lib/js/js_json.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function $plus$great(x, i) {
  return x[i];
}

function tToJson(x) {
  return x;
}

function tJsonValidate(x) {
  if (Js_json.test(x, /* Array */3) && x.length === 2 && Js_json.test(x[0], /* Number */1)) {
    return Js_json.test(x[1], /* Number */1);
  } else {
    return /* false */0;
  }
}

function tFromJson(x) {
  if (!tJsonValidate(x)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "ast_to_json_test.ml",
            23,
            2
          ]
        ];
  }
  return x;
}

function uToJson(x) {
  return x;
}

function uJsonValidate(x) {
  if (Js_json.test(x, /* Array */3) && x.length === 2 && Js_json.test(x[0], /* Number */1)) {
    return tJsonValidate(x[1]);
  } else {
    return /* false */0;
  }
}

function uFromJson(x) {
  if (!uJsonValidate(x)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "ast_to_json_test.ml",
            56,
            2
          ]
        ];
  }
  return x;
}

function vJsonValidate(x) {
  if (Js_json.test(x, /* Number */1)) {
    return /* true */1;
  } else if (Js_json.test(x, /* Array */3)) {
    var match = x[0];
    if (match !== 0) {
      if (match !== 1) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "ast_to_json_test.ml",
                83,
                13
              ]
            ];
      } else {
        return tJsonValidate(x[1]);
      }
    } else if (Js_json.test(x[1], /* Number */1) && uJsonValidate(x[2])) {
      return tJsonValidate(x[3]);
    } else {
      return /* false */0;
    }
  } else {
    return /* false */0;
  }
}

function vToJson(x) {
  if (typeof x === "number") {
    return x;
  } else if (x.tag) {
    return /* tuple */[
            1,
            x[0]
          ];
  } else {
    return /* tuple */[
            0,
            x[0],
            uToJson(x[1]),
            x[2]
          ];
  }
}

function vFromJson(x) {
  if (Js_json.test(x, /* Number */1)) {
    return x;
  } else {
    var match = x[0];
    if (match !== 0) {
      if (match !== 1) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "ast_to_json_test.ml",
                100,
                11
              ]
            ];
      } else {
        return /* V2 */Block.__(1, [tFromJson(x[1])]);
      }
    } else {
      return /* V1 */Block.__(0, [
                x[1],
                uFromJson(x[2]),
                tFromJson(x[3])
              ]);
    }
  }
}

function hJsonValidate(_x) {
  while(true) {
    var x = _x;
    if (Js_json.test(x, /* Number */1)) {
      return +(x === 0);
    } else if (Js_json.test(x, /* Array */3)) {
      if (x[0]) {
        return /* false */0;
      } else if (vJsonValidate(x[1])) {
        _x = x[1];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else {
      return /* false */0;
    }
  };
}

function hToJson(x) {
  if (x) {
    return /* tuple */[
            vToJson(x[0]),
            hToJson(x[1])
          ];
  } else {
    return x;
  }
}

function hFromJson(x) {
  if (!hJsonValidate(x)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "ast_to_json_test.ml",
            130,
            2
          ]
        ];
  }
  if (Js_json.test(x, /* Number */1)) {
    return x;
  } else {
    return /* tuple */[
            vFromJson(x[1]),
            hFromJson(x[2])
          ];
  }
}

function intArrayToJson(x) {
  return x;
}

function arrayToJson(_, _$1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "ast_to_json_test.ml",
          149,
          2
        ]
      ];
}

function arrayFromJson(_, _$1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "ast_to_json_test.ml",
          152,
          2
        ]
      ];
}

function intArrayFromJson() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "ast_to_json_test.ml",
          154,
          2
        ]
      ];
}

function arrayJsonValidate(_, _$1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "ast_to_json_test.ml",
          157,
          2
        ]
      ];
}

function intArrayJsonValidate() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "ast_to_json_test.ml",
          160,
          2
        ]
      ];
}

var intListJsonValidate = intArrayJsonValidate;

function listToJson(p, x) {
  return arrayToJson(p, $$Array.of_list(x));
}

var intListToJson = $$Array.of_list;

function listFromJson(p, x) {
  return $$Array.to_list(arrayFromJson(p, x));
}

function intListFromJson(x) {
  return $$Array.to_list(intArrayFromJson(x));
}

var listJsonValidate = arrayJsonValidate;

function zJsonValidate(x) {
  if (Js_json.test(x, /* Array */3)) {
    var match = x[0];
    if (match > 4 || match < 0) {
      return /* false */0;
    } else {
      switch (match) {
        case 0 : 
            return Js_json.test(x[1], /* Number */1);
        case 1 : 
        case 3 : 
            return intArrayJsonValidate(x[1]);
        case 2 : 
        case 4 : 
            return arrayJsonValidate(hJsonValidate, x[1]);
        
      }
    }
  } else {
    return /* false */0;
  }
}

function zToJson(x) {
  switch (x.tag | 0) {
    case 0 : 
        return /* tuple */[
                0,
                x[0]
              ];
    case 1 : 
        return /* tuple */[
                1,
                $$Array.of_list(x[0])
              ];
    case 2 : 
        return /* tuple */[
                2,
                arrayToJson(hToJson, $$Array.of_list(x[0]))
              ];
    case 3 : 
        return /* tuple */[
                3,
                x[0]
              ];
    case 4 : 
        return /* tuple */[
                4,
                arrayToJson(hToJson, x[0])
              ];
    
  }
}

function zFromJson(x) {
  if (!zJsonValidate(x)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "ast_to_json_test.ml",
            213,
            2
          ]
        ];
  }
  var match = x[0];
  if (match > 4 || match < 0) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "ast_to_json_test.ml",
            225,
            9
          ]
        ];
  } else {
    switch (match) {
      case 0 : 
          return /* V0 */Block.__(0, [x[1]]);
      case 1 : 
          return /* V1 */Block.__(1, [$$Array.to_list(x[1])]);
      case 2 : 
          var x$1 = x[1];
          return /* V2 */Block.__(2, [$$Array.to_list(arrayFromJson(hFromJson, x$1))]);
      case 3 : 
          return /* V3 */Block.__(3, [x[1]]);
      case 4 : 
          return /* V4 */Block.__(4, [arrayFromJson(hFromJson, x[1])]);
      
    }
  }
}

var tIsImmediateJson = /* true */1;

var uIsImmediateJson = /* true */1;

var vIsImmediateJson = /* false */0;

var hIsImmediateJson = /* false */0;

var zIsImmedateJson = /* false */0;

exports.$plus$great          = $plus$great;
exports.tIsImmediateJson     = tIsImmediateJson;
exports.tToJson              = tToJson;
exports.tJsonValidate        = tJsonValidate;
exports.tFromJson            = tFromJson;
exports.uIsImmediateJson     = uIsImmediateJson;
exports.uToJson              = uToJson;
exports.uJsonValidate        = uJsonValidate;
exports.uFromJson            = uFromJson;
exports.vIsImmediateJson     = vIsImmediateJson;
exports.vJsonValidate        = vJsonValidate;
exports.vToJson              = vToJson;
exports.vFromJson            = vFromJson;
exports.hIsImmediateJson     = hIsImmediateJson;
exports.hJsonValidate        = hJsonValidate;
exports.hToJson              = hToJson;
exports.hFromJson            = hFromJson;
exports.intArrayToJson       = intArrayToJson;
exports.arrayToJson          = arrayToJson;
exports.arrayFromJson        = arrayFromJson;
exports.intArrayFromJson     = intArrayFromJson;
exports.arrayJsonValidate    = arrayJsonValidate;
exports.intArrayJsonValidate = intArrayJsonValidate;
exports.intListJsonValidate  = intListJsonValidate;
exports.listToJson           = listToJson;
exports.intListToJson        = intListToJson;
exports.listFromJson         = listFromJson;
exports.intListFromJson      = intListFromJson;
exports.listJsonValidate     = listJsonValidate;
exports.zIsImmedateJson      = zIsImmedateJson;
exports.zJsonValidate        = zJsonValidate;
exports.zToJson              = zToJson;
exports.zFromJson            = zFromJson;
/* No side effect */
