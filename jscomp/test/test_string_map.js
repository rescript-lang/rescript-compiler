'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function height(param) {
  if (param !== "Empty") {
    return param.Arg4;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: x,
          Arg2: d,
          Arg3: r,
          Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, x, d, r) {
  var hl = l !== "Empty" ? l.Arg4 : 0;
  var hr = r !== "Empty" ? r.Arg4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg3;
      var ld = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr !== "Empty") {
        return create(create(ll, lv, ld, lr.Arg0), lr.Arg1, lr.Arg2, create(lr.Arg3, x, d, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== "Empty") {
      var rr = r.Arg3;
      var rd = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl !== "Empty") {
        return create(create(l, x, d, rl.Arg0), rl.Arg1, rl.Arg2, create(rl.Arg3, rv, rd, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: x,
            Arg2: d,
            Arg3: r,
            Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function add(x, data, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: x,
              Arg2: data,
              Arg3: r,
              Arg4: param.Arg4
            };
    } else if (c < 0) {
      return bal(add(x, data, l), v, d, r);
    } else {
      return bal(l, v, d, add(x, data, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: data,
            Arg3: "Empty",
            Arg4: 1
          };
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_string_compare(x, param.Arg1);
      if (c === 0) {
        return param.Arg2;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function timing(label, f) {
  console.time(label);
  Curry._1(f, /* () */0);
  console.timeEnd(label);
  return /* () */0;
}

function assertion_test(param) {
  var m = /* record */[/* contents */"Empty"];
  timing("building", (function (param) {
          for(var i = 0; i <= 1000000; ++i){
            m[0] = add(String(i), String(i), m[0]);
          }
          return /* () */0;
        }));
  return timing("querying", (function (param) {
                for(var i = 0; i <= 1000000; ++i){
                  find(String(i), m[0]);
                }
                return /* () */0;
              }));
}

exports.assertion_test = assertion_test;
/* No side effect */
