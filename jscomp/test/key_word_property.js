'use strict';

var Vscode = require("vscode");
var SomeEs6Module = require("some-es6-module");
var SomeEs6Module$1 = require("some-es6-module").default;
var OmeEs6Module = require("./ome-es6-module").default;
var OmeEs6Module$1 = require("./ome-es6-module");

var $$default = SomeEs6Module$1;

var default2 = SomeEs6Module.default2;

var oefault = OmeEs6Module;

var oefault2 = OmeEs6Module$1.default2;

var $$window = Vscode.window;

function mk($$window, $$default) {
  return {
          window: $$window,
          default: $$default
        };
}

function mk2($$window, $$default) {
  return {
          hd: {
            window: $$window,
            default: $$default
          },
          tl: /* [] */0
        };
}

function des(v) {
  return {
          window: v.window,
          default: v.default
        };
}

var test = {
  case: 3,
  window: 3
};

function u(param) {
  return $$window.switch();
}

var $$case = 3;

exports.$$default = $$default;
exports.default = $$default;
exports.__esModule = true;
exports.default2 = default2;
exports.oefault = oefault;
exports.oefault2 = oefault2;
exports.$$window = $$window;
exports.mk = mk;
exports.mk2 = mk2;
exports.des = des;
exports.$$case = $$case;
exports.test = test;
exports.u = u;
/* default Not a pure module */
