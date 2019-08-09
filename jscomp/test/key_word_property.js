'use strict';

var Vscode = require("vscode");
var SomeEs6Module = require("some-es6-module");

var $$default = SomeEs6Module.default;

var $$window = Vscode.window;

function mk($$window, $$default) {
  return {
          window: $$window,
          default: $$default
        };
}

function mk2($$window, $$default) {
  return /* :: */[
          /* record */{
            window: $$window,
            default: $$default
          },
          /* [] */0
        ];
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
exports.$$window = $$window;
exports.mk = mk;
exports.mk2 = mk2;
exports.des = des;
exports.$$case = $$case;
exports.test = test;
exports.u = u;
/* default Not a pure module */
