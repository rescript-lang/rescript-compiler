'use strict';

var ZZ     = require("X");
var Z      = require("z");
var Vscode = require("vscode");

function f(a, b, c) {
  Vscode.commands.executeCommands("hi", a, b, c);
  return process.env;
}

function f2() {
  return /* tuple */[
          Z.a0.a1.a2.hi,
          a0.a1.a2.ho,
          Math.imul(1, 2)
        ];
}

function f3() {
  new (global.Buffer)(20);
  new (global.a0.a1.a2.Buffer)(20);
  new (ZZ.global.a0.a1.a2.Buffer)(100);
  new (ZZ.global.a0.a1.a2.makeBuffer3)(20);
  return /* () */0;
}

exports.f  = f;
exports.f2 = f2;
exports.f3 = f3;
/* X Not a pure module */
