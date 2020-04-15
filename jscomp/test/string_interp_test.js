'use strict';


function hi2(xx, yy, zz) {
  return "\n" + xx + " " + yy + "\n\n" + zz + "\n";
}

function hi(a0, b0, xx, yy, zz) {
  return "\n零一二三四五六七八九 " + a0 + "\n零一二三四五六七八九 123456789 " + b0 + "\n测试一段中文 " + xx + ", " + yy + "\n" + zz + "\n\n";
}

function a3(world) {
  return "Hello \\" + world;
}

function a5(x) {
  return "" + x;
}

function a6(x) {
  return "" + x;
}

function a7(x0, x3, x5) {
  return "\\" + x0 + ",\$x1,\\\$x2,\\\\" + x3 + ", \\\\\$x4,\\\\\\" + x5;
}

function ffff(a_1, a_2) {
  return " hello " + a_1 + ", wlecome to " + a_2 + "  ";
}

function f(x, y) {
  var sum = x + y | 0;
  console.log(" " + x + " + " + y + " = " + sum + " ");
  
}

var world = "世界";

var hello_world = "你好，" + world;

function test1(x0) {
  return "你好，" + x0;
}

function test3(_xg) {
  return "你好，" + _xg;
}

function test5(x) {
  return "" + x;
}

var b = "test";

var c = "test";

var a = "test";

var a0 = "Hello \\";

var a1 = "Hello \\";

var a2 = "Hello \$";

var a4 = "";

exports.hi2 = hi2;
exports.hi = hi;
exports.b = b;
exports.c = c;
exports.a = a;
exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
exports.a5 = a5;
exports.a6 = a6;
exports.a7 = a7;
exports.ffff = ffff;
exports.f = f;
exports.world = world;
exports.hello_world = hello_world;
exports.test1 = test1;
exports.test3 = test3;
exports.test5 = test5;
/* No side effect */
