'use strict';


function f(children) {
  if (!children) {
    return [];
  }
  var children$1 = children[1];
  var a0 = children[0];
  if (!children$1) {
    return [a0];
  }
  var children$2 = children$1[1];
  var a1 = children$1[0];
  if (!children$2) {
    return [
            a0,
            a1
          ];
  }
  var children$3 = children$2[1];
  var a2 = children$2[0];
  if (!children$3) {
    return [
            a0,
            a1,
            a2
          ];
  }
  var children$4 = children$3[1];
  var a3 = children$3[0];
  if (!children$4) {
    return [
            a0,
            a1,
            a2,
            a3
          ];
  }
  var children$5 = children$4[1];
  var a4 = children$4[0];
  if (!children$5) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4
          ];
  }
  var children$6 = children$5[1];
  var a5 = children$5[0];
  if (!children$6) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5
          ];
  }
  var children$7 = children$6[1];
  var a6 = children$6[0];
  if (!children$7) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6
          ];
  }
  var children$8 = children$7[1];
  var a7 = children$7[0];
  if (!children$8) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7
          ];
  }
  var children$9 = children$8[1];
  var a8 = children$8[0];
  if (!children$9) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8
          ];
  }
  var children$10 = children$9[1];
  var a9 = children$9[0];
  if (!children$10) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9
          ];
  }
  var children$11 = children$10[1];
  var a10 = children$10[0];
  if (!children$11) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10
          ];
  }
  var children$12 = children$11[1];
  var a11 = children$11[0];
  if (!children$12) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11
          ];
  }
  var children$13 = children$12[1];
  var a12 = children$12[0];
  if (!children$13) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12
          ];
  }
  var children$14 = children$13[1];
  var a13 = children$13[0];
  if (!children$14) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13
          ];
  }
  var children$15 = children$14[1];
  var a14 = children$14[0];
  if (!children$15) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14
          ];
  }
  if (children$15[1]) {
    throw {
          ExceptionID: -9,
          _1: /* tuple */[
            "gpr_1150.ml",
            56,
            34
          ],
          Debug: "Assert_failure"
        };
  }
  return [
          a0,
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          children$15[0]
        ];
}

exports.f = f;
/* No side effect */
