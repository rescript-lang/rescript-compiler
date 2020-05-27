'use strict';


function f(children) {
  if (!children) {
    return [];
  }
  var children$1 = children.tl;
  var a0 = children.hd;
  if (!children$1) {
    return [a0];
  }
  var children$2 = children$1.tl;
  var a1 = children$1.hd;
  if (!children$2) {
    return [
            a0,
            a1
          ];
  }
  var children$3 = children$2.tl;
  var a2 = children$2.hd;
  if (!children$3) {
    return [
            a0,
            a1,
            a2
          ];
  }
  var children$4 = children$3.tl;
  var a3 = children$3.hd;
  if (!children$4) {
    return [
            a0,
            a1,
            a2,
            a3
          ];
  }
  var children$5 = children$4.tl;
  var a4 = children$4.hd;
  if (!children$5) {
    return [
            a0,
            a1,
            a2,
            a3,
            a4
          ];
  }
  var children$6 = children$5.tl;
  var a5 = children$5.hd;
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
  var children$7 = children$6.tl;
  var a6 = children$6.hd;
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
  var children$8 = children$7.tl;
  var a7 = children$7.hd;
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
  var children$9 = children$8.tl;
  var a8 = children$8.hd;
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
  var children$10 = children$9.tl;
  var a9 = children$9.hd;
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
  var children$11 = children$10.tl;
  var a10 = children$10.hd;
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
  var children$12 = children$11.tl;
  var a11 = children$11.hd;
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
  var children$13 = children$12.tl;
  var a12 = children$12.hd;
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
  var children$14 = children$13.tl;
  var a13 = children$13.hd;
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
  var children$15 = children$14.tl;
  var a14 = children$14.hd;
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
  if (children$15.tl) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "gpr_1150.ml",
            56,
            34
          ],
          Error: new Error()
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
          children$15.hd
        ];
}

exports.f = f;
/* No side effect */
