'use strict';


if (4294966997 > 99) {
  if (1 > 1) {
    console.log("the catch all");
  } else {
    console.log("good response");
  }
} else if (4294966985 > 85) {
  console.log("bad response");
} else {
  console.log("the catch all");
}

var httpResponseCode = 201;

exports.httpResponseCode = httpResponseCode;
/*  Not a pure module */
