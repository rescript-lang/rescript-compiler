'use strict';

var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var jsMapperConstantArray = [
  [
    "Horizontal",
    "horizontal"
  ],
  [
    "Vertical",
    "vertical"
  ]
];

function orientationToJs(param) {
  return Js_mapperRt.binarySearch(2, param, jsMapperConstantArray);
}

function orientationFromJs(param) {
  return Js_mapperRt.revSearch(2, jsMapperConstantArray, param);
}

console.log(orientationToJs("Horizontal"));

exports.orientationToJs = orientationToJs;
exports.orientationFromJs = orientationFromJs;
/*  Not a pure module */
