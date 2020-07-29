'use strict';


var _map = {"Horizontal":"horizontal","Vertical":"vertical"};

var _revMap = {"horizontal":"Horizontal","vertical":"Vertical"};

function orientationToJs(param) {
  return _map[param];
}

function orientationFromJs(param) {
  return _revMap[param];
}

console.log(orientationToJs("Horizontal"));

exports.orientationToJs = orientationToJs;
exports.orientationFromJs = orientationFromJs;
/*  Not a pure module */
