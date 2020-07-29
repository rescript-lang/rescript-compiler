'use strict';


var revData = {"x":"a","b":"b"};

var data = {"a":"x","b":"b"};

function tToJs(x) {
  return data[x];
}

function vFromJsOpt(s) {
  return revData[s];
}

function vFromJs(s) {
  return revData[s];
}

exports.revData = revData;
exports.data = data;
exports.tToJs = tToJs;
exports.vFromJsOpt = vFromJsOpt;
exports.vFromJs = vFromJs;
/* No side effect */
