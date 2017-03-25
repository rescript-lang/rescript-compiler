'use strict';

var List    = require("./list");
var $$Array = require("./array");
var Curry   = require("./curry");

function entries(dict) {
  return $$Array.map(function (key) {
              return /* tuple */[
                      key,
                      dict[key]
                    ];
            }, Object.keys(dict));
}

function values(dict) {
  return $$Array.map(function (param) {
              return dict[param];
            }, Object.keys(dict));
}

function fromList(entries) {
  var dict = { };
  List.iter(function (param) {
        dict[param[0]] = param[1];
        return /* () */0;
      }, entries);
  return dict;
}

function fromArray(entries) {
  var dict = { };
  $$Array.iter(function (param) {
        dict[param[0]] = param[1];
        return /* () */0;
      }, entries);
  return dict;
}

function map(f, source) {
  var target = { };
  $$Array.iter(function (key) {
        target[key] = Curry._1(f, source[key]);
        return /* () */0;
      }, Object.keys(source));
  return target;
}

exports.entries   = entries;
exports.values    = values;
exports.fromList  = fromList;
exports.fromArray = fromArray;
exports.map       = map;
/* No side effect */
