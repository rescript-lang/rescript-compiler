'use strict';

var Curry = require("./curry.js");
var Caml_option = require("./caml_option.js");
var Caml_splice_call = require("./caml_splice_call.js");

function copyWithin(to_, arg0) {
  return arg0.copyWithin(to_);
}

function copyWithinFrom(to_, from, arg0) {
  return arg0.copyWithin(to_, from);
}

function copyWithinFromRange(to_, start, end_, arg0) {
  return arg0.copyWithin(to_, start, end_);
}

function fillInPlace(arg1, arg0) {
  return arg0.fill(arg1);
}

function fillFromInPlace(arg1, from, arg0) {
  return arg0.fill(arg1, from);
}

function fillRangeInPlace(arg1, start, end_, arg0) {
  return arg0.fill(arg1, start, end_);
}

function pop(arg0) {
  return Caml_option.undefined_to_opt(arg0.pop());
}

function push(arg1, arg0) {
  return arg0.push(arg1);
}

function pushMany(arg1, arg0) {
  return Caml_splice_call.spliceObjApply(arg0, "push", [arg1]);
}

function reverseInPlace(arg0) {
  return arg0.reverse();
}

function shift(arg0) {
  return Caml_option.undefined_to_opt(arg0.shift());
}

function sortInPlace(arg0) {
  return arg0.sort();
}

function sortInPlaceWith(arg1, arg0) {
  return arg0.sort(Curry.__2(arg1));
}

function spliceInPlace(pos, remove, add, arg0) {
  return Caml_splice_call.spliceObjApply(arg0, "splice", [
              pos,
              remove,
              add
            ]);
}

function removeFromInPlace(pos, arg0) {
  return arg0.splice(pos);
}

function removeCountInPlace(pos, count, arg0) {
  return arg0.splice(pos, count);
}

function unshift(arg1, arg0) {
  return arg0.unshift(arg1);
}

function unshiftMany(arg1, arg0) {
  return Caml_splice_call.spliceObjApply(arg0, "unshift", [arg1]);
}

function concat(arg1, arg0) {
  return arg0.concat(arg1);
}

function concatMany(arg1, arg0) {
  return Caml_splice_call.spliceObjApply(arg0, "concat", [arg1]);
}

function includes(arg1, arg0) {
  return arg0.includes(arg1);
}

function indexOf(arg1, arg0) {
  return arg0.indexOf(arg1);
}

function indexOfFrom(arg1, from, arg0) {
  return arg0.indexOf(arg1, from);
}

function joinWith(arg1, arg0) {
  return arg0.join(arg1);
}

function lastIndexOf(arg1, arg0) {
  return arg0.lastIndexOf(arg1);
}

function lastIndexOfFrom(arg1, from, arg0) {
  return arg0.lastIndexOf(arg1, from);
}

function slice(start, end_, arg0) {
  return arg0.slice(start, end_);
}

function copy(arg0) {
  return arg0.slice();
}

function sliceFrom(arg1, arg0) {
  return arg0.slice(arg1);
}

function toString(arg0) {
  return arg0.toString();
}

function toLocaleString(arg0) {
  return arg0.toLocaleString();
}

function every(arg1, arg0) {
  return arg0.every(Curry.__1(arg1));
}

function everyi(arg1, arg0) {
  return arg0.every(Curry.__2(arg1));
}

function filter(arg1, arg0) {
  return arg0.filter(Curry.__1(arg1));
}

function filteri(arg1, arg0) {
  return arg0.filter(Curry.__2(arg1));
}

function find(arg1, arg0) {
  return Caml_option.undefined_to_opt(arg0.find(Curry.__1(arg1)));
}

function findi(arg1, arg0) {
  return Caml_option.undefined_to_opt(arg0.find(Curry.__2(arg1)));
}

function findIndex(arg1, arg0) {
  return arg0.findIndex(Curry.__1(arg1));
}

function findIndexi(arg1, arg0) {
  return arg0.findIndex(Curry.__2(arg1));
}

function forEach(arg1, arg0) {
  arg0.forEach(Curry.__1(arg1));
  
}

function forEachi(arg1, arg0) {
  arg0.forEach(Curry.__2(arg1));
  
}

function map(arg1, arg0) {
  return arg0.map(Curry.__1(arg1));
}

function mapi(arg1, arg0) {
  return arg0.map(Curry.__2(arg1));
}

function reduce(arg1, arg2, arg0) {
  return arg0.reduce(Curry.__2(arg1), arg2);
}

function reducei(arg1, arg2, arg0) {
  return arg0.reduce(Curry.__3(arg1), arg2);
}

function reduceRight(arg1, arg2, arg0) {
  return arg0.reduceRight(Curry.__2(arg1), arg2);
}

function reduceRighti(arg1, arg2, arg0) {
  return arg0.reduceRight(Curry.__3(arg1), arg2);
}

function some(arg1, arg0) {
  return arg0.some(Curry.__1(arg1));
}

function somei(arg1, arg0) {
  return arg0.some(Curry.__2(arg1));
}

exports.copyWithin = copyWithin;
exports.copyWithinFrom = copyWithinFrom;
exports.copyWithinFromRange = copyWithinFromRange;
exports.fillInPlace = fillInPlace;
exports.fillFromInPlace = fillFromInPlace;
exports.fillRangeInPlace = fillRangeInPlace;
exports.pop = pop;
exports.push = push;
exports.pushMany = pushMany;
exports.reverseInPlace = reverseInPlace;
exports.shift = shift;
exports.sortInPlace = sortInPlace;
exports.sortInPlaceWith = sortInPlaceWith;
exports.spliceInPlace = spliceInPlace;
exports.removeFromInPlace = removeFromInPlace;
exports.removeCountInPlace = removeCountInPlace;
exports.unshift = unshift;
exports.unshiftMany = unshiftMany;
exports.concat = concat;
exports.concatMany = concatMany;
exports.includes = includes;
exports.indexOf = indexOf;
exports.indexOfFrom = indexOfFrom;
exports.joinWith = joinWith;
exports.lastIndexOf = lastIndexOf;
exports.lastIndexOfFrom = lastIndexOfFrom;
exports.slice = slice;
exports.copy = copy;
exports.sliceFrom = sliceFrom;
exports.toString = toString;
exports.toLocaleString = toLocaleString;
exports.every = every;
exports.everyi = everyi;
exports.filter = filter;
exports.filteri = filteri;
exports.find = find;
exports.findi = findi;
exports.findIndex = findIndex;
exports.findIndexi = findIndexi;
exports.forEach = forEach;
exports.forEachi = forEachi;
exports.map = map;
exports.mapi = mapi;
exports.reduce = reduce;
exports.reducei = reducei;
exports.reduceRight = reduceRight;
exports.reduceRighti = reduceRighti;
exports.some = some;
exports.somei = somei;
/* No side effect */
