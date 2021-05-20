'use strict';

var Curry = require("./curry.js");
var Caml_option = require("./caml_option.js");
var Caml_splice_call = require("./caml_splice_call.js");

function charAt(arg1, arg0) {
  return arg0.charAt(arg1);
}

function charCodeAt(arg1, arg0) {
  return arg0.charCodeAt(arg1);
}

function codePointAt(arg1, arg0) {
  return arg0.codePointAt(arg1);
}

function concat(arg1, arg0) {
  return arg0.concat(arg1);
}

function concatMany(arg1, arg0) {
  return Caml_splice_call.spliceObjApply(arg0, "concat", [arg1]);
}

function endsWith(arg1, arg0) {
  return arg0.endsWith(arg1);
}

function endsWithFrom(arg1, arg2, arg0) {
  return arg0.endsWith(arg1, arg2);
}

function includes(arg1, arg0) {
  return arg0.includes(arg1);
}

function includesFrom(arg1, arg2, arg0) {
  return arg0.includes(arg1, arg2);
}

function indexOf(arg1, arg0) {
  return arg0.indexOf(arg1);
}

function indexOfFrom(arg1, arg2, arg0) {
  return arg0.indexOf(arg1, arg2);
}

function lastIndexOf(arg1, arg0) {
  return arg0.lastIndexOf(arg1);
}

function lastIndexOfFrom(arg1, arg2, arg0) {
  return arg0.lastIndexOf(arg1, arg2);
}

function localeCompare(arg1, arg0) {
  return arg0.localeCompare(arg1);
}

function match_(arg1, arg0) {
  return Caml_option.null_to_opt(arg0.match(arg1));
}

function normalize(arg0) {
  return arg0.normalize();
}

function normalizeByForm(arg1, arg0) {
  return arg0.normalize(arg1);
}

function repeat(arg1, arg0) {
  return arg0.repeat(arg1);
}

function replace(arg1, arg2, arg0) {
  return arg0.replace(arg1, arg2);
}

function replaceByRe(arg1, arg2, arg0) {
  return arg0.replace(arg1, arg2);
}

function unsafeReplaceBy0(arg1, arg2, arg0) {
  return arg0.replace(arg1, Curry.__3(arg2));
}

function unsafeReplaceBy1(arg1, arg2, arg0) {
  return arg0.replace(arg1, Curry.__4(arg2));
}

function unsafeReplaceBy2(arg1, arg2, arg0) {
  return arg0.replace(arg1, Curry.__5(arg2));
}

function unsafeReplaceBy3(arg1, arg2, arg0) {
  return arg0.replace(arg1, Curry.__6(arg2));
}

function search(arg1, arg0) {
  return arg0.search(arg1);
}

function slice(from, to_, arg0) {
  return arg0.slice(from, to_);
}

function sliceToEnd(from, arg0) {
  return arg0.slice(from);
}

function split(arg1, arg0) {
  return arg0.split(arg1);
}

function splitAtMost(arg1, limit, arg0) {
  return arg0.split(arg1, limit);
}

function splitLimited(arg1, arg2, arg0) {
  return arg0.split(arg1, arg2);
}

function splitByRe(arg1, arg0) {
  return arg0.split(arg1);
}

function splitByReAtMost(arg1, limit, arg0) {
  return arg0.split(arg1, limit);
}

function splitRegexpLimited(arg1, arg2, arg0) {
  return arg0.split(arg1, arg2);
}

function startsWith(arg1, arg0) {
  return arg0.startsWith(arg1);
}

function startsWithFrom(arg1, arg2, arg0) {
  return arg0.startsWith(arg1, arg2);
}

function substr(from, arg0) {
  return arg0.substr(from);
}

function substrAtMost(from, length, arg0) {
  return arg0.substr(from, length);
}

function substring(from, to_, arg0) {
  return arg0.substring(from, to_);
}

function substringToEnd(from, arg0) {
  return arg0.substring(from);
}

function toLowerCase(arg0) {
  return arg0.toLowerCase();
}

function toLocaleLowerCase(arg0) {
  return arg0.toLocaleLowerCase();
}

function toUpperCase(arg0) {
  return arg0.toUpperCase();
}

function toLocaleUpperCase(arg0) {
  return arg0.toLocaleUpperCase();
}

function trim(arg0) {
  return arg0.trim();
}

function anchor(arg1, arg0) {
  return arg0.anchor(arg1);
}

function link(arg1, arg0) {
  return arg0.link(arg1);
}

exports.charAt = charAt;
exports.charCodeAt = charCodeAt;
exports.codePointAt = codePointAt;
exports.concat = concat;
exports.concatMany = concatMany;
exports.endsWith = endsWith;
exports.endsWithFrom = endsWithFrom;
exports.includes = includes;
exports.includesFrom = includesFrom;
exports.indexOf = indexOf;
exports.indexOfFrom = indexOfFrom;
exports.lastIndexOf = lastIndexOf;
exports.lastIndexOfFrom = lastIndexOfFrom;
exports.localeCompare = localeCompare;
exports.match_ = match_;
exports.normalize = normalize;
exports.normalizeByForm = normalizeByForm;
exports.repeat = repeat;
exports.replace = replace;
exports.replaceByRe = replaceByRe;
exports.unsafeReplaceBy0 = unsafeReplaceBy0;
exports.unsafeReplaceBy1 = unsafeReplaceBy1;
exports.unsafeReplaceBy2 = unsafeReplaceBy2;
exports.unsafeReplaceBy3 = unsafeReplaceBy3;
exports.search = search;
exports.slice = slice;
exports.sliceToEnd = sliceToEnd;
exports.split = split;
exports.splitAtMost = splitAtMost;
exports.splitLimited = splitLimited;
exports.splitByRe = splitByRe;
exports.splitByReAtMost = splitByReAtMost;
exports.splitRegexpLimited = splitRegexpLimited;
exports.startsWith = startsWith;
exports.startsWithFrom = startsWithFrom;
exports.substr = substr;
exports.substrAtMost = substrAtMost;
exports.substring = substring;
exports.substringToEnd = substringToEnd;
exports.toLowerCase = toLowerCase;
exports.toLocaleLowerCase = toLocaleLowerCase;
exports.toUpperCase = toUpperCase;
exports.toLocaleUpperCase = toLocaleUpperCase;
exports.trim = trim;
exports.anchor = anchor;
exports.link = link;
/* No side effect */
