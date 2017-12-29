'use strict';

var Bytes = require("./bytes.js");

var make = Bytes.make;

var init = Bytes.init;

var empty = Bytes.empty;

var copy = Bytes.copy;

var of_string = Bytes.of_string;

var to_string = Bytes.to_string;

var sub = Bytes.sub;

var sub_string = Bytes.sub_string;

var fill = Bytes.fill;

var blit = Bytes.blit;

var concat = Bytes.concat;

var iter = Bytes.iter;

var iteri = Bytes.iteri;

var map = Bytes.map;

var mapi = Bytes.mapi;

var trim = Bytes.trim;

var escaped = Bytes.escaped;

var index = Bytes.index;

var rindex = Bytes.rindex;

var index_from = Bytes.index_from;

var rindex_from = Bytes.rindex_from;

var contains = Bytes.contains;

var contains_from = Bytes.contains_from;

var rcontains_from = Bytes.rcontains_from;

var uppercase = Bytes.uppercase;

var lowercase = Bytes.lowercase;

var capitalize = Bytes.capitalize;

var uncapitalize = Bytes.uncapitalize;

var compare = Bytes.compare;

var unsafe_to_string = Bytes.unsafe_to_string;

var unsafe_of_string = Bytes.unsafe_of_string;

exports.make = make;
exports.init = init;
exports.empty = empty;
exports.copy = copy;
exports.of_string = of_string;
exports.to_string = to_string;
exports.sub = sub;
exports.sub_string = sub_string;
exports.fill = fill;
exports.blit = blit;
exports.concat = concat;
exports.iter = iter;
exports.iteri = iteri;
exports.map = map;
exports.mapi = mapi;
exports.trim = trim;
exports.escaped = escaped;
exports.index = index;
exports.rindex = rindex;
exports.index_from = index_from;
exports.rindex_from = rindex_from;
exports.contains = contains;
exports.contains_from = contains_from;
exports.rcontains_from = rcontains_from;
exports.uppercase = uppercase;
exports.lowercase = lowercase;
exports.capitalize = capitalize;
exports.uncapitalize = uncapitalize;
exports.compare = compare;
exports.unsafe_to_string = unsafe_to_string;
exports.unsafe_of_string = unsafe_of_string;
/* No side effect */
