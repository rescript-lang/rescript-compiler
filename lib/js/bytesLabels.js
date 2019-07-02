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

var extend = Bytes.extend;

var fill = Bytes.fill;

var blit = Bytes.blit;

var blit_string = Bytes.blit_string;

var concat = Bytes.concat;

var cat = Bytes.cat;

var iter = Bytes.iter;

var iteri = Bytes.iteri;

var map = Bytes.map;

var mapi = Bytes.mapi;

var trim = Bytes.trim;

var escaped = Bytes.escaped;

var index = Bytes.index;

var index_opt = Bytes.index_opt;

var rindex = Bytes.rindex;

var rindex_opt = Bytes.rindex_opt;

var index_from = Bytes.index_from;

var index_from_opt = Bytes.index_from_opt;

var rindex_from = Bytes.rindex_from;

var rindex_from_opt = Bytes.rindex_from_opt;

var contains = Bytes.contains;

var contains_from = Bytes.contains_from;

var rcontains_from = Bytes.rcontains_from;

var uppercase = Bytes.uppercase;

var lowercase = Bytes.lowercase;

var capitalize = Bytes.capitalize;

var uncapitalize = Bytes.uncapitalize;

var uppercase_ascii = Bytes.uppercase_ascii;

var lowercase_ascii = Bytes.lowercase_ascii;

var capitalize_ascii = Bytes.capitalize_ascii;

var uncapitalize_ascii = Bytes.uncapitalize_ascii;

var compare = Bytes.compare;

var equal = Bytes.equal;

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
exports.extend = extend;
exports.fill = fill;
exports.blit = blit;
exports.blit_string = blit_string;
exports.concat = concat;
exports.cat = cat;
exports.iter = iter;
exports.iteri = iteri;
exports.map = map;
exports.mapi = mapi;
exports.trim = trim;
exports.escaped = escaped;
exports.index = index;
exports.index_opt = index_opt;
exports.rindex = rindex;
exports.rindex_opt = rindex_opt;
exports.index_from = index_from;
exports.index_from_opt = index_from_opt;
exports.rindex_from = rindex_from;
exports.rindex_from_opt = rindex_from_opt;
exports.contains = contains;
exports.contains_from = contains_from;
exports.rcontains_from = rcontains_from;
exports.uppercase = uppercase;
exports.lowercase = lowercase;
exports.capitalize = capitalize;
exports.uncapitalize = uncapitalize;
exports.uppercase_ascii = uppercase_ascii;
exports.lowercase_ascii = lowercase_ascii;
exports.capitalize_ascii = capitalize_ascii;
exports.uncapitalize_ascii = uncapitalize_ascii;
exports.compare = compare;
exports.equal = equal;
exports.unsafe_to_string = unsafe_to_string;
exports.unsafe_of_string = unsafe_of_string;
/* No side effect */
