'use strict';

import * as Bytes from "./bytes";

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

export {
  make             ,
  init             ,
  empty            ,
  copy             ,
  of_string        ,
  to_string        ,
  sub              ,
  sub_string       ,
  fill             ,
  blit             ,
  concat           ,
  iter             ,
  iteri            ,
  map              ,
  mapi             ,
  trim             ,
  escaped          ,
  index            ,
  rindex           ,
  index_from       ,
  rindex_from      ,
  contains         ,
  contains_from    ,
  rcontains_from   ,
  uppercase        ,
  lowercase        ,
  capitalize       ,
  uncapitalize     ,
  compare          ,
  unsafe_to_string ,
  unsafe_of_string ,
  
}
/* No side effect */
