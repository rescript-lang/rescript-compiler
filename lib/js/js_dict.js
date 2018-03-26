'use strict';


var unsafeDeleteKey = (
  function(dict,key){
     delete dict[key];
     return 0
   }
);

function entries(dict) {
  var keys = Object.keys(dict);
  var l = keys.length;
  var values = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    var key = keys[i];
    values[i] = /* tuple */[
      key,
      dict[key]
    ];
  }
  return values;
}

function values(dict) {
  var keys = Object.keys(dict);
  var l = keys.length;
  var values$1 = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    values$1[i] = dict[keys[i]];
  }
  return values$1;
}

function fromList(entries) {
  var dict = { };
  var _param = entries;
  while(true) {
    var param = _param;
    if (param) {
      var match = param[0];
      dict[match[0]] = match[1];
      _param = param[1];
      continue ;
    } else {
      return dict;
    }
  };
}

function fromArray(entries) {
  var dict = { };
  var l = entries.length;
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    var match = entries[i];
    dict[match[0]] = match[1];
  }
  return dict;
}

function map(f, source) {
  var target = { };
  var keys = Object.keys(source);
  var l = keys.length;
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    var key = keys[i];
    target[key] = f(source[key]);
  }
  return target;
}

exports.unsafeDeleteKey = unsafeDeleteKey;
exports.entries = entries;
exports.values = values;
exports.fromList = fromList;
exports.fromArray = fromArray;
exports.map = map;
/* unsafeDeleteKey Not a pure module */
