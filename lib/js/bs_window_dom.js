'use strict';

var Caml_obj = require("./caml_obj");
var Block    = require("./block");
var List     = require("./list");

function list_of_nodeList(nodeList) {
  var length = nodeList.length;
  var _acc = /* [] */0;
  var _i = 0;
  while(true) {
    var i = _i;
    var acc = _acc;
    if (i < length) {
      var match = nodeList.item(i);
      _i = i + 1 | 0;
      if (match !== null) {
        _acc = /* :: */[
          match,
          acc
        ];
        continue ;
        
      }
      else {
        continue ;
        
      }
    }
    else {
      return List.rev(acc);
    }
  };
}

function has(t, mask) {
  return +((t & mask) === mask);
}

function add(x, y) {
  return x | y;
}

var DocumentPosition = /* module */[
  /* disconnected */1,
  /* preceding */2,
  /* following */4,
  /* contains */8,
  /* contained_by */16,
  /* implementation_specific */32,
  /* has */has,
  /* add */add,
  /* + */add
];

function appendChild(p, n) {
  p.appendChild(n);
  return /* () */0;
}

function removeChild(p, n) {
  p.removeChild(n);
  return /* () */0;
}

function replaceChild(p, n, o) {
  p.replaceChild(n, o);
  return /* () */0;
}

function insertBefore(p, n, o) {
  p.insertBefore(n, o);
  return /* () */0;
}

function nodeType(e) {
  var match = e.nodeType;
  if (match !== 0) {
    switch (match - 1 | 0) {
      case 0 : 
          return /* Element */Block.__(0, [e]);
      case 1 : 
          return /* Attr */Block.__(1, [e]);
      case 2 : 
      case 3 : 
          return /* Text */Block.__(2, [e]);
      case 4 : 
      case 5 : 
      case 6 : 
      case 7 : 
      case 8 : 
      case 9 : 
      case 10 : 
      case 11 : 
          return /* Other */Block.__(3, [e]);
      
    }
  }
  else {
    return /* Other */Block.__(3, [e]);
  }
}

function cast(e, t) {
  if (Caml_obj.caml_equal(e.nodeType, t)) {
    return e;
  }
  else {
    return null;
  }
}

function element(e) {
  return cast(e, /* ELEMENT */1);
}

function text(e) {
  if (e.nodeType === /* TEXT */3 || e.nodeType === /* CDATA_SECTION */4) {
    return e;
  }
  else {
    return null;
  }
}

function attr(e) {
  return cast(e, /* ATTRIBUTE */2);
}

var CoerceTo = [
  element,
  text,
  attr
];

exports.DocumentPosition = DocumentPosition;
exports.insertBefore     = insertBefore;
exports.replaceChild     = replaceChild;
exports.removeChild      = removeChild;
exports.appendChild      = appendChild;
exports.list_of_nodeList = list_of_nodeList;
exports.nodeType         = nodeType;
exports.CoerceTo         = CoerceTo;
/* No side effect */
