'use strict';



function add(x,y){
  return x + y
}

;

console.log(add(3.0, 2.0));

console.log(add("x", "y"));


function add_dyn(kind,x,y){
  switch(kind){
  case "string" : return x + y;
  case "float" : return x + y;
  }
}

;

function string_of_kind(kind) {
  if (kind !== 0) {
    return "string";
  } else {
    return "float";
  }
}

function add2(k, x, y) {
  return add_dyn(string_of_kind(k), x, y);
}

console.log(add2(/* Float */0, 3.0, 2.0));

console.log(add2(/* String */1, "x", "y"));

exports.string_of_kind = string_of_kind;
exports.add2 = add2;
/*  Not a pure module */
