// Generated by ReScript, PLEASE EDIT WITH CARE


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
  if (kind === "Float") {
    return "float";
  } else {
    return "string";
  }
}

function add2(k, x, y) {
  return add_dyn(string_of_kind(k), x, y);
}

console.log(add2("Float", 3.0, 2.0));

console.log(add2("String", "x", "y"));

export {
  string_of_kind,
  add2,
}
/*  Not a pure module */
