'use strict';


function Alias_default_value_test$C0(props) {
  var a = props.a;
  var a$1 = a !== undefined ? a : 2;
  var b = props.b;
  var b$1 = b !== undefined ? b : (a$1 << 1);
  return a$1 + b$1 | 0;
}

var C0 = {
  make: Alias_default_value_test$C0
};

function Alias_default_value_test$C1(props) {
  var foo = props.foo;
  if (foo !== undefined) {
    return foo;
  } else {
    return "";
  }
}

var C1 = {
  make: Alias_default_value_test$C1
};

exports.C0 = C0;
exports.C1 = C1;
/* No side effect */
