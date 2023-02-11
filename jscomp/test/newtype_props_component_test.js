'use strict';


function Newtype_props_component_test$C1(props) {
  return props.foo;
}

var C1 = {
  make: Newtype_props_component_test$C1
};

function Newtype_props_component_test$C2(props) {
  return props.foo;
}

var C2 = {
  make: Newtype_props_component_test$C2
};

exports.C1 = C1;
exports.C2 = C2;
/* No side effect */
