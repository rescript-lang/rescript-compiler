'use strict';

var React = require("react");

function make(Props) {
  var foo = Props.foo;
  return React.createElement(make, {
              foo: foo
            });
}

exports.make = make;
/* react Not a pure module */
