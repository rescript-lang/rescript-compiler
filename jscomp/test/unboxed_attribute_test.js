'use strict';


function make(x) {
  return /* constructor */{
          tag: "A",
          Arg0: x
        };
}

function get(param) {
  return param.Arg0;
}

var v0 = /* constructor */{
  tag: "A",
  Arg0: 3
};

exports.v0 = v0;
exports.make = make;
exports.get = get;
/* No side effect */
