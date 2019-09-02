'use strict';


var a = {
  x: 3,
  y: /* constructor */{
    tag: "::",
    Arg0: 1,
    Arg1: /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: "[]"
      }
    }
  }
};

exports.a = a;
/* No side effect */
