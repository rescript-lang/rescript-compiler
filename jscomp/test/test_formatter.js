'use strict';


function f(param) {
  return /* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "Int",
            Arg0: "Int_d",
            Arg1: "No_padding",
            Arg2: "No_precision",
            Arg3: /* constructor */{
              tag: "String",
              Arg0: "No_padding",
              Arg1: "End_of_format"
            }
          },
          Arg1: "%d%s"
        };
}

exports.f = f;
/* No side effect */
