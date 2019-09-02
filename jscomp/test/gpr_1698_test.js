'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function is_number(_expr) {
  while(true) {
    var expr = _expr;
    switch (/* XXX */expr.tag) {
      case "Val" :
          if (/* XXX */expr.Arg0.tag === "Natural") {
            return true;
          } else {
            return false;
          }
      case "Neg" :
          _expr = expr.Arg0;
          continue ;
      case "Sum" :
      case "Pow" :
      case "Frac" :
      case "Gcd" :
          return false;
      
    }
  };
}

function compare(context, state, _a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    var exit = 0;
    var na;
    var da;
    var nb;
    var db;
    var exit$1 = 0;
    var exit$2 = 0;
    switch (/* XXX */a.tag) {
      case "Val" :
          switch (/* XXX */b.tag) {
            case "Val" :
                return 111;
            case "Neg" :
                exit$2 = 5;
                break;
            case "Sum" :
                exit$1 = 4;
                break;
            case "Frac" :
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "gpr_1698_test.ml",
                        45,
                        10
                      ]
                    ];
            case "Pow" :
            case "Gcd" :
                exit = 1;
                break;
            
          }
          break;
      case "Neg" :
          _a = a.Arg0;
          continue ;
      case "Sum" :
      case "Pow" :
          exit$2 = 5;
          break;
      case "Frac" :
          switch (/* XXX */b.tag) {
            case "Val" :
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "gpr_1698_test.ml",
                        45,
                        10
                      ]
                    ];
            case "Neg" :
                exit$2 = 5;
                break;
            case "Sum" :
                exit$1 = 4;
                break;
            case "Frac" :
                na = a.Arg0;
                da = a.Arg1;
                nb = b.Arg0;
                db = b.Arg1;
                exit = 2;
                break;
            case "Pow" :
            case "Gcd" :
                exit = 1;
                break;
            
          }
          break;
      case "Gcd" :
          switch (/* XXX */b.tag) {
            case "Neg" :
                exit$2 = 5;
                break;
            case "Sum" :
                exit$1 = 4;
                break;
            case "Gcd" :
                na = a.Arg0;
                da = a.Arg1;
                nb = b.Arg0;
                db = b.Arg1;
                exit = 2;
                break;
            default:
              
          }
          break;
      
    }
    if (exit$2 === 5) {
      if (/* XXX */b.tag === "Neg") {
        _b = b.Arg0;
        continue ;
      } else if (/* XXX */a.tag === "Sum" && is_number(b)) {
        return 1;
      } else {
        exit$1 = 4;
      }
    }
    if (exit$1 === 4 && /* XXX */b.tag === "Sum" && is_number(a)) {
      return -1;
    }
    switch (/* XXX */a.tag) {
      case "Sum" :
          exit = 1;
          break;
      case "Pow" :
          return -1;
      case "Val" :
      case "Frac" :
      case "Gcd" :
          return 1;
      
    }
    switch (exit) {
      case 1 :
          switch (/* XXX */b.tag) {
            case "Pow" :
                return 1;
            case "Gcd" :
                return -1;
            default:
              return -1;
          }
      case 2 :
          var denom = compare(context, state, da, db);
          var match = denom === 0;
          if (match) {
            _b = nb;
            _a = na;
            continue ;
          } else {
            return denom;
          }
      
    }
  };
}

var a = /* constructor */{
  tag: "Sum",
  Arg0: /* constructor */{
    tag: "::",
    Arg0: /* constructor */{
      tag: "Val",
      Arg0: /* constructor */{
        tag: "Symbol",
        Arg0: "a"
      }
    },
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Val",
        Arg0: /* constructor */{
          tag: "Natural",
          Arg0: 2
        }
      },
      Arg1: "[]"
    }
  }
};

var b = /* constructor */{
  tag: "Val",
  Arg0: /* constructor */{
    tag: "Symbol",
    Arg0: "x"
  }
};

console.log(compare("InSum", /* record */[/* complex */true], a, b));

/*  Not a pure module */
