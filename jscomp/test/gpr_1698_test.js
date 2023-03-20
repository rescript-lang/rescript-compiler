'use strict';


function is_number(_expr) {
  while(true) {
    var expr = _expr;
    switch (expr.TAG) {
      case "Val" :
          if (expr._0.TAG === "Natural") {
            return true;
          } else {
            return false;
          }
      case "Neg" :
          _expr = expr._0;
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
    var exit$3 = 0;
    switch (a.TAG) {
      case "Val" :
          switch (b.TAG) {
            case "Val" :
                return 111;
            case "Neg" :
                exit$3 = 5;
                break;
            case "Sum" :
                exit$2 = 4;
                break;
            case "Frac" :
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: [
                        "gpr_1698_test.ml",
                        45,
                        10
                      ],
                      Error: new Error()
                    };
            case "Pow" :
            case "Gcd" :
                exit = 1;
                break;
            
          }
          break;
      case "Neg" :
          _a = a._0;
          continue ;
      case "Sum" :
      case "Pow" :
          exit$3 = 5;
          break;
      case "Frac" :
          switch (b.TAG) {
            case "Val" :
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: [
                        "gpr_1698_test.ml",
                        45,
                        10
                      ],
                      Error: new Error()
                    };
            case "Neg" :
                exit$3 = 5;
                break;
            case "Sum" :
                exit$2 = 4;
                break;
            case "Frac" :
                na = a._0;
                da = a._1;
                nb = b._0;
                db = b._1;
                exit = 2;
                break;
            case "Pow" :
            case "Gcd" :
                exit = 1;
                break;
            
          }
          break;
      case "Gcd" :
          switch (b.TAG) {
            case "Neg" :
                exit$3 = 5;
                break;
            case "Sum" :
                exit$2 = 4;
                break;
            case "Gcd" :
                na = a._0;
                da = a._1;
                nb = b._0;
                db = b._1;
                exit = 2;
                break;
            default:
              exit$1 = 3;
          }
          break;
      
    }
    if (exit$3 === 5) {
      if (b.TAG === "Neg") {
        _b = b._0;
        continue ;
      }
      if (a.TAG === "Sum") {
        if (is_number(b)) {
          return 1;
        }
        exit$2 = 4;
      } else {
        exit$2 = 4;
      }
    }
    if (exit$2 === 4) {
      if (b.TAG === "Sum") {
        if (is_number(a)) {
          return -1;
        }
        exit$1 = 3;
      } else {
        exit$1 = 3;
      }
    }
    if (exit$1 === 3) {
      switch (a.TAG) {
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
    }
    switch (exit) {
      case 1 :
          switch (b.TAG) {
            case "Pow" :
                return 1;
            case "Gcd" :
                return -1;
            default:
              return -1;
          }
      case 2 :
          var denom = compare(context, state, da, db);
          if (denom !== 0) {
            return denom;
          }
          _b = nb;
          _a = na;
          continue ;
      
    }
  };
}

var a = {
  TAG: "Sum",
  _0: {
    hd: {
      TAG: "Val",
      _0: {
        TAG: "Symbol",
        _0: "a"
      }
    },
    tl: {
      hd: {
        TAG: "Val",
        _0: {
          TAG: "Natural",
          _0: 2
        }
      },
      tl: /* [] */0
    }
  }
};

var b = {
  TAG: "Val",
  _0: {
    TAG: "Symbol",
    _0: "x"
  }
};

console.log(compare("InSum", {
          complex: true
        }, a, b));

/*  Not a pure module */
