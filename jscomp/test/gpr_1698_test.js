'use strict';


function is_number(_expr) {
  while(true) {
    var expr = _expr;
    switch (expr.tag | 0) {
      case /* Val */0 :
          if (expr._0.tag) {
            return false;
          } else {
            return true;
          }
      case /* Neg */1 :
          _expr = expr._0;
          continue ;
      case /* Sum */2 :
      case /* Pow */3 :
      case /* Frac */4 :
      case /* Gcd */5 :
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
    switch (a.tag | 0) {
      case /* Val */0 :
          switch (b.tag | 0) {
            case /* Val */0 :
                return 111;
            case /* Neg */1 :
                exit$3 = 5;
                break;
            case /* Sum */2 :
                exit$2 = 4;
                break;
            case /* Frac */4 :
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: /* tuple */[
                        "gpr_1698_test.ml",
                        45,
                        10
                      ],
                      Error: new Error()
                    };
            case /* Pow */3 :
            case /* Gcd */5 :
                exit = 1;
                break;
            
          }
          break;
      case /* Neg */1 :
          _a = a._0;
          continue ;
      case /* Sum */2 :
      case /* Pow */3 :
          exit$3 = 5;
          break;
      case /* Frac */4 :
          switch (b.tag | 0) {
            case /* Val */0 :
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: /* tuple */[
                        "gpr_1698_test.ml",
                        45,
                        10
                      ],
                      Error: new Error()
                    };
            case /* Neg */1 :
                exit$3 = 5;
                break;
            case /* Sum */2 :
                exit$2 = 4;
                break;
            case /* Frac */4 :
                na = a._0;
                da = a._1;
                nb = b._0;
                db = b._1;
                exit = 2;
                break;
            case /* Pow */3 :
            case /* Gcd */5 :
                exit = 1;
                break;
            
          }
          break;
      case /* Gcd */5 :
          switch (b.tag | 0) {
            case /* Neg */1 :
                exit$3 = 5;
                break;
            case /* Sum */2 :
                exit$2 = 4;
                break;
            case /* Gcd */5 :
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
      if (b.tag === /* Neg */1) {
        _b = b._0;
        continue ;
      }
      if (a.tag === /* Sum */2) {
        if (is_number(b)) {
          return 1;
        }
        exit$2 = 4;
      } else {
        exit$2 = 4;
      }
    }
    if (exit$2 === 4) {
      if (b.tag === /* Sum */2) {
        if (is_number(a)) {
          return -1;
        }
        exit$1 = 3;
      } else {
        exit$1 = 3;
      }
    }
    if (exit$1 === 3) {
      switch (a.tag | 0) {
        case /* Sum */2 :
            exit = 1;
            break;
        case /* Pow */3 :
            return -1;
        case /* Val */0 :
        case /* Frac */4 :
        case /* Gcd */5 :
            return 1;
        
      }
    }
    switch (exit) {
      case 1 :
          switch (b.tag | 0) {
            case /* Pow */3 :
                return 1;
            case /* Gcd */5 :
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
  tag: /* Sum */2,
  _0: /* :: */{
    _0: {
      tag: /* Val */0,
      _0: {
        tag: /* Symbol */1,
        _0: "a"
      }
    },
    _1: /* :: */{
      _0: {
        tag: /* Val */0,
        _0: {
          tag: /* Natural */0,
          _0: 2
        }
      },
      _1: /* [] */0
    }
  }
};

var b = {
  tag: /* Val */0,
  _0: {
    tag: /* Symbol */1,
    _0: "x"
  }
};

console.log(compare(/* InSum */0, {
          complex: true
        }, a, b));

/*  Not a pure module */
