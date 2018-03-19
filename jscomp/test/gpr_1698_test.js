'use strict';

var Block = require("../../lib/js/block.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function is_number(_expr) {
  while(true) {
    var expr = _expr;
    switch (expr.tag | 0) {
      case 0 : 
          if (expr[0].tag) {
            return /* false */0;
          } else {
            return /* true */1;
          }
      case 1 : 
          _expr = expr[0];
          continue ;
      case 2 : 
      case 3 : 
      case 4 : 
      case 5 : 
          return /* false */0;
      
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
      case 0 : 
          switch (b.tag | 0) {
            case 0 : 
                return 111;
            case 1 : 
                exit$3 = 5;
                break;
            case 2 : 
                exit$2 = 4;
                break;
            case 4 : 
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      [
                        "gpr_1698_test.ml",
                        45,
                        10
                      ]
                    ];
            case 3 : 
            case 5 : 
                exit = 1;
                break;
            
          }
          break;
      case 1 : 
          _a = a[0];
          continue ;
      case 2 : 
      case 3 : 
          exit$3 = 5;
          break;
      case 4 : 
          switch (b.tag | 0) {
            case 0 : 
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      [
                        "gpr_1698_test.ml",
                        45,
                        10
                      ]
                    ];
            case 1 : 
                exit$3 = 5;
                break;
            case 2 : 
                exit$2 = 4;
                break;
            case 4 : 
                na = a[0];
                da = a[1];
                nb = b[0];
                db = b[1];
                exit = 2;
                break;
            case 3 : 
            case 5 : 
                exit = 1;
                break;
            
          }
          break;
      case 5 : 
          switch (b.tag | 0) {
            case 1 : 
                exit$3 = 5;
                break;
            case 2 : 
                exit$2 = 4;
                break;
            case 5 : 
                na = a[0];
                da = a[1];
                nb = b[0];
                db = b[1];
                exit = 2;
                break;
            default:
              exit$1 = 3;
          }
          break;
      
    }
    if (exit$3 === 5) {
      if (b.tag === 1) {
        _b = b[0];
        continue ;
      } else if (a.tag === 2 && is_number(b)) {
        return 1;
      } else {
        exit$2 = 4;
      }
    }
    if (exit$2 === 4) {
      if (b.tag === 2 && is_number(a)) {
        return -1;
      } else {
        exit$1 = 3;
      }
    }
    if (exit$1 === 3) {
      switch (a.tag | 0) {
        case 2 : 
            exit = 1;
            break;
        case 3 : 
            return -1;
        case 0 : 
        case 4 : 
        case 5 : 
            return 1;
        
      }
    }
    switch (exit) {
      case 1 : 
          switch (b.tag | 0) {
            case 3 : 
                return 1;
            case 5 : 
                return -1;
            default:
              return -1;
          }
      case 2 : 
          var denom = compare(context, state, da, db);
          var match = +(denom === 0);
          if (match !== 0) {
            _b = nb;
            _a = na;
            continue ;
          } else {
            return denom;
          }
      
    }
  };
}

var a = /* Sum */Block.__(2, [/* :: */[
      /* Val */Block.__(0, [/* Symbol */Block.__(1, ["a"])]),
      /* :: */[
        /* Val */Block.__(0, [/* Natural */Block.__(0, [2])]),
        /* [] */0
      ]
    ]]);

var b = /* Val */Block.__(0, [/* Symbol */Block.__(1, ["x"])]);

console.log(compare(/* InSum */0, /* record */[/* complex : true */1], a, b));

/*  Not a pure module */
