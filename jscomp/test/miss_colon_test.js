'use strict';

var Block = require("../../lib/js/block.js");

function $plus$colon(_f, _g) {
  while(true) {
    var g = _g;
    var f = _f;
    if (!f.tag) {
      var n = f[0];
      if (!g.tag) {
        return /* Int */Block.__(0, [n + g[0] | 0]);
      }
      if (n === 0) {
        return g;
      }
      
    }
    switch (g.tag | 0) {
      case /* Int */0 :
          if (g[0] !== 0) {
            return /* Add */Block.__(2, [
                      f,
                      g
                    ]);
          } else {
            return f;
          }
      case /* Add */2 :
          _g = g[1];
          _f = $plus$colon(f, g[0]);
          continue ;
      case /* Var */1 :
      case /* Mul */3 :
          return /* Add */Block.__(2, [
                    f,
                    g
                  ]);
      
    }
  };
}

function $star$colon(_f, _g) {
  while(true) {
    var g = _g;
    var f = _f;
    var exit = 0;
    var exit$1 = 0;
    if (f.tag) {
      exit$1 = 3;
    } else {
      var n = f[0];
      if (!g.tag) {
        return /* Int */Block.__(0, [Math.imul(n, g[0])]);
      }
      if (n === 0) {
        return /* Int */Block.__(0, [0]);
      }
      exit$1 = 3;
    }
    if (exit$1 === 3) {
      if (g.tag) {
        exit = 2;
      } else {
        if (g[0] === 0) {
          return /* Int */Block.__(0, [0]);
        }
        exit = 2;
      }
    }
    if (exit === 2 && !f.tag && f[0] === 1) {
      return g;
    }
    switch (g.tag | 0) {
      case /* Int */0 :
          if (g[0] !== 1) {
            return /* Mul */Block.__(3, [
                      f,
                      g
                    ]);
          } else {
            return f;
          }
      case /* Var */1 :
      case /* Add */2 :
          return /* Mul */Block.__(3, [
                    f,
                    g
                  ]);
      case /* Mul */3 :
          _g = g[1];
          _f = $star$colon(f, g[0]);
          continue ;
      
    }
  };
}

function simplify(f) {
  switch (f.tag | 0) {
    case /* Int */0 :
    case /* Var */1 :
        return f;
    case /* Add */2 :
        return $plus$colon(simplify(f[0]), simplify(f[1]));
    case /* Mul */3 :
        return $star$colon(simplify(f[0]), simplify(f[1]));
    
  }
}

exports.$plus$colon = $plus$colon;
exports.$star$colon = $star$colon;
exports.simplify = simplify;
/* No side effect */
