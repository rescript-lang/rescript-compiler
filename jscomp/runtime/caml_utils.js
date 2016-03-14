// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");


if (!String.prototype.repeat) {
    String.prototype.repeat = function(count , self) {
        if (self.length == 0 || count == 0) {
            return '';
        }
        // Ensuring count is a 31-bit integer allows us to heavily optimize the
        // main part. But anyway, most current (August 2014) browsers can't handle
        // strings 1 << 28 chars or longer, so:
        if (self.length * count >= 1 << 28) {
            throw new RangeError('repeat count must not overflow maximum string size');
        }
        var rpt = '';
        for (;;) {
            if ((count & 1) == 1) {
                rpt += self;
            }
            count >>>= 1;
            if (count == 0) {
                break;
            }
            self += self;
        }
        return rpt;
    }
}




if (!Math.imul){
    Math.imul = function (x,y)
        { y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; }
}

var $$repeat = function (n, s) {
   return s.repeat(n);
}

;

function i32div(x, y) {
  if (y === 0) {
    throw Caml_builtin_exceptions.division_by_zero;
  }
  else {
    return x / y | 0;
  }
}

function i32mod(x, y) {
  if (y === 0) {
    throw Caml_builtin_exceptions.division_by_zero;
  }
  else {
    return x % y;
  }
}

function repeat(prim, prim$1) {
  return $$repeat(prim, prim$1);
}

exports.i32div = i32div;
exports.i32mod = i32mod;
exports.repeat = repeat;
/*  Not a pure module */
