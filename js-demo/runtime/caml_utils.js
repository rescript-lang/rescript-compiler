// Generated CODE, PLEASE EDIT WITH CARE
'use strict';
define(["exports", "./caml_builtin_exceptions"],
  function(exports, Caml_builtin_exceptions){
    'use strict';
    var repeat = ( (String.prototype.repeat && function (count,self){return self.repeat(count)}) ||
                                                  function(count , self) {
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
);
    
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
    
    exports.i32div = i32div;
    exports.i32mod = i32mod;
    exports.repeat = repeat;
    
  })
/* repeat Not a pure module */
