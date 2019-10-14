'use strict';


function Make(M) {
  return {
          $$return: M.$$return
        };
}

exports.Make = Make;
/* No side effect */
