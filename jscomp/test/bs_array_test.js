'use strict';


console.log(/* int array */[
            1,
            2,
            3,
            4
          ].filter(function (x) {
              return +(x > 2);
            }).map(function (x, i) {
            return x + i | 0;
          }).reduce(function (x, y) {
          return x + y | 0;
        }, 0));

/*  Not a pure module */
