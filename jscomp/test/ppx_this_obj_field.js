'use strict';


function v5() {
  return {
          x: 3,
          y: 4,
          setY: function (v) {
            var self = this ;
            self.y = 2;
            return /* tuple */[
                    self.y,
                    v
                  ];
          },
          say: function () {
            var self = this ;
            return self.x + self.y | 0;
          }
        };
}

exports.v5 = v5;
/* No side effect */
