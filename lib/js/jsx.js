'use strict';


function addKeyProp(p, k) {
  return Object.assign(p, {
              key: k
            });
}

exports.addKeyProp = addKeyProp;
/* No side effect */
