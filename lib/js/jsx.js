'use strict';


function addKeyProp(o, k) {
  return Object.assign(o, {
              key: k
            });
}

exports.addKeyProp = addKeyProp;
/* No side effect */
