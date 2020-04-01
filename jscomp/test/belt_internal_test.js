'use strict';


function treeHeight(n) {
  if (n !== undefined) {
    return n.height;
  } else {
    return 0;
  }
}

function copy(n) {
  if (n === undefined) {
    return n;
  }
  var v = n.value;
  var h = n.height;
  var l = n.left;
  var r = n.right;
  return {
          value: v,
          height: h,
          left: copy(l),
          right: copy(r)
        };
}

exports.treeHeight = treeHeight;
exports.copy = copy;
/* No side effect */
