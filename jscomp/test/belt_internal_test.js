'use strict';


function treeHeight(n) {
  if (n !== undefined) {
    return n.height;
  } else {
    return 0;
  }
}

function copy(n) {
  if (n !== undefined) {
    var match = n;
    var v = match.value;
    var h = match.height;
    var l = match.left;
    var r = match.right;
    return {
            value: v,
            height: h,
            left: copy(l),
            right: copy(r)
          };
  } else {
    return n;
  }
}

exports.treeHeight = treeHeight;
exports.copy = copy;
/* No side effect */
