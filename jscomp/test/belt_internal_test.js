'use strict';


function treeHeight(n) {
  if (n !== void 0) {
    return n.height;
  } else {
    return 0;
  }
}

function copy(n) {
  if (n === void 0) {
    return n;
  }
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
}

exports.treeHeight = treeHeight;
exports.copy = copy;
/* No side effect */
