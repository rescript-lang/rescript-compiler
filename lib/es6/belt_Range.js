

import * as Curry from "./curry.js";

function forEachU(s, f, action) {
  for(var i = s; i <= f; ++i){
    action(i);
  }
  return /* () */0;
}

function forEach(s, f, action) {
  return forEachU(s, f, Curry.__1(action));
}

function everyU(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return true;
    } else if (p(s)) {
      _s = s + 1 | 0;
      continue ;
    } else {
      return false;
    }
  };
}

function every(s, f, p) {
  return everyU(s, f, Curry.__1(p));
}

function everyByU(s, f, step, p) {
  if (step > 0) {
    var _s = s;
    var f$1 = f;
    var step$1 = step;
    var p$1 = p;
    while(true) {
      var s$1 = _s;
      if (s$1 > f$1) {
        return true;
      } else if (p$1(s$1)) {
        _s = s$1 + step$1 | 0;
        continue ;
      } else {
        return false;
      }
    };
  } else {
    return true;
  }
}

function everyBy(s, f, step, p) {
  return everyByU(s, f, step, Curry.__1(p));
}

function someU(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return false;
    } else if (p(s)) {
      return true;
    } else {
      _s = s + 1 | 0;
      continue ;
    }
  };
}

function some(s, f, p) {
  return someU(s, f, Curry.__1(p));
}

function someByU(s, f, step, p) {
  if (step > 0) {
    var _s = s;
    var f$1 = f;
    var step$1 = step;
    var p$1 = p;
    while(true) {
      var s$1 = _s;
      if (s$1 > f$1) {
        return false;
      } else if (p$1(s$1)) {
        return true;
      } else {
        _s = s$1 + step$1 | 0;
        continue ;
      }
    };
  } else {
    return false;
  }
}

function someBy(s, f, step, p) {
  return someByU(s, f, step, Curry.__1(p));
}

export {
  forEachU ,
  forEach ,
  everyU ,
  every ,
  everyByU ,
  everyBy ,
  someU ,
  some ,
  someByU ,
  someBy ,
  
}
/* No side effect */
