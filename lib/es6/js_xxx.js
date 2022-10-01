

import * as Js_promise from "./js_promise.js";

var then = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

var then_ = Js_promise.then_;

export {
  then_ ,
  then ,
  $$catch ,
}
/* No side effect */
