

import * as Js_promise from "./js_promise.js";

var then_ = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var catch_ = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

var $$catch = Js_promise.$$catch;

export {
  $$catch ,
  then_ ,
  catch_ ,
}
/* No side effect */
