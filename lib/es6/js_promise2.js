


var then = (function(p, cont) {
    return Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      return Promise.resolve(p).catch(cont)
    });

export default {
  then ,
  $$catch ,
}
/* No side effect */
