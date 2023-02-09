


var then = (function(p, cont) {
    return Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      return Promise.resolve(p).catch(cont)
    });

export {
  then ,
  $$catch ,
}
/* No side effect */
