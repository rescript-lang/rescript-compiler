


var then = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

export {
  then ,
  $$catch ,
}
/* No side effect */
