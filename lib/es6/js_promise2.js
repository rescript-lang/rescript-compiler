


var then = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

var then_ = then;

export {
  then ,
  $$catch ,
  then_ ,
}
/* No side effect */
