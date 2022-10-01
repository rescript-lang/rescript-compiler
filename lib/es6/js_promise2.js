


var then_ = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

export {
  then_ ,
  $$catch ,
}
/* No side effect */
