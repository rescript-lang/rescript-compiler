// Generated by ReScript, PLEASE EDIT WITH CARE


function f() {
  let n = 0;
  while ((() => {
      let fib = x => {
        if (x !== 0 && x !== 1) {
          return fib(x - 1 | 0) + fib(x - 2 | 0) | 0;
        } else {
          return 1;
        }
      };
      return fib(n) > 10;
    })()) {
    console.log(n.toString());
    n = n + 1 | 0;
  };
}

function ff() {
  while ((() => {
      let b = 9;
      return (3 + b | 0) > 10;
    })()) {
    
  };
}

export {
  f,
  ff,
}
/* No side effect */
