// Generated by ReScript, PLEASE EDIT WITH CARE


let v1 = {
  stdio: "inherit",
  v: 3
};

let v2 = {
  stdio: 1,
  v: 2
};

process.on("exit", exit_code => exit_code.toString());

process.on(1, param => {});

process.on(i => i.toString(), "exit");

process.on(i => i.toString(), 1);

xx(3, 3, "xxx", "a", "b");

function f(x) {
  x.xx(72, [
    1,
    2,
    3
  ]);
  x.xx(73, 3, "xxx", [
    1,
    2,
    3
  ]);
  x.xx(74, 3, "xxx", 1, 2, 3);
  x.xx(75, 3, "xxx", 0, "b", 1, 2, 3, 4, 5);
  x.xx(76, 3, true, false, "你好",  ["你好",1,2,3] ,  [{ "arr" : ["你好",1,2,3], "encoding" : "utf8"}] ,  [{ "arr" : ["你好",1,2,3], "encoding" : "utf8"}] , "xxx", 0, "yyy", "b", 1, 2, 3, 4, 5);
}

process.on("exit", exit_code => {
  console.log("error code: %d", exit_code);
});

function register(p) {
  p.on("exit", i => {
    console.log(i);
  });
}

let config = {
  stdio: "inherit",
  cwd: "."
};

export {
  v1,
  v2,
  f,
  register,
  config,
}
/*  Not a pure module */
