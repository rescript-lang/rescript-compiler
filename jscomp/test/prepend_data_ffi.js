'use strict';


var v1 = {
  stdio: "inherit",
  v: 3
};

var v2 = {
  stdio: 1,
  v: 2
};

process.on("exit", (function (exit_code) {
        return String(exit_code);
      }));

process.on(1, (function (param) {
        
      }));

process.on((function (i) {
        return String(i);
      }), "exit");

process.on((function (i) {
        return String(i);
      }), 1);

xx(3, 3, "xxx", "a", "b");

function f(x) {
  x.xx(110, [
        1,
        2,
        3
      ]);
  x.xx(111, 3, "xxx", [
        1,
        2,
        3
      ]);
  x.xx(112, 3, "xxx", 1, 2, 3);
  x.xx(113, 3, "xxx", 0, "b", 1, 2, 3, 4, 5);
  x.xx(114, 3, true, false, "你好", ["你好",1,2,3], [{ "arr" : ["你好",1,2,3], "encoding" : "utf8"}], [{ "arr" : ["你好",1,2,3], "encoding" : "utf8"}], "xxx", 0, "yyy", "b", 1, 2, 3, 4, 5);
  
}

process.on("exit", (function (exit_code) {
        console.log("error code: " + String(exit_code));
        
      }));

function register(p) {
  p.on("exit", (function (i) {
          console.log(i);
          
        }));
  
}

var config = {
  stdio: "inherit",
  cwd: "."
};

exports.v1 = v1;
exports.v2 = v2;
exports.f = f;
exports.register = register;
exports.config = config;
/*  Not a pure module */
