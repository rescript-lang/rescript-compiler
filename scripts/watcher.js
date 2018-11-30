'use strict';

var Process = require('process');
var Fs = require('fs');
var Path = require('path');
var Child_process = require('child_process');

var undefinedHeader = /* array */[];

function some(x) {
  if (x === undefined) {
    var block = /* tuple */[
      undefinedHeader,
      0
    ];
    block.tag = 256;
    return block;
  } else if (x !== null && x[0] === undefinedHeader) {
    var nid = x[1] + 1 | 0;
    var block$1 = /* tuple */[
      undefinedHeader,
      nid
    ];
    block$1.tag = 256;
    return block$1;
  } else {
    return x;
  }
}

function undefined_to_opt(x) {
  if (x === undefined) {
    return undefined;
  } else {
    return some(x);
  }
}

function valFromOption(x) {
  if (x !== null && x[0] === undefinedHeader) {
    var depth = x[1];
    if (depth === 0) {
      return undefined;
    } else {
      return /* tuple */[
              undefinedHeader,
              depth - 1 | 0
            ];
    }
  } else {
    return x;
  }
}
/* No side effect */

function getExn(x) {
  if (x !== undefined) {
    return valFromOption(x);
  } else {
    throw new Error("getExn");
  }
}
/* No side effect */

function iter(f, xs) {
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    f(xs[i]);
  }
  return /* () */0;
}
/* No side effect */

/* unsafeDeleteKey Not a pure module */

function putEnvVar(key, $$var) {
  Process.env[key] = $$var;
  return /* () */0;
}
/* Js_dict Not a pure module */

function findFile(_prev, _cwd, f) {
  while(true) {
    var cwd = _cwd;
    var prev = _prev;
    if (prev.length === cwd.length) {
      var str = " " + (String(f) + " not found ");
      throw new Error(str);
    } else if (Fs.existsSync(Path.join(cwd, f))) {
      return cwd;
    } else {
      _cwd = Path.dirname(cwd);
      _prev = cwd;
      continue ;
    }
  }}

function makeEventObj(param) {
  return {
          events: /* array */[],
          empty: (function () {
              var self = this ;
              var a = self.events;
              a.splice(0);
              return /* () */0;
            }),
          push: (function (a) {
              var self = this ;
              var xs = self.events;
              xs.push(a);
              return /* () */0;
            }),
          needRebuild: (function () {
              var self = this ;
              return self.events.length !== 0;
            }),
          currentEvents: (function () {
              var self = this ;
              return self.events;
            })
        };
}

function makeLock(param) {
  return {
          isBuilding: false,
          acquire: (function () {
              var self = this ;
              if (self.isBuilding) {
                return false;
              } else {
                self.isBuilding = true;
                return true;
              }
            }),
          release: (function () {
              var self = this ;
              self.isBuilding = false;
              return /* () */0;
            })
        };
}

function build(cmd, $$event, lock, idle) {
  if (lock.acquire()) {
    console.log(">>>> Start compiling");
    var events = $$event.currentEvents();
    console.log("Rebuilding since " + (String(events) + " "));
    $$event.empty();
    Child_process.spawn(cmd, ( [ ]), ( { "stdio" : "inherit" })).on("exit", (function () {
            console.log(">>>> Finish compiling");
            lock.release();
            if ($$event.needRebuild()) {
              return build(cmd, $$event, lock, idle);
            } else {
              return idle();
            }
          }));
    return /* () */0;
  } else {
    return 0;
  }
}

function buildWithShell(cmd, $$event, lock, idle) {
  if (lock.acquire()) {
    console.log(">>>> Start compiling");
    var events = $$event.currentEvents();
    console.log("Rebuilding since " + (String(events) + " "));
    $$event.empty();
    Child_process.spawn(cmd, ( [ ]), ( { "stdio" : "inherit", "shell" : true })).on("exit", (function () {
            console.log(">>>> Finish compiling");
            lock.release();
            require('./runtimeDeps.js').create();
            if ($$event.needRebuild()) {
              return build(cmd, $$event, lock, idle);
            } else {
              return idle();
            }
          }));
    return /* () */0;
  } else {
    return 0;
  }
}

function makeWatcher(file, onChange) {
  return /* record */[
          /* dir */file,
          /* watcher */Fs.watch(file, onChange)
        ];
}
/* fs Not a pure module */

getExn(undefined_to_opt(typeof (__dirname) === "undefined" ? undefined : (__dirname)));

var cwd = Process.cwd();

var bsconfig = "bsconfig.json";

var root = Fs.existsSync(Path.join(cwd, bsconfig)) ? cwd : findFile(cwd, Path.dirname(cwd), bsconfig);

console.log(" Root: " + (String(root) + " "));

var jscomp = Path.join(root, "jscomp");

var lock = makeLock(/* () */0);

var events = makeEventObj(/* () */0);

var command = "./watch-build.sh";

function exec(param) {
  return buildWithShell(command, events, lock, (function () {
                return /* () */0;
              }));
}

function watch(dir) {
  return makeWatcher(dir, (function (_event, fileName) {
                if (fileName.endsWith(".ml") || fileName.endsWith(".mli") || fileName.endsWith(".cppo") || fileName.endsWith(".js") || fileName === "Makefile" || fileName === "Makefile.shared") {
                  return exec(/* () */0);
                } else {
                  return 0;
                }
              }));
}

putEnvVar("BS_VSCODE", "1");

var match = Process.argv.slice(2);

var exit = 0;

if (match.length !== 1) {
  exit = 1;
} else {
  var match$1 = match[0];
  if (match$1 === "-build") {
    Child_process.spawn(command, ( [ ]), ( { "stdio" : "inherit", "shell" : true }));
  } else {
    exit = 1;
  }
}

if (exit === 1) {
  iter((function (x) {
          watch(Path.join(jscomp, x));
          return /* () */0;
        }), /* array */[
        "core",
        "syntax",
        "ext",
        "depends",
        "others",
        "ounit",
        "ounit_tests",
        "test",
        "runtime",
        "xwatcher",
        "bsb",
        "common",
        "super_errors",
        ".",
        "stubs",
        "stdlib-406"
      ]);
  exec(/* () */0);
}
/*  Not a pure module */
