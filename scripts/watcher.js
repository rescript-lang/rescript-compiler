'use strict';

var Fs = require('fs');
var Path = require('path');
var Process = require('process');
var Child_process = require('child_process');

function getExn(x) {
  if (x) {
    return x[0];
  } else {
    throw new Error("Bs_option.getExn");
  }
}


/* No side effect */

var invalid_argument = /* tuple */[
  "Invalid_argument",
  -3
];

invalid_argument.tag = 248;


/*  Not a pure module */

/* No side effect */

/* No side effect */

/* No side effect */

/* No side effect */

/* stdin Not a pure module */

/* No side effect */

/* imul Not a pure module */

/* repeat Not a pure module */

/* two_ptr_32_dbl Not a pure module */

/* float_of_string Not a pure module */

/* No side effect */

/* No side effect */

/* not_implemented Not a pure module */

/* No side effect */

/* No side effect */

/* No side effect */

function iter$$1(f, xs) {
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    f(xs[i]);
  }
  return /* () */0;
}


/* No side effect */

function undefined_to_opt(x) {
  if (x === undefined) {
    return /* None */0;
  } else {
    return /* Some */[x];
  }
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
    var cwd$$1 = _cwd;
    var prev = _prev;
    if (prev.length === cwd$$1.length) {
      var str = " " + (String(f) + " not found ");
      throw new Error(str);
    } else if (Fs.existsSync(Path.join(cwd$$1, f))) {
      return cwd$$1;
    } else {
      _cwd = Path.dirname(cwd$$1);
      _prev = cwd$$1;
      continue ;
      
    }
  }
}

function makeEventObj() {
  return {
          events: /* array */[],
          empty: function () {
            var self = this;
            var a = self.events;
            a.splice(0);
            return /* () */0;
          },
          push: function (a) {
            var self = this;
            var xs = self.events;
            xs.push(a);
            return /* () */0;
          },
          needRebuild: function () {
            var self = this;
            return +(self.events.length !== 0);
          },
          currentEvents: function () {
            var self = this;
            return self.events;
          }
        };
}

function makeLock() {
  return {
          isBuilding: /* false */0,
          acquire: function () {
            var self = this;
            if (self.isBuilding) {
              return /* false */0;
            } else {
              self.isBuilding = /* true */1;
              return /* true */1;
            }
          },
          release: function () {
            var self = this;
            return self.isBuilding = /* false */0;
          }
        };
}

function build(cmd, $$event, lock, idle) {
  if (lock.acquire()) {
    console.log(">>>> Start compiling");
    var events = $$event.currentEvents();
    console.log("Rebuilding since " + (String(events) + " "));
    $$event.empty();
    Child_process.spawn(cmd, ( [ ]), ( { "stdio" : "inherit" })).on("exit", function () {
          console.log(">>>> Finish compiling");
          lock.release();
          if ($$event.needRebuild()) {
            return build(cmd, $$event, lock, idle);
          } else {
            return idle();
          }
        });
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
    Child_process.spawn(cmd, ( [ ]), ( { "stdio" : "inherit", "shell" : true })).on("exit", function () {
          console.log(">>>> Finish compiling");
          lock.release();
          if ($$event.needRebuild()) {
            return build(cmd, $$event, lock, idle);
          } else {
            return idle();
          }
        });
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


/*  Not a pure module */

getExn(undefined_to_opt(typeof (__dirname) === "undefined" ? undefined : (__dirname)));

var cwd$1 = Process.cwd();

var bsconfig = "bsconfig.json";

var root = Fs.existsSync(Path.join(cwd$1, bsconfig)) ? cwd$1 : findFile(cwd$1, Path.dirname(cwd$1), bsconfig);

console.log(" Root: " + (String(root) + " "));

var jscomp = Path.join(root, "jscomp");

var lock = makeLock(/* () */0);

var events = makeEventObj(/* () */0);

var command = "./watch-build.sh";

function exec() {
  return buildWithShell(command, events, lock, function () {
              return /* () */0;
            });
}

function watch$1(dir) {
  return makeWatcher(dir, function (_, fileName) {
              if (fileName.endsWith(".ml") || fileName.endsWith(".mli") || fileName.endsWith(".cppo") || fileName.endsWith(".js") || fileName === "Makefile" || fileName === "Makefile.shared" || fileName.endsWith(".mll")) {
                return exec(/* () */0);
              } else {
                return 0;
              }
            });
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
  iter$$1(function (x) {
        watch$1(Path.join(jscomp, x));
        return /* () */0;
      }, /* array */[
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
        "outcome_printer",
        ".",

      ]);
  exec(/* () */0);
}


/*  Not a pure module */
