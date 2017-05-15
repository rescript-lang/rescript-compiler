'use strict';

var Path = require('path');
var Process = require('process');
var Fs = require('fs');
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

function filterInPlace(p, a) {
  var i = 0;
  var j = 0;
  while(i < a.length) {
    var v = a[i];
    if (p(v)) {
      a[j] = v;
      j = j + 1 | 0;
    }
    i = i + 1 | 0;
  }
  a.splice(j);
  return /* () */0;
}

function memByRef(x, xs) {
  return +(xs.indexOf(x) >= 0);
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

function getWatchFiles(file) {
  if (Fs.existsSync(file)) {
    return Fs.readFileSync(file, "utf8").split("\n").filter(function (x) {
                return +(x.trim().length !== 0);
              });
  } else {
    return /* array */[];
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

function makeWatcher(file, onChange) {
  return /* record */[
          /* dir */file,
          /* watcher */Fs.watch(file, onChange)
        ];
}


/*  Not a pure module */

var sourceDirs = Path.join("lib", "bs", ".sourcedirs");

var lock = makeLock(/* () */0);

var events = makeEventObj(/* () */0);

var watchers = /* array */[];

var source_dirname = getExn(undefined_to_opt(typeof (__dirname) === "undefined" ? undefined : (__dirname)));

var bsb = Path.join(source_dirname, "bsb.exe");

var bsconfig = "bsconfig.json";

function onChange(eventType, fileName) {
  console.log("Event " + (String(eventType) + (" " + (String(fileName) + ""))));
  events.push(/* tuple */[
        eventType,
        fileName
      ]);
  return build(bsb, events, lock, function () {
              return idle(/* () */0);
            });
}

function idle() {
  var watchFiles = getWatchFiles(sourceDirs);
  filterInPlace(function (param) {
        var dir = param[/* dir */0];
        if (dir === bsconfig || memByRef(dir, watchFiles)) {
          return /* true */1;
        } else {
          console.log(" " + (String(dir) + " is no longer watched"));
          param[/* watcher */1].close();
          return /* false */0;
        }
      }, watchers);
  watchFiles.forEach(function (dir) {
        if (watchers.some(function (param) {
                return +(param[/* dir */0] === dir);
              })) {
          return 0;
        } else {
          console.log("watching dir " + (String(dir) + " now "));
          var x = makeWatcher(dir, onChange);
          watchers.push(x);
          return /* () */0;
        }
      });
  return /* () */0;
}

putEnvVar("BS_VSCODE", "1");

var x = makeWatcher(bsconfig, onChange);

watchers.push(x);

build(bsb, events, lock, function () {
      return idle(/* () */0);
    });


/* sourceDirs Not a pure module */
