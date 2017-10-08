'use strict';

import * as Fs            from "fs";
import * as Path          from "path";
import * as Process       from "process";
import * as Js_option     from "../../lib/es6/js_option.js";
import * as Js_vector     from "../../lib/es6/js_vector.js";
import * as Js_primitive  from "../../lib/es6/js_primitive.js";
import * as Node_process  from "../../lib/es6/node_process.js";
import * as Xwatcher_util from "./xwatcher_util.js";
import * as Child_process from "child_process";

Js_option.getExn(Js_primitive.undefined_to_opt(typeof (__dirname) === "undefined" ? undefined : (__dirname)));

var cwd = Process.cwd();

var bsconfig = "bsconfig.json";

var root = Fs.existsSync(Path.join(cwd, bsconfig)) ? cwd : Xwatcher_util.findFile(cwd, Path.dirname(cwd), bsconfig);

console.log(" Root: " + (String(root) + " "));

var jscomp = Path.join(root, "jscomp");

var lock = Xwatcher_util.makeLock(/* () */0);

var events = Xwatcher_util.makeEventObj(/* () */0);

var command = "./watch-build.sh";

function exec() {
  return Xwatcher_util.buildWithShell(command, events, lock, (function () {
                return /* () */0;
              }));
}

function watch(dir) {
  return Xwatcher_util.makeWatcher(dir, (function (_, fileName) {
                if (fileName.endsWith(".ml") || fileName.endsWith(".mli") || fileName.endsWith(".cppo") || fileName.endsWith(".js") || fileName === "Makefile" || fileName === "Makefile.shared") {
                  return exec(/* () */0);
                } else {
                  return 0;
                }
              }));
}

Node_process.putEnvVar("BS_VSCODE", "1");

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
  Js_vector.iter((function (x) {
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
        "."
      ]);
  exec(/* () */0);
}

export {
  
}
/*  Not a pure module */
