'use strict';

import * as Fs            from "fs";
import * as Path          from "path";
import * as Process       from "process";
import * as Bs_array      from "../../lib/es6/bs_array.js";
import * as Bs_option     from "../../lib/es6/bs_option.js";
import * as Js_primitive  from "../../lib/es6/js_primitive.js";
import * as Node_process  from "../../lib/es6/node_process.js";
import * as Xwatcher_util from "./xwatcher_util.js";

Bs_option.getExn(Js_primitive.undefined_to_opt(typeof (__dirname) === "undefined" ? undefined : (__dirname)));

var cwd = Process.cwd();

var bsconfig = "bsconfig.json";

var root = Fs.existsSync(Path.join(cwd, bsconfig)) ? cwd : Xwatcher_util.findFile(cwd, Path.dirname(cwd), bsconfig);

console.log(" Root: " + (String(root) + " "));

var jscomp = Path.join(root, "jscomp");

var lock = Xwatcher_util.makeLock(/* () */0);

var events = Xwatcher_util.makeEventObj(/* () */0);

function exec() {
  return Xwatcher_util.buildWithShell("make -r -j5   check", events, lock, function () {
              return /* () */0;
            });
}

function watch(dir) {
  return Xwatcher_util.makeWatcher(dir, function (_, fileName) {
              if (fileName.endsWith(".ml") || fileName.endsWith(".mli") || fileName.endsWith(".cppo")) {
                return exec(/* () */0);
              } else {
                return 0;
              }
            });
}

Node_process.putEnvVar("BS_VSCODE", "1");

Bs_array.iter(function (x) {
      watch(Path.join(jscomp, x));
      return /* () */0;
    }, /* array */[
      "core",
      "syntax",
      "ext",
      "depends"
    ]);

exec(/* () */0);

export {
  
}
/*  Not a pure module */
