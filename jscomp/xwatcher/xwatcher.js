

import * as Path from "path";
import * as Js_option from "../../lib/es6/js_option.js";
import * as Js_vector from "../../lib/es6/js_vector.js";
import * as Js_primitive from "../../lib/es6/js_primitive.js";
import * as Node_process from "../../lib/es6/node_process.js";
import * as Xwatcher_util from "./xwatcher_util.js";

var sourceDirs = Path.join("lib", "bs", ".sourcedirs.json");

var lock = Xwatcher_util.makeLock(/* () */0);

var events = Xwatcher_util.makeEventObj(/* () */0);

var watchers = /* array */[];

var source_dirname = Js_option.getExn(Js_primitive.undefined_to_opt(typeof (__dirname) === "undefined" ? undefined : (__dirname)));

var bsb = Path.join(source_dirname, "bsb.exe");

var bsconfig = "bsconfig.json";

function onChange(eventType, fileName) {
  console.log("Event " + (String(eventType) + (" " + (String(fileName) + ""))));
  events.push(/* tuple */[
        eventType,
        fileName
      ]);
  return Xwatcher_util.build(bsb, events, lock, (function () {
                return idle(/* () */0);
              }));
}

function idle() {
  var watchFiles = Xwatcher_util.getWatchFiles(sourceDirs);
  Js_vector.filterInPlace((function (param) {
          var dir = param[/* dir */0];
          if (dir === bsconfig || Js_vector.memByRef(dir, watchFiles)) {
            return true;
          } else {
            console.log(" " + (String(dir) + " is no longer watched"));
            param[/* watcher */1].close();
            return false;
          }
        }), watchers);
  watchFiles.forEach((function (dir) {
          if (watchers.some((function (param) {
                    return param[/* dir */0] === dir;
                  }))) {
            return 0;
          } else {
            console.log("watching dir " + (String(dir) + " now "));
            var x = Xwatcher_util.makeWatcher(dir, onChange);
            watchers.push(x);
            return /* () */0;
          }
        }));
  return /* () */0;
}

Node_process.putEnvVar("BS_VSCODE", "1");

var x = Xwatcher_util.makeWatcher(bsconfig, onChange);

watchers.push(x);

Xwatcher_util.build(bsb, events, lock, (function () {
        return idle(/* () */0);
      }));

export {
  
}
/* sourceDirs Not a pure module */
