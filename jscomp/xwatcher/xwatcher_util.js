

import * as Fs from "fs";
import * as Path from "path";
import * as Child_process from "child_process";

function getWatchFiles(file) {
  if (Fs.existsSync(file)) {
    return Fs.readFileSync(file, "utf8").split("\n").filter((function (x) {
                  return +(x.trim().length !== 0);
                }));
  } else {
    return /* array */[];
  }
}

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
  };
}

function makeEventObj() {
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
              return +(self.events.length !== 0);
            }),
          currentEvents: (function () {
              var self = this ;
              return self.events;
            })
        };
}

function makeLock() {
  return {
          isBuilding: /* false */0,
          acquire: (function () {
              var self = this ;
              if (self.isBuilding) {
                return /* false */0;
              } else {
                self.isBuilding = /* true */1;
                return /* true */1;
              }
            }),
          release: (function () {
              var self = this ;
              self.isBuilding = /* false */0;
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

export {
  makeLock ,
  makeEventObj ,
  build ,
  buildWithShell ,
  findFile ,
  getWatchFiles ,
  makeWatcher ,
  
}
/* fs Not a pure module */
