//@ts-check

process.env["BS_VSCODE"] = "1";

var fs = require("fs");
var path = require("path");
var cp = require("child_process");
var sourceDirs = [
  "ext",
  "common",
  "frontend",
  "depends",
  "core",
  "super_errors",
  "outcome_printer",
  "bsb",
  "main",
  "others",
  "stdlib-406",
  "runtime",
  "test",
  "ounit_tests",
  "bsb_helper",
  "refmt",
  "napkin",
  "js_parser",
];

var buildAppending = false;
var isBuilding = false;

var jscompDir = path.join("..", "jscomp");
/**
 *
 * @param {Date} d
 */
function showDate(d) {
  return `${d.getHours()}:${d.getMinutes()}:${d.getSeconds()}`;
}
function rebuild() {
  if (isBuilding) {
    buildAppending = true;
  } else {
    console.log(">>>> Start compiling");
    isBuilding = true;

    cp.fork(path.join(__dirname, "ninja.js"), ["build"]).on(
      "exit",
      buildFinished
    );
  }
}
/**
 *
 * @param {number} code
 * @param {string} signal
 */
function buildFinished(code, signal) {
  isBuilding = false;
  if (buildAppending) {
    buildAppending = false;
    rebuild();
  } else {
    if (code !== 0) {
      // console.log(`File "package.json", line 1, characters 1-1:`);
      // console.log(`Error: Failed to build ${showDate(new Date())}`);
    }
    console.log(">>>> Finish compiling (options: R|clean|config)");
    // TODO: check ninja exit error code
    if (code === 0) {
      // This is not always correct
      // ninjaFile.updateDev();
      // This file is cached
      cp.fork(path.join(__dirname, "ninja.js"), ["-dev"]);
    }
  }
}
/**
 *
 * @param {string} eventType
 * @param {string} filename
 */
function onSourceChange(eventType, filename) {
  // console.log('event ', eventType,filename)
  if (
    filename.endsWith(".ml") ||
    filename.endsWith(".mli") ||
    filename.endsWith(".json") ||
    filename.endsWith(".re") || 
    filename.endsWith(".rei") ||
    filename.endsWith(".res") ||
    filename.endsWith(".resi")
  ) {
    rebuild();
  }
}

sourceDirs.forEach(x => {
  fs.watch(path.join(jscompDir, x), "utf8", onSourceChange);
});

fs.watch(path.join("..", "package.json"), "utf8", onSourceChange);

rebuild();

var readline = require("readline");
readline
  .createInterface({
    input: process.stdin,
    output: process.stdout
  })
  .on("line", input => {
    switch (input.toLowerCase()) {
      case "r":
        rebuild();
        break;
      case "config":
        if (isBuilding) {
          console.log(`it's building`);
        } else {
          isBuilding = true;
          cp
            .fork(path.join(__dirname, "ninja.js"), ["config"])
            .on("close", () => {
              isBuilding = false;
              rebuild();
            });
        }
        break;
      case "clean":
        if (isBuilding) {
          console.log(`it's building`);
        } else {
          isBuilding = true;
          cp
            .fork(path.join(__dirname, "ninja.js"), ["cleanbuild"])
            .on("close", () => {
              isBuilding = false;
            });
        }

        break;
    }
  });
