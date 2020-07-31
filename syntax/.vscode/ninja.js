#!/usr/bin/env node
var path = require("path");
var cp = require("child_process");
var jscompDir = path.join(__dirname, "..");
var vendorNinjaPath = "make";
function main() {
  var subcommand = process.argv[2];
  switch (subcommand) {
    case "build":
      cp.execSync(vendorNinjaPath, {
        encoding: "utf8",
        cwd: jscompDir,
        stdio: [0, 1, 2],
      });
      //   cp.execSync(`make depend`, {
      //     cwd: jscompDir,
      //     stdio: [0, 1, 2],
      //   });
      break;
    case "clean":
      cp.execSync(`make clean`, {
        cwd: jscompDir,
        stdio: [0, 1, 2],
      });
      break;
    case "config":
      cp.execSync(`make depend`, {
        cwd: jscompDir,
        stdio: [0, 1, 2],
      });
      break;
    case "cleanbuild":
      cp.execSync(`make clean && make depend && make -j9`, {
        cwd: jscompDir,
        stdio: [0, 1, 2],
      });
      break;
    case "help":
      console.log(`supported subcommands:
  [exe] config        
  [exe] build
  [exe] cleanbuild
  [exe] help
  [exe] clean
          `);
      break;
    default:
      break;
  }
}

main();
