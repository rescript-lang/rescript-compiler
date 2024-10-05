//@ts-check
var child_process = require("child_process");
var fs = require("fs");
var path = require("path");

var { rescript_exe } = require("#cli/bin_path");

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

console.log(child_process.execSync(rescript_exe, { encoding: "utf8" }));

var content =
  "" + fs.readFileSync(path.join(__dirname, "lib", "bs", ".sourcedirs.json"));

var assert = require("assert");

assert(JSON.parse(content).dirs.some(x => x.includes("📕annotation")));
