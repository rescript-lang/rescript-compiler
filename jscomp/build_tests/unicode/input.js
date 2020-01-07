//@ts-check
var child_process = require("child_process");

console.log(child_process.execSync(`bsb`, { encoding: "utf8" }));

var fs = require("fs");
var path = require("path");
var content =
  "" + fs.readFileSync(path.join(__dirname, "lib", "bs", ".sourcedirs.json"));

var assert = require("assert");

assert(JSON.parse(content).dirs.some(x => x.includes("📕annotation")));
