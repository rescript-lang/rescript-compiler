var child_process = require("child_process");
var fs = require("fs");
var path = require("path");
var assert = require("assert");

child_process.execSync(`../node_modules/.bin/rescript clean -with-deps && ../node_modules/.bin/rescript build`, {
  cwd: __dirname,
  stdio: [0, 1, 2],
});

var x = require("./src/demo.bs.js");
assert.equal(x.v, 42);

var merlin = fs.readFileSync(path.join(__dirname, ".merlin"), "utf8");
assert.ok(merlin.includes("-open Foo_bar"));
