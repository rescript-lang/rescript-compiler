//@ts-check
var assert = require("assert");
var path = require("path");
var p = require("child_process");
p.execSync(`rescript build`, { cwd: __dirname, shell: true, encoding: "utf8" });
var u = require("./examples/test.js");
assert.equal(path.basename(u.v), "demo.mldemo.ml");
