//@ts-check

var cp = require("child_process");
var assert = require("assert");
cp.execSync(`rescript build`, { cwd: __dirname, encoding: "utf8" });

assert.equal(require("./src/demo.bs").v, 5);
