//@ts-check
var cp = require("child_process");
var assert = require("assert");

var output = cp.spawnSync(`rescript`, { encoding: "utf8", shell: true });

assert(/dependency cycle/.test(output.stdout));
