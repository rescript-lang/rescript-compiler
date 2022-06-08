var p = require("child_process");
var assert = require("assert");
var out = p.spawnSync(`rescript`, { encoding: "utf8", cwd: __dirname });
assert(out.status == 0)
