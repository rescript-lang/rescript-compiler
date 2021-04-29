var p = require("child_process");
var assert = require("assert");
p.spawnSync(`rescript`, { encoding: "utf8", cwd: __dirname });
