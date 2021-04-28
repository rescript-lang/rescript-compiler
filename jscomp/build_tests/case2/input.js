var p = require("child_process");
var assert = require("assert");
var o = p.spawnSync(`rescript`, { encoding: "utf8", cwd: __dirname });

assert.ok(o.stderr.match(/different cases/).length > 0);
