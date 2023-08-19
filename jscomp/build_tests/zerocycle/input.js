var p = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
var out = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });
assert(out.status == 0);
