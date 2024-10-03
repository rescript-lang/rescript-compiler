var p = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");
var out = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });
assert(out.status == 0);
