//@ts-check
var cp = require("child_process");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

cp.execSync(rescript_exe, { cwd: __dirname, encoding: "utf8" });
