var p = require("child_process");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

p.execSync(rescript_exe, { cwd: __dirname });
