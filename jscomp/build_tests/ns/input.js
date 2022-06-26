var child_process = require("child_process");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

child_process.execSync(rescript_exe, { cwd: __dirname });
