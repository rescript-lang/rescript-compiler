var child_process = require("child_process");
var { rescript_exe } = require("#cli/bin_path");

child_process.execSync(rescript_exe, { cwd: __dirname });
