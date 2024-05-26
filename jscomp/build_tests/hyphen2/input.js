var p = require("child_process");
var { rescript_exe } = require("#cli/bin_path.js");

p.execSync(rescript_exe, { cwd: __dirname });
