var p = require("child_process");
var { rescript_exe } = require("rescript/bin_path");

p.execSync(rescript_exe, { cwd: __dirname });
