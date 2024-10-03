var p = require("child_process");
var { rescript_exe } = require("#cli/bin_path");

p.execSync(rescript_exe);
