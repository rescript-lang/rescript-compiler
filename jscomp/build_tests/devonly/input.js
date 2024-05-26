//@ts-check
var cp = require("child_process");
var { rescript_exe } = require("#cli/bin_path.js");

cp.execSync(rescript_exe, { cwd: __dirname, encoding: "utf8" });
