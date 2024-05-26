//@ts-check
const cp = require("child_process");
var { rescript_exe } = require("#cli/bin_path.js");

cp.execSync(`${rescript_exe} clean`, { cwd: __dirname });
cp.execSync(`${rescript_exe}`, { cwd: __dirname });
