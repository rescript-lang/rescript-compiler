//@ts-check
var cp = require("child_process");

cp.execSync(`rescript`, { cwd: __dirname, stdio: [0, 1, 2] });
