//@ts-check
var cp = require("child_process");

cp.execSync(`rescript`, { cwd: __dirname, encoding: "utf8" });
