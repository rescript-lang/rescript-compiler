//@ts-check
var cp = require("child_process");

cp.execSync(`../node_modules/.bin/rescript`, { cwd: __dirname, encoding: "utf8" });
