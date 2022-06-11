var p = require("child_process");

p.execSync(`../node_modules/.bin/rescript`, { cwd: __dirname, stdio: [0, 1, 2] });
