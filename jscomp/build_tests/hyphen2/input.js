var p = require("child_process");

p.execSync(`rescript`, { cwd: __dirname, stdio: [0, 1, 2] });
