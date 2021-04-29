var child_process = require("child_process");

child_process.execSync(`rescript`, { cwd: __dirname, stdio: [0, 1, 2] });
