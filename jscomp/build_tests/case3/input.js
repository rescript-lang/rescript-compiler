var p = require("child_process");

var o = p.spawnSync(`bsb`, { encoding: "utf8", cwd: __dirname });
