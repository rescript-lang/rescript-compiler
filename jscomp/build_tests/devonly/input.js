
//@ts-check
var cp = require("child_process");

cp.execSync(`bsb`,{cwd:__dirname,encoding:'utf8'})
