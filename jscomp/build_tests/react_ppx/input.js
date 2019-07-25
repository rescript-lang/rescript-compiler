
//@ts-check
var cp = require('child_process')

cp.execSync(`bsb`,{cwd:__dirname,stdio:[0,1,2]})