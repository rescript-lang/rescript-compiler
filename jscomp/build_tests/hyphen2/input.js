var p  = require('child_process')

p.execSync(`bsb`,{cwd:__dirname,stdio:[0,1,2]})