

var child_process = require('child_process')

child_process.execSync(`bsb -clean-world && bsb -make-world`, {cwd:__dirname, stdio:[0,1,2]})

