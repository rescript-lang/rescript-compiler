var child_process = require('child_process')

child_process.execSync(`bsb`,
    { cwd: __dirname, encoding: 'utf8', stdio: [0, 1, 2] })