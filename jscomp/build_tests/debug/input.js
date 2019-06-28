//@ts-check

var cp = require('child_process')
cp.execSync(`bsb`,{encoding : 'ascii'})
require('./src/demo.bs.js')