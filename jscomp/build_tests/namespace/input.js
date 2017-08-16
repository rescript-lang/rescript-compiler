

var child_process = require('child_process')

child_process.execSync(`bsb -clean-world && bsb -make-world`, {cwd:__dirname, stdio:[0,1,2]})

var x = require('./src/demo.js')
var assert = require('assert')

assert.equal(x.v, 3 )