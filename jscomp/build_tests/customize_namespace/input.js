
//@ts-check

var cp = require('child_process')
var assert = require('assert')
cp.execSync(`bsb -make-world`, {cwd : __dirname, encoding : 'utf8'})

assert.equal(require('./src/demo.bs').v,5)