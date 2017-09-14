

var p = require('child_process')
var assert = require('assert')
p.execSync(`bsb`, {cwd : __dirname, stdio:[0,1,2]})

assert.ok(require('./src/demo.js').v === 4,'nested')