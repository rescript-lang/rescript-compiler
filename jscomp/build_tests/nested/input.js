

var p = require('child_process')
var assert = require('assert')
var fs = require('fs')
var path = require('path')
p.execSync(`bsb`, {cwd : __dirname, stdio:[0,1,2]})

var content = fs.readFileSync(path.join(__dirname,'src','demo.js'),'utf8')

assert.ok(content.match(/A00_a1_main/g).length === 3 )
assert.ok(content.match(/B00_b1_main/g).length === 3 )
assert.ok(content.match(/A0_main/g).length === 2)
assert.ok(content.match(/a0_main/g).length === 1)
assert.ok(content.match(/B0_main/g).length === 2)
assert.ok(content.match(/b0_main/g).length === 1)


assert.ok(require('./src/demo.js').v === 4,'nested')


