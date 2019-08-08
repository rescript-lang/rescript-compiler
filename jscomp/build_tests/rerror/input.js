var child_process = require('child_process')
var assert = require('assert')
var o = child_process.spawnSync(`bsb `,{cwd:__dirname, encoding:'utf8',shell:true})


// verify the output is in reason syntax
var u = o.stdout.match(/=>/g)

var lines = o.stdout.split('\n').map(x=>x.trim()).filter(Boolean)
assert.ok(/rerror\/src\/demo.re 3:23-25/.test(lines[4]))
assert.equal(lines[3],"We've found a bug for you!")
assert.ok(u.length === 2)