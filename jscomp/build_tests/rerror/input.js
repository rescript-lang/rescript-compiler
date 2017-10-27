var child_process = require('child_process')
var assert = require('assert')
var o = child_process.spawnSync(`bsb `,{cwd:__dirname, encoding:'utf8',shell:true})


// verify the output is in reason syntax
var u = o.stdout.match(/=>/g)
assert.ok(u.length === 2)