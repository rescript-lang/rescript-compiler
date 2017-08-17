

var child_process = require('child_process')

var output = child_process.spawnSync(
    `bsb -clean-world && bsb -make-world`, 
    {cwd:__dirname, shell: true, encoding : 'utf8'})


var assert = require('assert')
assert.ok(output.stderr.match(/IGNORED/))


