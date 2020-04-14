var child_process = require('child_process')

var assert = require('assert').strict

var output = child_process.spawnSync('bsb -clean-world && bsb -make-world',
    {
        cwd: __dirname,
        encoding: 'utf8',
        stdio : ['inherit','inherit','pipe'],
        shell : true
    }
)

// Should warn the user about the deprecation,
assert.match(output.stderr, /top-level 'suffix' field is deprecated/)

// ... but still respect it
assert.equal(output.status, 0)
assert.ok(require('./demo.bs'))
