var p = require('child_process')
var assert = require('assert')
var fs = require('fs')
try {
    var output = p.spawnSync(`bsb -regen`, { shell: true, encoding: 'utf8' })

    assert.ok(output.stderr.match(/reserved package name/))
}
finally {
    
}