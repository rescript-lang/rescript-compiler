var child_process = require('child_process')
var assert = require('assert')

var out = child_process.spawnSync(`bsb`,{encoding : 'utf8'}) 

if(out.status !== '' ){
    assert.fail(out.stdout + out.stderr)
}