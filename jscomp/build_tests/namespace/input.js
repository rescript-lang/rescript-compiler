

var child_process = require('child_process')
var fs = require('fs') 
var path = require('path')
child_process.execSync(`bsb -clean-world && bsb -make-world`, {cwd:__dirname, stdio:[0,1,2]})

var x = require('./src/demo.bs.js')
var assert = require('assert')

assert.equal(x.v, 3 )

var merlin = fs.readFileSync(path.join(__dirname,'.merlin'), 'utf8')
var warn_flag = '-w -40+6+7'
assert.ok(merlin.includes('-open'))
assert.ok(merlin.includes(warn_flag))

var testWarnError = /warnings\s*=\s*[^\r\n]*-warn-error/

function hasWarnError(file){
    var content = fs.readFileSync(file,'utf8')
    return testWarnError.test(content)
}

var content = fs.readFileSync(path.join(__dirname,'lib','bs','build.ninja'))
assert.ok(testWarnError.test(content))
assert.ok(content.includes(warn_flag))
assert.ok(!hasWarnError(path.join(__dirname,'node_modules','liba','lib','bs','build.ninja')))
assert.ok(!hasWarnError(path.join(__dirname,'node_modules','libb','lib','bs','build.ninja')))
