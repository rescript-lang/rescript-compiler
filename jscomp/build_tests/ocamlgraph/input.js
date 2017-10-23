//@ts-check

var p = require('child_process')
var path = require('path')
var fs = require('fs')
var assert = require('assert')
p.execSync(`npm link bs-platform && bsb`,{cwd: __dirname})
var check_file = path.join(__dirname,'tests','check.bs.js')
var xs = p.spawnSync('node',
    [check_file],
    { encoding: 'utf8'}
).output

var ys= fs.readFileSync(path.join(__dirname,'tests','check.ref'),'utf8')
// console.log(fs.readFileSync(check_file,'utf8'))
// console.log(`output begin ...`)
// console.log(xs)
// console.log(`output end...`)
assert.equal(xs[1],ys)


var xs = p.spawnSync('node',
    [path.join(__dirname,'tests','test_topsort.bs.js')],
    { encoding: 'ascii'}
).output
var ys = fs.readFileSync(path.join(__dirname,'tests','test_topsort.ref'),'ascii')
assert.equal(
    xs[1] , ys
)

var xs = p.spawnSync('node',
    [path.join(__dirname,'tests','test_johnson.bs.js')],
    { encoding: 'ascii'}
).output
var ys = fs.readFileSync(path.join(__dirname,'tests','test_johnson.ref'),'ascii')
assert.equal(
    xs[1], ys
)


// debugger