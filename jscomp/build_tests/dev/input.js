//@ts-check
var assert = require('assert')
var path = require('path')
var p = require('child_process')
p.execSync(`bsb -make-world`, {cwd:__dirname,shell:true,encoding:'utf8'})
var u = require("./examples/test.js")
assert.ok(path.basename(u.v)==='demo.ml')