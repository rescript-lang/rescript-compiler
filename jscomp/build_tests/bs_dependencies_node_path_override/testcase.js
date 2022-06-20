//@ts-check
var assert = require('assert')
var path = require('path')
var p = require('child_process')
var fs = require('fs')
var output = p.spawnSync(
    `../node_modules/.bin/rescript clean -with-deps && ../node_modules/.bin/rescript build`,
    {
        cwd: __dirname,
        encoding: "utf8",
        stdio: ["pipe", "pipe", "pipe"],
        shell: true,
    })
fs.rmSync('overridden_node_modules/liba/lib', { recursive: true, force: true })

var u = require("./examples/test.js")
assert.equal(path.basename(u.v), 'demo.mldemo.ml')