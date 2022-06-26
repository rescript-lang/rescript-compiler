//@ts-check
var assert = require('assert')
var path = require('path')
var p = require('child_process')
var fs = require('fs')
var rescript_exe = require("../../../scripts/bin_path").rescript_exe
var output = p.spawnSync(
    `${rescript_exe} clean -with-deps && ${rescript_exe} build`,
    {
        cwd: __dirname,
        encoding: "utf8",
        stdio: ["pipe", "pipe", "pipe"],
        shell: true,
    })
fs.rmSync('overridden_node_modules/liba/lib', { recursive: true, force: true })

var u = require("./examples/test.js")
assert.equal(path.basename(u.v), 'demo.mldemo.ml')
