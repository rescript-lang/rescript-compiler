#!/usr/bin/env node
var cp = require('child_process')
var fs = require('fs')
function run() {
    cp.execSync(`make clean`)
    if (fs.existsSync(`stdlib-406`)) {
        cp.execSync(`git clean -dfx stdlib-406`)
    }

    cp.execSync(`git clean -dfx stdlib-402 test others runtime`)
}

if(require.main === module){
    run()
}

exports.run = run