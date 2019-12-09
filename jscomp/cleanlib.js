#!/usr/bin/env node
var cp = require('child_process')
var fs = require('fs')
function run() {
    cp.execSync(`make clean`)
    cp.execSync(`git clean -dfx stdlib-406 test others runtime`)
}

if(require.main === module){
    run()
}

exports.run = run