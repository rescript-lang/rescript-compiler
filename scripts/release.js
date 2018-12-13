#!/usr/bin/env node 
//@ts-check

var path = require('path')
var fs = require('fs')
var cp = require('child_process')
var root = path.join(__dirname,'..')
var libJsDir = path.join(root,'lib','js')
var jscompDir = path.join(root,'jscomp')
function run() {


    for (let file of fs.readdirSync(libJsDir)) {
        if (file.endsWith('.js')) {
            fs.unlinkSync(path.join(libJsDir, file))
        }
    }


    cp.execSync(`make clean`,
        { cwd: jscompDir, encoding: 'utf8', stdio: [0, 1, 2] })
    cp.execSync(`make themes`,
        { cwd: jscompDir, encoding: 'utf8', stdio: [0, 1, 2] })
    cp.execSync(`make -j9 check`,
        { cwd: jscompDir, encoding: 'utf8', stdio: [0, 1, 2] })
    cp.execSync(`BS_DEBUG=false make -j9 force-snapshotml`,
        { cwd: jscompDir, encoding: 'utf8', stdio: [0, 1, 2] })
    cp.execSync(`make  -j9 world`,
        { cwd: root, stdio: [0, 1, 2] })

}
if(require.main === module){
    run()
}
exports.run = run 