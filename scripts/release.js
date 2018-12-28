#!/usr/bin/env node 
//@ts-check

// used as a unit of releasing
var path = require('path')
var fs = require('fs')
var cp = require('child_process')
var rootDir = path.join(__dirname,'..')
var libJsDir = path.join(rootDir,'lib','js')
var jscompDir = path.join(rootDir,'jscomp')

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
    cp.execSync(`ninja -t clean && ninja`,
        { cwd: jscompDir, encoding: 'utf8', stdio: [0, 1, 2] })
    cp.execSync('ninja', {cwd : path.join(rootDir,'lib'), stdio:[0,1,2]})
    cp.execSync('ninja -f release.ninja -t clean && ninja -f release.ninja', { cwd: path.join(rootDir, 'jscomp'), stdio: [0, 1, 2]})
    
}
if(require.main === module){
    run()
}
exports.run = run 