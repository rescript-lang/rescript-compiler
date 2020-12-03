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

    // Note removing js file would affect `release.ninja`
    // for (let file of fs.readdirSync(libJsDir)) {
    //     if (file.endsWith('.js')) {
    //         fs.unlinkSync(path.join(libJsDir, file))
    //     }
    // }


    cp.execSync(`git clean -dfx stubs ext common syntax depends core bsb main .`,
        { cwd: jscompDir, encoding: 'utf8', stdio: [0, 1, 2] })
    // cp.execSync(`git clean -dfx templates && ocp-ocamlres templates -o bsb_templates.ml`,
    //     { cwd: path.join(jscompDir,'bsb'), encoding: 'utf8', stdio: [0, 1, 2] })
    cp.execSync(`ninja -t clean -g && ninja`,
        { cwd: jscompDir, encoding: 'utf8', stdio: [0, 1, 2] })
    cp.execSync('ninja', {cwd : path.join(rootDir,'lib'), stdio:[0,1,2]})
    cp.execSync('ninja -f release.ninja -t clean && ninja -f release.ninja', { cwd: path.join(rootDir, 'jscomp'), stdio: [0, 1, 2]})
    require('./doc_gen').main()
}
if(require.main === module){
    run()
}
exports.run = run 