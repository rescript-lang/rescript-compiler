
// For Windows, we distribute a prebuilt bsc.exe
// To build on windows, we still need figure out constructing config.ml
// from existing  compiler

// For other OSes, we detect
// if there is other compiler installed and the version matches,
// we get the config.ml from existing OCaml compiler and build `whole_compiler`

// Otherwise, we build the compiler shipped with Buckle and use the
// old compiler.ml

var child_process = require('child_process')
var process = require('process')
var fs = require('fs')
var path = require('path')
var os = require('os')

var is_windows = ! (os.type().indexOf('Windows') < 0)
var jscomp = path.join(__dirname,'..','jscomp')
var working_dir = process.cwd()
console.log("Working dir", working_dir)
var working_config = {cwd: jscomp, stdio:[0,1,2]}
var clean = require('./clean.js')

if (is_windows) {
    process.env.WIN32 = '1'
}

try{
    child_process.execSync('node config.js', working_config)
    console.log("Build the compiler and runtime .. ")
    child_process.execSync("make world", working_config)

}catch(e){
    child_process.execSync(path.join(__dirname,'buildocaml.sh'))
    process.env.PATH = path.join(__dirname,'..','bin') + path.delimiter + process.env.PATH
    console.log('configure again with local ocaml installed')
    if(process.env.BS_TRAVIS_CI){
        child_process.execSync("make travis-world-test", working_config)
    } else {
        child_process.execSync("make world", working_config)
    }

    clean.clean()
}

console.log("Installing")
child_process.execSync('make VERBOSE=true install', working_config)
