
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

var is_windows = !(os.type().indexOf('Windows') < 0)
var jscomp = path.join(__dirname, '..', 'jscomp')
var jscomp_bin = path.join(jscomp, 'bin')

var working_dir = process.cwd()
console.log("Working dir", working_dir)
var working_config = { cwd: jscomp, stdio: [0, 1, 2] }
var clean = require('./clean.js')
var build_util = require('./build_util')


function non_windows_npm_release() {
    try {
        child_process.execSync('node config.js', working_config)
        console.log("Build the compiler and runtime .. ")
        child_process.execSync("make world", working_config)

    } catch (e) {
        child_process.execSync(path.join(__dirname, 'buildocaml.sh')) // TODO: sh -c ? this will be wrong if we have white space in the path
        process.env.PATH = path.join(__dirname, '..', 'bin') + path.delimiter + process.env.PATH
        console.log('configure again with local ocaml installed')
        if (process.env.BS_TRAVIS_CI) {
            child_process.execSync("make travis-world-test", working_config)
        } else {
            child_process.execSync("make world", working_config)
        }

        clean.clean()
    }

    console.log("Installing")
    child_process.execSync('make VERBOSE=true install', working_config)

}

if (is_windows) {
    process.env.WIN32 = '1'
    console.log("Installing on Windows")
    var indeed_windows_release = 0
    fs.readdirSync(jscomp_bin).forEach(function (f) {
        var last_index = f.lastIndexOf('.win')
        if (last_index !== -1) {
            var new_file = f.slice(0, -4) + ".exe"
            build_util.poor_copy_sync(path.join(jscomp_bin, f), path.join(jscomp_bin, new_file));
            // we do have .win file which means windows npm release
            ++indeed_windows_release
        }

    })
    if (indeed_windows_release > 1) {
        // only ninja.win in this case
        child_process.execFileSync(path.join(__dirname, 'win_build.bat'), working_config)
        clean.clean()
        console.log("Installing")
        build_util.install()
    } else {
        console.log("It is on windows, but seems to be that you are building against master branch, so we are going to depend on cygwin on master")
        non_windows_npm_release()
    }
}
else {
    non_windows_npm_release()
}

