
// For Windows, we distribute a prebuilt bsc.exe
// To build on windows, we still need figure out constructing config.ml
// from existing  compiler

// For other OSes, we detect
// if there is other compiler installed and the version matches,
// we get the config.ml from existing OCaml compiler and build whole_compiler

// Otherwise, we build the compiler shipped with Buckle and use the
// old compiler.ml
// This will be run in npm postinstall, don't use too fancy features here

var child_process = require('child_process')
var process = require('process')
var fs = require('fs')
var path = require('path')
var os = require('os')

var os_type = os.type()
var is_windows = !(os_type.indexOf('Windows') < 0)
var root_dir = path.join(__dirname,'..')
var jscomp = path.join(root_dir, 'jscomp')
var jscomp_bin = path.join(jscomp, 'bin')

var working_dir = process.cwd()
console.log("Working dir", working_dir)
var working_config = { cwd: jscomp, stdio: [0, 1, 2] }
var clean = require('./clean.js')
var build_util = require('./build_util')
var vendor_ninja_version = '1.7.2'

var ninja_bin_output = path.join(root_dir,'bin','ninja.exe')
var ninja_vendor_dir = path.join(jscomp_bin,'vendor')
console.log('Prepare ninja binary ')
if(is_windows){
    fs.rename(path.join(ninja_vendor_dir,'ninja.win'),ninja_bin_output)
}
else if(os_type==='Darwin'){

    fs.renameSync(path.join(ninja_vendor_dir,'ninja.darwin'),ninja_bin_output)
}  
else if(process.env.BS_TRAVIS_CI){
    fs.renameSync(path.join(ninja_vendor_dir,'ninja.linux64'),ninja_bin_output)
}
else {
    console.log('No prebuilt Ninja, building Ninja now')
    var ninja_vendor_dir = "ninja-" + vendor_ninja_version
    var ninja_vendor_tar = ninja_vendor_dir + ".tar.gz"
    var build_ninja_command = "tar -xf " + ninja_vendor_tar + " && cd " + ninja_vendor_dir + " && ./configure.py --bootstrap "
    child_process.execSync(build_ninja_command,{cwd:root_dir})
    fs.renameSync(path.join(root_dir, ninja_vendor_dir,'ninja'), ninja_bin_output)
}
console.log('ninja binary is ready: ', ninja_bin_output)

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
        // Make it more fault tolerant
        // =1 can still be okay (only ninja.win in this case)
        child_process.execFileSync(path.join(__dirname, 'win_build.bat'), working_config)
        clean.clean()
        console.log("Installing")
        build_util.install()
    } else {
        // Cygwin
        console.log("It is on windows, but seems to be that you are building against master branch, so we are going to depend on cygwin on master")
        non_windows_npm_release()
    }
}
else {
    non_windows_npm_release()
}

