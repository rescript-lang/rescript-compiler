
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

var fs = require('fs')
var path = require('path')
var os = require('os')

var os_type = os.type()
var os_arch = os.arch()
var is_windows = !(os_type.indexOf('Windows') < 0)
var is_bsd = !(os_type.indexOf('BSD') < 0)
var root_dir = path.join(__dirname, '..')
var jscomp = path.join(root_dir, 'jscomp')
var jscomp_bin = path.join(jscomp, 'bin')

var working_dir = process.cwd()
console.log("Working dir", working_dir)
var working_config = { cwd: jscomp, stdio: [0, 1, 2] }

var build_util = require('./build_util')
var vendor_ninja_version = '1.8.2'

var ninja_bin_output = path.join(root_dir, 'bin', 'ninja.exe')
var ninja_source_dir = path.join(root_dir,'vendor','ninja')
var ninja_build_dir = path.join(root_dir, 'vendor', 'ninja-build')

function build_ninja() {
    console.log('No prebuilt Ninja, building Ninja now')
    var build_ninja_command = "./configure.py --bootstrap"
    child_process.execSync(build_ninja_command, { cwd: ninja_source_dir , stdio:[0,1,2]})
    fs.renameSync(path.join(ninja_source_dir, 'ninja'), ninja_bin_output)
    console.log('ninja binary is ready: ', ninja_bin_output)
}

// sanity check to make sure the binary actually runs. Used for Linux. Too many variants
function test_ninja_compatible(binary_path) {
    var version;
    try {
        version = child_process.execSync(JSON.stringify(binary_path) + ' --version', {
            encoding: 'utf-8',
            stdio: ['pipe', 'pipe', 'ignore'] // execSync outputs to stdout even if we catch the error. Silent it here
        }).trim();
    } catch (e) {
        console.log('ninja not compatible?', String(e))
        return false;
    }
    return  version === vendor_ninja_version;
};


var ninja_os_path
if (is_windows) {
    ninja_os_path = path.join(ninja_build_dir, 'ninja.win')
} else if (os_type === 'Darwin') {
    ninja_os_path = path.join(ninja_build_dir, 'ninja.darwin')
} else if (os_type === 'Linux') {
    ninja_os_path = path.join(ninja_build_dir, 'ninja.linux64')
}
if (fs.existsSync(ninja_bin_output) && test_ninja_compatible(ninja_bin_output)) {
    console.log("ninja binary is already cached: ", ninja_bin_output)
} else if (fs.existsSync(ninja_os_path)) {
    fs.renameSync(ninja_os_path, ninja_bin_output)
    if (test_ninja_compatible(ninja_bin_output)) {
        console.log("ninja binary is copied from pre-distribution")
    } else {
        build_ninja()
    }
} else {
    build_ninja()
}

process.env.PATH = path.join(__dirname, '..', 'vendor', 'ocaml', 'bin') + path.delimiter + process.env.PATH
var make = is_bsd ? 'gmake' : 'make';

function non_windows_npm_release() {

    try {
        child_process.execSync('node ../scripts/config_compiler.js', working_config)
    } catch (e) {
        console.log('Build a local version of OCaml compiler, it may take a couple of minutes')
        try {
            child_process.execSync(path.join(__dirname, 'buildocaml.sh')) // TODO: sh -c ? this will be wrong if we have white space in the path
        } catch (e) {
            console.log(e.stdout.toString());
            console.log(e.stderr.toString());
            console.log('Building a local version of the OCaml compiler failed, check the outut above for more information. A possible problem is that you don\'t have a compiler installed');
            throw e;
        }
        console.log('configure again with local ocaml installed')
        child_process.execSync('node ../scripts/config_compiler.js', working_config)
        console.log("config finished")
    }
    console.log("Build the compiler and runtime .. ")
    child_process.execSync(make + " world", working_config)
    console.log("Installing")
    child_process.execSync(make + ' VERBOSE=true install', working_config)

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
        // clean.clean()
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
