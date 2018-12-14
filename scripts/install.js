//@ts-check
// For Windows, we distribute a prebuilt bsc.exe
// To build on windows, we still need figure out constructing config.ml
// from existing  compiler

// For other OSes, we detect
// if there is other compiler installed and the version matches,
// we get the config.ml from existing OCaml compiler and build whole_compiler

// Otherwise, we build the compiler shipped with Buckle and use the
// old compiler.ml
// This will be run in npm postinstall, don't use too fancy features here

var cp = require('child_process')
var fs = require('fs')
var path = require('path')
// var os = require('os')

// var os_type = os.type()
var root_dir = path.join(__dirname, '..')
var lib_dir = path.join(root_dir, 'lib')
var jscomp_dir = path.join(root_dir, 'jscomp')
var runtime_dir = path.join(jscomp_dir,'runtime')
var others_dir = path.join(jscomp_dir,'others')
var stdlib_dir = path.join(jscomp_dir, 'stdlib-402')
var root_dir_config = { cwd: root_dir, stdio: [0, 1, 2] }

// var dest_bin = path.join(root_dir, 'lib')
// var dest_lib = path.join(root_dir, 'lib', 'ocaml')

var ocaml_dir = path.join(lib_dir,'ocaml')
var config = require('./config.js')
var make = config.make
var is_windows = config.is_windows
var sys_extension = config.sys_extension

process.env.BS_RELEASE_BUILD = 'true'
// Add vendor bin path
// So that second try will work
process.env.PATH =
    path.join(__dirname, '..', 'vendor', 'ocaml', 'bin') +
    path.delimiter +
    process.env.PATH

var ninja_bin_output = path.join(root_dir, 'lib', 'ninja.exe')
// Make sure `ninja_bin_output` exists    
function provideNinja() {
    var vendor_ninja_version = '1.8.2'    
    var ninja_source_dir = path.join(root_dir, 'vendor', 'ninja')
    function build_ninja() {
        console.log('No prebuilt Ninja, building Ninja now')
        var build_ninja_command = "./configure.py --bootstrap"
        cp.execSync(build_ninja_command, { cwd: ninja_source_dir, stdio: [0, 1, 2] })
        fs.renameSync(path.join(ninja_source_dir, 'ninja'), ninja_bin_output)
        console.log('ninja binary is ready: ', ninja_bin_output)
    }

    // sanity check to make sure the binary actually runs. Used for Linux. Too many variants
    function test_ninja_compatible(binary_path) {
        var version;
        try {
            version = cp.execSync(JSON.stringify(binary_path) + ' --version', {
                encoding: 'utf8',
                stdio: ['pipe', 'pipe', 'ignore'] // execSync outputs to stdout even if we catch the error. Silent it here
            }).trim();
        } catch (e) {
            console.log('ninja not compatible?', String(e))
            return false;
        }
        return version === vendor_ninja_version;
    };


    var ninja_os_path = path.join(ninja_source_dir,'snapshot', 'ninja' + sys_extension)
    if (fs.existsSync(ninja_bin_output) && test_ninja_compatible(ninja_bin_output)) {
        console.log("ninja binary is already cached: ", ninja_bin_output)
    }
    else if (fs.existsSync(ninja_os_path)) {
        if(fs.copyFileSync){
            // ninja binary size is small    
            fs.copyFileSync(ninja_os_path,ninja_bin_output)
        }
        else {
            fs.renameSync(ninja_os_path, ninja_bin_output)
        }
        if (test_ninja_compatible(ninja_bin_output)) {
            console.log("ninja binary is copied from pre-distribution")
        } else {
            build_ninja()
        }
    } else {
        build_ninja()
    }
}

function throwWhenError(err){
    if(err!==null){
        throw err
    }
}

function poorCopyFile(file, target) {
	var stat = fs.statSync(file)
	fs.createReadStream(file).pipe(
		fs.createWriteStream(target,
			{ mode: stat.mode }))            
}
var installTrytoCopy;
if(fs.copyFile !== undefined){
    installTrytoCopy = function(x,y){
        fs.copyFile(x,y,throwWhenError)
    }
} else if(is_windows){
    installTrytoCopy = function(x,y){
        fs.rename(x,y,throwWhenError)
    }
} else {
    installTrytoCopy = function(x,y){
        poorCopyFile(x,y)
    }
}

/**
 * 
 * @param {string} src 
 * @param {(file:string)=>boolean} filter 
 * @param {string} dest 
 */
function installDirBy(src,dest,filter){
    fs.readdir(src,function(err,files){
        if( err === null) {
            files.forEach(function(file){
                if(filter(file)){
                    var x = path.join(src,file)
                    var y = path.join(dest,file)
                    // console.log(x, '----->', y )
                    installTrytoCopy(x,y)                    
                }
            })
        } else {
            throw err
        }
    })
}

function install(){
    if (!fs.existsSync(lib_dir)) {
        fs.mkdirSync(lib_dir)
    }
    if (!fs.existsSync(ocaml_dir)) {
        fs.mkdirSync(ocaml_dir)
    }
    installDirBy(runtime_dir,ocaml_dir,function(file){        
        var y = path.parse(file)
        return y.name === 'js' || y.ext.includes('cm')        
    })
    installDirBy(others_dir,ocaml_dir,function(file){
        var y = path.parse(file)
        return y.ext === '.ml' || y.ext === '.mli' || y.ext.includes('cm')
    })
    installDirBy(stdlib_dir,ocaml_dir,function(file){
        var y = path.parse(file)
        return y.ext === '.ml' || y.ext === '.mli' || y.ext.includes('cm')
    })
}
/**
 * raise an exception if not matched
 */
function matchedCompilerExn() {
    var output = cp.execSync('ocamlc.opt -v', { encoding: 'ascii' })
    if (output.indexOf("4.02.3") >= 0) {
        console.log(output)
        console.log("Use the compiler above")
    } else {
        console.log("No matched compiler found, may re-try")
        throw ""
    }
}
function tryToProvideOCamlCompiler() {
    try {
        if (process.env.BS_ALWAYS_BUILD_YOUR_COMPILER) {
            throw 'FORCED TO REBUILD'
        }
        matchedCompilerExn()
    } catch (e) {
        console.log('Build a local version of OCaml compiler, it may take a couple of minutes')
        try {
            cp.execFileSync(path.join(__dirname, 'buildocaml.sh'))
        } catch (e) {
            console.log(e.stdout.toString());
            console.log(e.stderr.toString());
            console.log('Building a local version of the OCaml compiler failed, check the output above for more information. A possible problem is that you don\'t have a compiler installed.');
            throw e;
        }
        console.log('configure again with local ocaml installed')
        matchedCompilerExn()
        console.log("config finished")
    }
}

function renamePrebuiltCompilers() {
    fs.readdirSync(lib_dir).forEach(function (f) {
        var last_index = f.lastIndexOf(sys_extension)
        if (last_index !== -1) {
            var new_file = f.slice(0, - sys_extension.length) + ".exe"
            var x = path.join(lib_dir, f)
            var y = path.join(lib_dir,new_file)
            console.log(x,'-->',y)
            fs.renameSync(x, y);
            // we do have .win file which means windows npm release
        }
    })
}

/**
 * @returns {boolean}
 */
function checkPrebuilt() {
    try {
        var version = cp.execFileSync(path.join(lib_dir, 'bsc' + sys_extension), ['-v'])
        console.log("checkoutput:", String(version))
        console.log("Prebuilt compiler works good")
        renamePrebuiltCompilers()
        return true
    } catch (e) {
        console.log("No working prebuilt buckleScript compiler")
        return false
    }
}

function buildLibs(){
    cp.execFileSync(ninja_bin_output, ["-t", "clean"], { cwd: runtime_dir, stdio: [0, 1, 2] , shell: false})
    cp.execFileSync(ninja_bin_output, { cwd: runtime_dir, stdio: [0, 1, 2] , shell: false})
    cp.execFileSync(ninja_bin_output, ["-t", "clean"], { cwd: others_dir, stdio: [0, 1, 2], shell: false})
    cp.execFileSync(ninja_bin_output, { cwd: others_dir, stdio: [0, 1, 2], shell: false })
    cp.execFileSync(ninja_bin_output, ["-t", "clean"], { cwd: stdlib_dir, stdio: [0, 1, 2], shell: false })
    cp.execFileSync(ninja_bin_output, { cwd: stdlib_dir, stdio: [0, 1, 2], shell : false })
    console.log('Build finsihed')
}

function provideCompiler() {
    if (fs.existsSync(path.join(lib_dir,'ocaml','pervasives.cmi'))) {
        console.log('Found pervasives.cmi, assume it was already built')
        return true // already built before
    }
    if (!checkPrebuilt()) {
        // when not having bsc.exe
        tryToProvideOCamlCompiler()
        if (process.env.BS_TRAVIS_CI === "1") {
            console.log('Enforcing snapshot in CI mode')
            if (fs.existsSync(path.join(root_dir, 'jscomp', 'Makefile'))) {
                cp.execSync("make -C jscomp force-snapshotml", root_dir_config)
            } else {
                console.log("jscomp/Makefile is missing")
            }
        }
        // Note this ninja file only works under *nix due to the suffix
        // under windows require '.exe'
        cp.execFileSync(ninja_bin_output, { cwd: lib_dir, stdio: [0, 1, 2] })

    }    
}

provideNinja()

if(is_windows){
    renamePrebuiltCompilers()
} else{
    provideCompiler()
}

buildLibs()

install()

