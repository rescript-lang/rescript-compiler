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
var root_dir = path.join(__dirname, '..')
var lib_dir = path.join(root_dir, 'lib')
var jscomp_dir = path.join(root_dir, 'jscomp')
var runtime_dir = path.join(jscomp_dir,'runtime')
var others_dir = path.join(jscomp_dir,'others')



var ocaml_dir = path.join(lib_dir,'ocaml')
var config = require('./config.js')

var is_windows = config.is_windows
var sys_extension = config.sys_extension

process.env.BS_RELEASE_BUILD = 'true'
var ocamlVersion = require('./buildocaml.js').getVersionPrefix()
var stdlib_dir = path.join(jscomp_dir, ocamlVersion.includes('4.02') ? 'stdlib-402' : 'stdlib-406')
// Add vendor bin path
// So that second try will work
process.env.PATH =
    path.join(__dirname, '..', 'native',ocamlVersion,'bin') +
    path.delimiter +
    process.env.PATH

var ninja_bin_output = path.join(root_dir, 'lib', 'ninja.exe')


/**
 * Make sure `ninja_bin_output` exists    
 * The installation of `ninja.exe` is re-entrant, since we always pre-check if it is already installed
 * This is less problematic since `ninja.exe` is very stable
 */
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
    /**
     * 
     * @param {string} binary_path 
     */
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
        console.log("ninja binary is already cached and installed: ", ninja_bin_output)
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
/**
 * 
 * @param {NodeJS.ErrnoException} err 
 */
function throwWhenError(err){
    if(err!==null){
        throw err
    }
}
/**
 * 
 * @param {string} file 
 * @param {string} target 
 */
function poorCopyFile(file, target) {
	var stat = fs.statSync(file)
	fs.createReadStream(file).pipe(
		fs.createWriteStream(target,
			{ mode: stat.mode }))            
}
/**
 * @type {(x:string,y:string)=>void} 
 * 
 */
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

    if (output.indexOf(ocamlVersion) >= 0) {
        console.log(output)
        console.log("Use the compiler above")
    } else {
        console.log('version', output, 'needed version', ocamlVersion, "No matched compiler found, may re-try")
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
            require('./buildocaml.js').build(true)
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

/**
 * 
 * @param {string} sys_extension 
 * 
 */
function createCopyNinja(sys_extension){
    var output = ''
    switch(sys_extension){
        case '.win32':
            output += `
rule cp
    command = cmd /q /c copy $in $out 1>nul
`
            break
        default:
            output += `
rule cp 
    command = cp $in $out
`
            break
    }
    output += [
        'bsc','bsb','bsb_helper','bsppx',
        'refmt',
    ].map(function(x){
        return `build ${x}.exe: cp ${x}${sys_extension}`
    }).join('\n')
    output += '\n'
    return output
}

function copyPrebuiltCompilers() {
    var filePath = path.join(lib_dir,'copy.ninja')
    fs.writeFileSync(filePath,createCopyNinja(sys_extension),'ascii')
    cp.execFileSync(ninja_bin_output,
        ["-f", 'copy.ninja'],
        { cwd: lib_dir, stdio: [0, 1, 2] })
    fs.unlinkSync(filePath)    
}

/**
 * @returns {boolean}
 */
function checkPrebuiltBscCompiler() {
    try {
        var version = cp.execFileSync(path.join(lib_dir, 'bsc' + sys_extension), ['-v'])
        console.log("checkoutput:", String(version))
        console.log("Prebuilt compiler works good")
        
        return true
    } catch (e) {
        console.log("No working prebuilt buckleScript compiler")
        return false
    }
}

function buildLibs(){
    var releaseNinja = `
stdlib = ${ocamlVersion.includes('4.06')? 'stdlib-406' : 'stdlib-402'}
subninja runtime/release.ninja
subninja others/release.ninja
subninja $stdlib/release.ninja
${process.env.BS_TRAVIS_CI ? 'subninja test/build.ninja\n' : '\n'}
build all: phony runtime others $stdlib
`
    var filePath = path.join(jscomp_dir,'release.ninja')
    fs.writeFileSync(filePath,releaseNinja,'ascii')
    cp.execFileSync(ninja_bin_output, [ "-f", "release.ninja", "-t", "clean"], { cwd: jscomp_dir, stdio: [0, 1, 2] , shell: false})
    cp.execFileSync(ninja_bin_output, [ "-f", "release.ninja"], { cwd: jscomp_dir, stdio: [0, 1, 2] , shell: false})
    fs.unlinkSync(filePath)
    console.log('Build finished')
}

function provideCompiler() {
    // FIXME: weird logic
    // if (fs.existsSync(path.join(lib_dir,'ocaml','pervasives.cmi'))) {
    //     console.log('Found pervasives.cmi, assume it was already built')
    //     return true // already built before
    // }
    if (checkPrebuiltBscCompiler()) {
        copyPrebuiltCompilers()
    }
    else {
        // when not having bsc.exe
        tryToProvideOCamlCompiler()
        // Note this ninja file only works under *nix due to the suffix
        // under windows require '.exe'
        var releaseNinja = require('./ninjaFactory.js').libNinja({
            ocamlopt : 'ocamlopt.opt',
            ext : '.exe',
            INCL: require('./buildocaml.js').getVersionPrefix()
        })

        var filePath =  path.join(lib_dir,'release.ninja')
        fs.writeFileSync(filePath,releaseNinja,'ascii')
        cp.execFileSync(ninja_bin_output, ['-f', 'release.ninja'], { cwd: lib_dir, stdio: [0, 1, 2] })
        fs.unlinkSync(filePath)

    }    
}

provideNinja()

if(is_windows){
    copyPrebuiltCompilers()
} else{
    provideCompiler()
}

buildLibs()

install()

