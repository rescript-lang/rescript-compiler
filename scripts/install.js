
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
var jscomp_bin = path.join(jscomp,'bin')
var dest_bin = path.join(__dirname,'..','bin')
var working_dir = process.cwd()
console.log("Working dir", working_dir)
var working_config = {cwd: jscomp, stdio:[0,1,2]}


/*
 * It is weird reaname cause Busy resource or lock error
 */
function copyFile(file,target){
    var stat = fs.statSync(file)
    fs.createReadStream(file).pipe(
	fs.createWriteStream(target,
			     {mode : stat.mode}))
    
}

function poor_copy(from,to){
    console.log(from , '----->', to)
    fs.rename(from,to)
    // fs.renameSync(from,to)

}

function poor_copy_sync(from,to){
    console.log(from , '----->', to)
    fs.renameSync(from,to)
    // fs.renameSync(from,to)

}
function install_directory(dir,dest_lib){
    var files = fs.readdirSync(dir)
    files.forEach(function(file){
	var installed_fmt = ['.cmt','.cmti', '.cmj','.ml','.mli','.cmi']
	var format_file = path.parse(file)
	if(format_file && 
	   (installed_fmt.indexOf(format_file.ext) !== -1)
	  ){
	    var from = path.join(dir,file)
	    var to = path.join(dest_lib,file)
	    poor_copy(from,to)
	}
    })

}
function install(){
    var dest_lib = path.join(__dirname,'..','lib','ocaml')
    if(!fs.existsSync(dest_bin)){
	fs.mkdirSync(dest_bin)
    }
    if(!fs.existsSync(dest_lib)){
	fs.mkdirSync(dest_lib)
    }



    var jscomp_runtime = path.join(jscomp,'runtime')

    files = fs.readdirSync(jscomp_runtime)
    files.forEach(function(file){
	var format_file = path.parse(file)
	var special_files = ['js', 'js_unsafe', 'js_null', 'js_undefined']
	var installed_fmt = ['.cmt','.cmti', '.cmj']
	if( 
	    format_file &&
		((special_files.indexOf(format_file.name) !== -1)
		 ||
		 (installed_fmt.indexOf(format_file.ext) !== -1))
	  ){
	    var from = path.join(jscomp_runtime,file)
	    var to = path.join(dest_lib,file)
	    poor_copy(from,to)
	}
    })

    install_directory(path.join(jscomp,'stdlib'), dest_lib)
    install_directory(path.join(jscomp,'others'), dest_lib)

    var files = fs.readdirSync(jscomp_bin)
    files.forEach(function(file){
	var format_file = path.parse(file)
	if(format_file &&
	   format_file.ext === '.exe' || 
	   format_file.ext === '.js'
	  ){
	    var from = path.join(jscomp_bin,file)
	    var to = path.join(dest_bin,file)
	    copyFile(from,to)
	}

    })
}

var clean = require('./clean.js')
if (is_windows) {

    process.env.WIN32 = '1';
    console.log("Installing on Windows")
    fs.readdirSync(path.join(jscomp,'bin')).forEach(function(f){
	var last_index = f.lastIndexOf('.win')
	if(last_index !== -1){
	    var new_file = f.slice(0,-4) + ".exe"
	    poor_copy(path.join(jscomp, 'bin',f),path.join(jscomp,'bin',new_file))
	}

    })
    child_process.execSync(path.join(__dirname,'win_build.bat'), working_config)
    clean.clean()
    console.log("Installing")
    install()
}
else {

    try {
        child_process.execSync('node config.js', working_config)
        console.log("Build the compiler and runtime .. ")
        child_process.execSync("make world", working_config)

    } catch (e) {
        child_process.execSync(path.join(__dirname, 'buildocaml.sh'))
        process.env.PATH = path.join(__dirname, '..', 'bin') + path.delimiter + process.env.PATH
        child_process.execSync("make world", working_config)
    }

    clean.clean()
    console.log("Installing")
    child_process.execSync('make VERBOSE=true install', working_config)

}

