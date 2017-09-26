//@ts-check
var child_process = require('child_process')

var fs = require('fs')
var path = require('path')
var os = require('os')

var dest_bin = path.join(__dirname, '..', 'bin')
var dest_lib = path.join(__dirname,'..','lib','ocaml')


var jscomp = path.join(__dirname, '..', 'jscomp')
var jscomp_bin = path.join(jscomp, 'bin')


/*
 * It is weird reaname cause Busy resource or lock error
 */
function copyFile(file,target){
    var stat = fs.statSync(file)
    fs.createReadStream(file).pipe(
	fs.createWriteStream(target,
			     {mode : stat.mode}))
    
}


/**
 * @param {string} from 
 * @param {string} to 
 */
function renameAsync(from,to){
    console.log(from , '----->', to)
    fs.rename(from,to)
    // fs.renameSync(from,to)

}

/**
 * @param {string} from
 * @param {string} to
 */
function poor_copy_sync(from,to){
    console.log(from , '----->', to)
    fs.renameSync(from,to)
    // fs.renameSync(from,to)

}

/**
 * @param {string} dir 
 * @param {string} dest_lib
 */
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
	    renameAsync(from,to)
	}
    })

}
function install(){
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
	var special_files = ['js', 'js_unsafe', 'js_internal',  'js_null', 'js_undefined', 'js_typed_array', 'caml_exceptions', 'js_float']
	var installed_fmt = ['.cmt','.cmti', '.cmj']
	if( 
	    format_file &&
		((special_files.indexOf(format_file.name) !== -1)
		 ||
		 (installed_fmt.indexOf(format_file.ext) !== -1))
	  ){
	    var from = path.join(jscomp_runtime,file)
	    var to = path.join(dest_lib,file)
	    renameAsync(from,to)
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

exports.install =  install;
exports.poor_copy_sync = poor_copy_sync;
