//@ts-check
var child_process = require('child_process')

var fs = require('fs')
var path = require('path')
var os = require('os')
var root = path.join(__dirname, '..')
var dest_bin = path.join(root, 'lib')
var dest_lib = path.join(root, 'lib', 'ocaml')


var jscomp = path.join(root, 'jscomp')



/*
 * It is weird reaname cause Busy resource or lock error
 */
function copyFile(file, target) {
	var stat = fs.statSync(file)
	fs.createReadStream(file).pipe(
		fs.createWriteStream(target,
			{ mode: stat.mode }))

}


/**
 * @param {string} from
 * @param {string} to
 */
function renameAsync(from, to) {
	console.log(from, '----->', to)
	// @ts-ignore
	fs.rename(from, to, (err) => {
            if (err) throw err;
        })
	// fs.renameSync(from,to)

}

/**
 * @param {string} from
 * @param {string} to
 */
function poor_copy_sync(from, to) {
	console.log(from, '----->', to)
	fs.renameSync(from, to)
	// fs.renameSync(from,to)

}

/**
 * @param {string} dir
 * @param {string} dest_lib
 */
function install_directory(dir, dest_lib) {
	var files = fs.readdirSync(dir)
	files.forEach(function (file) {
		var installed_fmt = ['.cmt', '.cmti', '.cmj', '.ml', '.mli', '.cmi']
		var format_file = path.parse(file)
		if (format_file &&
			(installed_fmt.indexOf(format_file.ext) !== -1)
		) {
			var from = path.join(dir, file)
			var to = path.join(dest_lib, file)
			renameAsync(from, to)
		}
	})

}
function install() {
	if (!fs.existsSync(dest_bin)) {
		fs.mkdirSync(dest_bin)
	}
	if (!fs.existsSync(dest_lib)) {
		fs.mkdirSync(dest_lib)
	}



	var jscomp_runtime = path.join(jscomp, 'runtime')

	var files = fs.readdirSync(jscomp_runtime)
	files.forEach(function (file) {
		var format_file = path.parse(file)
		var special_files = [
			'js',
			'js_unsafe',
			'js_internal',
			'caml_exceptions',
			'js_null',
			'js_undefined',
			'js_exn',
			'js_int',
			'js_float',
			'js_typed_array'
		]
		var installed_fmt = ['.cmt', '.cmti', '.cmj']
		if (
			format_file &&
			((special_files.indexOf(format_file.name) !== -1)
				||
				(installed_fmt.indexOf(format_file.ext) !== -1))
			// Always copy cmt* cmj*
		) {
			var from = path.join(jscomp_runtime, file)
			var to = path.join(dest_lib, file)
			renameAsync(from, to)
		}
	})

	install_directory(path.join(jscomp, 'stdlib'), dest_lib)
	install_directory(path.join(jscomp, 'others'), dest_lib)
}

exports.install = install;
exports.poor_copy_sync = poor_copy_sync;
