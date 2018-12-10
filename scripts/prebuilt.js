//@ts-check
var child_process = require('child_process')
var fs = require('fs')
var path = require('path')
var {sys_extension} = require('./config.js')

var root = path.join(__dirname, '..')
var root_config = { cwd: root, stdio: [0, 1, 2] }
process.env.BS_RELEASE_BUILD = 'true'



function buildCompiler() {
	child_process.execSync('make -j1 -B -C lib all', root_config)
	fs.readdirSync(path.join(root, 'lib')).forEach(function (f) {
		var last_index = f.lastIndexOf('.exe')
		if (last_index !== -1) {
			var new_file = f.slice(0, -4) + sys_extension
			console.log(f + " --> " + new_file)
			fs.renameSync(path.join(root, 'lib', f), path.join(root, 'lib', new_file))
		}
	})
}


buildCompiler()

