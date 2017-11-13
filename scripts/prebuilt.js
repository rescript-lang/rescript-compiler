//@ts-check
var child_process = require('child_process')

var fs = require('fs')
var path = require('path')
var os = require('os')
var config = require('./config.js')
var sys_extension = config.sys_extension
var is_windows = !(os.type().indexOf('Windows') < 0)
var root = path.join(__dirname, '..')
var jscomp = path.join(root, 'jscomp')

var working_dir = process.cwd()
console.log("Working dir", working_dir)
var jscomp_dir_config = { cwd: jscomp, stdio: [0, 1, 2] }
var root_config = { cwd: root, stdio: [0, 1, 2] }
process.env.BS_RELEASE_BUILD = 'true'

function getLibCommands() {
	var result =
		child_process.execSync(
			'make -B -j1 --dry-run libs',
			{
				cwd: jscomp,
				encoding: 'utf8'
			})
	var jobs =
		result.split('\n').reduce(function (acc, line) {

			if (line.indexOf('Entering') !== -1) {
				var line_segs = line.split(' ')
				var new_path =
					path.parse(line_segs[line_segs.length - 1].slice(1, -1))
				acc.push({ base: new_path.base, jobs: [] })
				return acc
			} else if (line.indexOf('bsc.exe') !== -1) {
				var current_job = acc[acc.length - 1]
				var new_line = line
				if (current_job.base === 'stdlib') {
					var shell_commands = line.split('`')
					if (shell_commands.length === 3) {
						var command = shell_commands[1]
						console.log('-')
						var command_output =
							child_process.execSync('sh ' + command, { cwd: path.join(jscomp, current_job.base), encoding: 'utf8' })
						new_line = shell_commands[0] + ' ' + command_output.trim() + ' ' + shell_commands[2]
						// console.log('command output', command_output)
					} else {
						console.log('weird > ', shell_commands)
					}

				}
				current_job.jobs.push(new_line);
				return acc
			} else {
				return acc
			}
		}, [])


	var bat_commands = jobs.map(function (current) {
		return `cd ${current.base}\n` + current.jobs.map(function (job) { return job.replace(/-bs-diagnose/g, ' ').replace(/\//g, '\\') }).join('\n') + `\ncd ..`
	}
	).join('\n')
	return bat_commands
}


function buildCompiler() {
	child_process.execSync('make -j1 -B -C lib all', root_config)

	if(is_windows){
		fs.writeFileSync(
			path.join(__dirname,
			'win_build.bat'),
			getLibCommands(),
			'utf8'
		)
	}
	
	// rename exe to .win
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

