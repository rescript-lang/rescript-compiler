//@ts-check
var child_process = require('child_process')
var path = require('path')
var {sys_extension, is_windows} = require('./config.js')

var root = path.join(__dirname, '..')
var root_config = { cwd: root, stdio: [0, 1, 2] }
process.env.BS_RELEASE_BUILD = 'true'



function buildCompiler() {
	child_process.execSync(`ninja -C lib -f prebuilt${sys_extension}.ninja -t clean && ninja -C lib -f prebuilt${sys_extension}.ninja`,root_config)
}
if(!is_windows){
  require('./ninja.js').updateRelease()
}

buildCompiler()

