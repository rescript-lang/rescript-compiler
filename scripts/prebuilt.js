//@ts-check
var child_process = require('child_process')
var path = require('path')
var {sys_extension, is_windows} = require('./config.js')

var root = path.join(__dirname, '..')
var root_config = { cwd: root, stdio: [0, 1, 2] }
process.env.BS_RELEASE_BUILD = 'true'


var version = require('./vendored_ocaml_version.js').getVersionPrefix()
var fs = require('fs')
function buildCompiler() {
  var prebuilt = 'prebuilt.ninja'
  var content = `
ocamlopt = ../native/bin/ocamlopt.opt
ext = ${sys_extension}
INCL = ${version.includes('4.06') ? '4.06.1+BS' : '4.02.3+BS'}
include body.ninja
`
  fs.writeFileSync(path.join(root,'lib',prebuilt),content,'ascii')
	child_process.execSync(`ninja -C lib -f ${prebuilt} -t clean && ninja -C lib -f ${prebuilt}`,root_config)
}
if(!is_windows){
  require('./ninja.js').updateRelease()
}

buildCompiler()

