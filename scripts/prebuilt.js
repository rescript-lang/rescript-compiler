//@ts-check
var cp = require('child_process')
var path = require('path')
var {sys_extension, is_windows} = require('./config.js')

var root = path.join(__dirname, '..')
var root_config = { cwd: root, stdio: [0, 1, 2] }
process.env.BS_RELEASE_BUILD = 'true'


var version = require('./buildocaml.js').getVersionPrefix()
var fs = require('fs')
var hostPlatform = 'darwin'


function buildCompiler() {
  delete process.env.OCAMLLIB
  var prebuilt = 'prebuilt.ninja'
  var content = require('./ninjaFactory.js').libNinja({
    ocamlopt : is_windows?`ocamlopt.opt.exe`:`../native/${version}/bin/ocamlopt.opt`,
    ext : sys_extension,
    INCL : version
  })
  
  fs.writeFileSync(path.join(root,'lib',prebuilt),content,'ascii')
	cp.execSync(`ninja -C lib -f ${prebuilt} -t clean && ninja -C lib -f ${prebuilt}`,root_config)
}
if(!is_windows){
  require('./ninja.js').updateRelease()
}
var os = require('os')
function createOCamlTar(){
  if(os.platform ()=== hostPlatform){
    cp.execSync(`git -C ocaml status -uno`, { cwd: root, stdio: [0, 1, 2] })    
    cp.execSync(`git  -C ocaml archive --format=tar.gz HEAD -o ../ocaml.tar.gz`,
      { cwd: root, stdio: [0, 1, 2] }
    )
    fs.copyFileSync(path.join(root,'ocaml','VERSION'),path.join(root,'OCAML_VERSION'))
  }  
}
createOCamlTar()
buildCompiler()

// 
