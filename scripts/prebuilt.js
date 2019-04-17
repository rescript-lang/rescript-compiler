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
  var content = `
ocamlopt = ${is_windows?`ocamlopt.opt.exe`:`../native/${version}/bin/ocamlopt.opt`}
ext = ${sys_extension}
INCL = ${version}
flags = -I $INCL -g -w -a ../jscomp/stubs/ext_basic_hash_stubs.c
rule cc
    command = $ocamlopt $flags $in -o $out
    description = Making $out
# -inline 1000 makes size too large
# TODO: make sure it can be bootstrapped, at least is a very good
# test case of our optimizations
# build bsdep.exe: cc bsdep.mli bsdep.ml
build bsppx$ext: cc $INCL/bsppx.mli $INCL/bsppx.ml
# build bspp.exe:  cc bspp.mli bspp.ml
build bsb$ext:  cc $INCL/bsb.mli $INCL/bsb.ml
    flags = $flags unix.cmxa str.cmxa
build bsb_helper$ext:  cc $INCL/bsb_helper.mli $INCL/bsb_helper.ml
    flags = $flags unix.cmxa -w -a
build refmt$ext: cc $INCL/refmt_main3.mli $INCL/refmt_main3.ml
    flags = $flags -w -40-30 -no-alias-deps -I +compiler-libs ocamlcommon.cmxa 
build reactjs_jsx_ppx_2$ext: cc $INCL/reactjs_jsx_ppx_v2.mli $INCL/reactjs_jsx_ppx_v2.ml
    flags = $flags -w -40-30 -no-alias-deps -I +compiler-libs ocamlcommon.cmxa
build reactjs_jsx_ppx_3$ext: cc $INCL/reactjs_jsx_ppx_v3.mli $INCL/reactjs_jsx_ppx_v3.ml
    flags = $flags -w -40-30 -no-alias-deps -I +compiler-libs ocamlcommon.cmxa
build bsc$ext: cc $INCL/whole_compiler.mli $INCL/whole_compiler.ml

`
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
