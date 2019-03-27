//@ts-check
var cp = require('child_process')
var path = require('path')
var fs = require('fs')





// FIXME: this works in CI, but for release build, submodule
// is carried, so it needs to be fixed
/**
 * @returns{string}
 */
function getVersionPrefix(){
    var version = fs.readFileSync(path.join(__dirname, '..', 'ocaml', 'VERSION'), 'ascii')
    return version.substr(0, version.indexOf('+'))
}
exports.getVersionPrefix = getVersionPrefix


function build() {
    var prefix = path.normalize(path.join(__dirname,'..','native',getVersionPrefix()))
    cp.execSync('./configure -prefix ' + prefix + ' -no-ocamlbuild  -no-curses -no-graph -no-pthread -no-debugger  && make clean && make -j9 world.opt && make install '
        , { cwd: path.join(__dirname, '..', 'ocaml'), stdio: [0, 1, 2] })
}

exports.build = build
if(require.main === module){
    build()
}
