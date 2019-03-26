
var cp = require('child_process')
var path = require('path')
var prefix = path.normalize(path.join(__dirname,'..','native'))

function build() {
    cp.execSync('./configure -prefix ' + prefix + ' -no-ocamlbuild  -no-curses -no-graph -no-pthread -no-debugger  && make clean && make -j9 world.opt && make install '
        , { cwd: path.join(__dirname, '..', 'ocaml'), stdio: [0, 1, 2] })
}

exports.build = build
if(require.main === module){
    build()
}
