
var cp = require('child_process')
var path = require('path')
var prefix = path.normalize(path.join(__dirname,'..','native'))

function build() {
    cp.execSync('./configure -prefix ' + prefix + ' -no-ocamldoc -no-ocamlbuild -no-shared-libs -no-curses -no-graph -no-pthread -no-debugger  && make -j9 world.opt && make install '
        , { cwd: path.join(__dirname, '..', 'vendor', 'ocaml'), stdio: [0, 1, 2] })
}

exports.build = build
if(require.main === module){
    build()
}
