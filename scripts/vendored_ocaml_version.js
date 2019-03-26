//@ts-check
var fs = require('fs')
var path = require('path')


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