//@ts-check
var fs = require('fs')
var path = require('path')

/**
 * @returns{string}
 */
function getVersionPrefix(){
    var version = fs.readFileSync(path.join(__dirname, '..', 'vendor', 'ocaml', 'VERSION'), 'ascii')
    return version.substr(0, version.indexOf('+'))
}
exports.getVersionPrefix = getVersionPrefix