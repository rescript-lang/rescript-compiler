//@ts-check
var os = require('os')
var os_type = os.type()

var is_windows = !(os_type.indexOf('Windows') < 0)
var is_bsd = !(os_type.indexOf('BSD') < 0)

exports.is_windows = is_windows
var sys_extension;
switch (os.type()) {
    case 'Darwin':
    case 'FreeBSD':
    case 'Linux':
    case 'OpenBSD':
    case 'Windows_NT':
        sys_extension = "." + os.platform(); break;
    default: throw ("Not supported " + os.type())
}

exports.sys_extension  = sys_extension
var make = is_bsd ? 'gmake' : 'make'
exports.make  = make


