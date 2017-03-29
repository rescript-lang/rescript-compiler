'use strict';

var Sys = require("../../lib/js/sys.js");

var is_windows_or_cygwin = Sys.win32 || Sys.cygwin;

exports.is_windows_or_cygwin = is_windows_or_cygwin;
/* No side effect */
