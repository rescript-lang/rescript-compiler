'use strict';

var Caml_sys = require("./caml_sys.js");
var Caml_exceptions = require("./caml_exceptions.js");

var match = Caml_sys.sys_get_argv();

var os_type = Caml_sys.os_type();

var backend_type = {
  TAG: "Other",
  _0: "BS"
};

var big_endian = false;

var unix = Caml_sys.os_type() === "Unix";

var win32 = Caml_sys.os_type() === "Win32";

function getenv_opt(s) {
  var x = typeof process === "undefined" ? undefined : process;
  if (x !== undefined) {
    return x.env[s];
  }
  
}

function command(param) {
  return 127;
}

var interactive = {
  contents: false
};

function signal(param, param$1) {
  return "Signal_default";
}

function set_signal(sig_num, sig_beh) {
  
}

var Break = /* @__PURE__ */Caml_exceptions.create("Sys.Break");

function catch_break(on) {
  
}

function enable_runtime_warnings(param) {
  
}

function runtime_warnings_enabled(param) {
  return false;
}

var argv = match[1];

var executable_name = match[0];

var cygwin = false;

var word_size = 32;

var int_size = 32;

var max_string_length = 2147483647;

var max_array_length = 2147483647;

var sigabrt = -1;

var sigalrm = -2;

var sigfpe = -3;

var sighup = -4;

var sigill = -5;

var sigint = -6;

var sigkill = -7;

var sigpipe = -8;

var sigquit = -9;

var sigsegv = -10;

var sigterm = -11;

var sigusr1 = -12;

var sigusr2 = -13;

var sigchld = -14;

var sigcont = -15;

var sigstop = -16;

var sigtstp = -17;

var sigttin = -18;

var sigttou = -19;

var sigvtalrm = -20;

var sigprof = -21;

var sigbus = -22;

var sigpoll = -23;

var sigsys = -24;

var sigtrap = -25;

var sigurg = -26;

var sigxcpu = -27;

var sigxfsz = -28;

var ocaml_version = "4.06.2+BS";

exports.argv = argv;
exports.executable_name = executable_name;
exports.getenv_opt = getenv_opt;
exports.command = command;
exports.interactive = interactive;
exports.os_type = os_type;
exports.backend_type = backend_type;
exports.unix = unix;
exports.win32 = win32;
exports.cygwin = cygwin;
exports.word_size = word_size;
exports.int_size = int_size;
exports.big_endian = big_endian;
exports.max_string_length = max_string_length;
exports.max_array_length = max_array_length;
exports.signal = signal;
exports.set_signal = set_signal;
exports.sigabrt = sigabrt;
exports.sigalrm = sigalrm;
exports.sigfpe = sigfpe;
exports.sighup = sighup;
exports.sigill = sigill;
exports.sigint = sigint;
exports.sigkill = sigkill;
exports.sigpipe = sigpipe;
exports.sigquit = sigquit;
exports.sigsegv = sigsegv;
exports.sigterm = sigterm;
exports.sigusr1 = sigusr1;
exports.sigusr2 = sigusr2;
exports.sigchld = sigchld;
exports.sigcont = sigcont;
exports.sigstop = sigstop;
exports.sigtstp = sigtstp;
exports.sigttin = sigttin;
exports.sigttou = sigttou;
exports.sigvtalrm = sigvtalrm;
exports.sigprof = sigprof;
exports.sigbus = sigbus;
exports.sigpoll = sigpoll;
exports.sigsys = sigsys;
exports.sigtrap = sigtrap;
exports.sigurg = sigurg;
exports.sigxcpu = sigxcpu;
exports.sigxfsz = sigxfsz;
exports.Break = Break;
exports.catch_break = catch_break;
exports.ocaml_version = ocaml_version;
exports.enable_runtime_warnings = enable_runtime_warnings;
exports.runtime_warnings_enabled = runtime_warnings_enabled;
/* No side effect */
