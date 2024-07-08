'use strict';

let Caml_sys = require("./caml_sys.js");
let Caml_exceptions = require("./caml_exceptions.js");

let match = Caml_sys.sys_get_argv();

let os_type = Caml_sys.os_type();

let backend_type = {
  TAG: "Other",
  _0: "BS"
};

let big_endian = false;

let unix = Caml_sys.os_type() === "Unix";

let win32 = Caml_sys.os_type() === "Win32";

function getenv_opt(s) {
  let x = typeof process === "undefined" ? undefined : process;
  if (x !== undefined) {
    return x.env[s];
  }
  
}

function command(param) {
  return 127;
}

let interactive = {
  contents: false
};

function signal(param, param$1) {
  return "Signal_default";
}

function set_signal(sig_num, sig_beh) {
  
}

let Break = /* @__PURE__ */Caml_exceptions.create("Sys.Break");

function catch_break(on) {
  if (on) {
    return set_signal(-6, {
      TAG: "Signal_handle",
      _0: (function (param) {
        throw new Error(Break, {
              cause: {
                RE_EXN_ID: Break
              }
            });
      })
    });
  } else {
    return set_signal(-6, "Signal_default");
  }
}

function enable_runtime_warnings(param) {
  
}

function runtime_warnings_enabled() {
  return false;
}

let argv = match[1];

let executable_name = match[0];

let cygwin = false;

let word_size = 32;

let int_size = 32;

let max_string_length = 2147483647;

let max_array_length = 2147483647;

let sigabrt = -1;

let sigalrm = -2;

let sigfpe = -3;

let sighup = -4;

let sigill = -5;

let sigint = -6;

let sigkill = -7;

let sigpipe = -8;

let sigquit = -9;

let sigsegv = -10;

let sigterm = -11;

let sigusr1 = -12;

let sigusr2 = -13;

let sigchld = -14;

let sigcont = -15;

let sigstop = -16;

let sigtstp = -17;

let sigttin = -18;

let sigttou = -19;

let sigvtalrm = -20;

let sigprof = -21;

let sigbus = -22;

let sigpoll = -23;

let sigsys = -24;

let sigtrap = -25;

let sigurg = -26;

let sigxcpu = -27;

let sigxfsz = -28;

let ocaml_version = "4.06.2+BS";

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
